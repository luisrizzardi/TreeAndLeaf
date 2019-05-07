#' Initial layout creation for the TreeAndLeaf.
#'
#' Returns a matrix of positions to be relaxed by the force based algorithm
#' implemented in the RedeR package.
#'
#' @param rdp An object of RedPort class, from RedeR package <RedPort>.
#' @param gg An hclust or igraph object, containing the dendrogram to
#' be reorganized <igraph><hclust>.
#' @param size The size of the dendrogram. "small" = less
#' than 100 nodes; "medium" = less than 500 nodes; "large" = 500+ nodes.
#' This is just a reference point, use the size that gives the best
#' results (default = "small") <string>.
#' @param showgraph Toggles the addition of the resulting layout to the
#' RedeR app and the relaxation process (default = TRUE) <logical>.
#' @return A matrix of the nodes positions.
#'
#' @seealso \code{\link{formatTree}}
#' @seealso \code{\link[RedeR:addGraph]{addGraph}}
#' @seealso \code{\link[RedeR:relax]{relax}}
#'
#' @examples
#' #For showgraph = FALSE, get the resulting layout using:
#' hc <- hclust(dist(USArrests), "ave")
#' layout <- treeAndLeaf(hc, showgraph = FALSE)
#'
#' #For showgraph = TRUE, plot the tree using:
#' rdp <- RedeR::RedPort()
#' RedeR::calld(rdp)
#' hc <- hclust(dist(USArrests), "ave")
#' treeAndLeaf(hc)
#' @export


treeAndLeaf <- function(rdp, gg, size = "small", showgraph = TRUE){
  if(class(gg)=="hclust"){
    gg <- hclust2igraph(gg)
  }
  #-- Find root and get number of leafs
  edgelist <- igraph::get.edgelist(gg)
  root <- .findRoot(edgelist)

  #-- Start layout
  layout <- matrix(0, nrow = igraph::vcount(gg), ncol = 2,
                   dimnames = list(igraph::V(gg)$name, c("x","y")))

  #-- Calculate the edges lengths for root
  elL <- .findSubTreeSizeLeft(root, edgelist)^2
  elR <- .findSubTreeSizeRight(root, edgelist)^2

  #-- Find the root's children from edgelist
  children <- edgelist[which(edgelist[,1] %in% root),2]

  #-- Set the layout
  layout[children[1],] <- c(elR, 0)
  layout[children[2],] <- c(-elL, 0)

  count <<- 0

  #-- Recursively set the layout for the rest of the binary tree
  layout <- .setLayout(children[1], edgelist, layout, size = size)
  layout <- .setLayout(children[2], edgelist, layout, size = size)

  if(showgraph == TRUE){
    RedeR::.rederpost(rdp, 'RedHandler.stopPaint')
    switch(size,
           small = RedeR::addGraph(rdp, gg, layout = layout, zoom = 15),
           medium = RedeR::addGraph(rdp, gg, layout = layout, zoom = 8),
           large = RedeR::addGraph(rdp, gg, layout = layout, zoom = 3))
    switch(size,
           small = RedeR::relax(rdp, p1 = 50, p8 = 40, ps = TRUE),
           medium = RedeR::relax(rdp, p1 = 80, p8 = 60, ps = TRUE),
           large = RedeR::relax(rdp, p1 = 100, p8 = 80, ps = TRUE))

    seconds <- ceiling(length(igraph::V(gg)$name)/100)+2
    cat("Please wait... Your tree will be available in", seconds, "seconds.")
    Sys.sleep(seconds)
    RedeR::.rederpost(rdp, 'RedHandler.startPaint')
  }
  return(invisible(layout))
}

.setLayout <- function(node, edgelist, layout, size = "small"){
    if(node %in% edgelist[,1]){
      #-- Counter to alternate between directions (should find a better way)
      count <<- count + 1

      #-- Find children
      children <- edgelist[which(edgelist[,1] %in% node),2]

      #-- Calculate edges
      if(size == "small"){
        elL <- (.findSubTreeSizeLeft(node, edgelist)*10)
        elR <- (.findSubTreeSizeRight(node, edgelist)*10)
      }
      if(size == "medium"){
        elL <- (.findSubTreeSizeLeft(node, edgelist)*10 + .countChildren(node, edgelist)*5)
        elR <- (.findSubTreeSizeRight(node, edgelist)*10 + .countChildren(node, edgelist)*5)
      }
      if(size == "large"){
        elL <- (.findSubTreeSizeLeft(node, edgelist)*10 + .countChildren(node, edgelist)*10)
        elR <- (.findSubTreeSizeRight(node, edgelist)*10 + .countChildren(node, edgelist)*10)
      }

      #-- Alternates between up/down and left/right
      if(count %% 3 == 0){
        coord.child1 <- c(layout[node, 1], layout[node, 2] + elR)
        coord.child2 <- c(layout[node, 1], layout[node, 2] - elL)
      }
      if(count %% 3 == 1){
        coord.child1 <- c(layout[node, 1] + elR, layout[node, 2])
        coord.child2 <- c(layout[node, 1] - elL, layout[node, 2])
      }
      if(count %% 3 == 2){
        coord.child1 <- c(layout[node, 1] + 0.5*elR, layout[node, 2]+ 0.5*elR)
        coord.child2 <- c(layout[node, 1] - 0.5*elL, layout[node, 2]- 0.5*elL)
      }

      #-- Set the layout
      layout[children[1],] <- coord.child1
      layout[children[2],] <- coord.child2


      #-- Recursive call
      layout <- .setLayout(children[1], edgelist, layout, size)
      layout <- .setLayout(children[2], edgelist, layout, size)
    }
    else{
      return(layout)
    }
}

.findSubTreeSizeLeft <- function(node, edgelist, length = 0){
  children <- edgelist[which(edgelist[,1] %in% node),2]
  if(node %in% edgelist[,1]){
    length <- length + 1
    length <- .findSubTreeSize(children[2], edgelist, length)
  }
  return(length)
}

.findSubTreeSizeRight <- function(node, edgelist, length = 0){
  children <- edgelist[which(edgelist[,1] %in% node),2]
  if(node %in% edgelist[,1]){
    length <- length + 1
    length <- .findSubTreeSize(children[1], edgelist, length)
  }
  return(length)
}

.findSubTreeSize <- function(node, edgelist, length = 0){
  children <- edgelist[which(edgelist[,1] %in% node),2]
  if(node %in% edgelist[,1]){
    length <- length + 1
    left <- .findSubTreeSize(children[1], edgelist, length)
    right <- .findSubTreeSize(children[2], edgelist, length)
    if(left > length || right > length){
      if(left > right){
        return(left)
      }
      else{
        return(right)
      }
    }
    return(length)
  }
  return(length)
}

.findRoot <- function(edgelist){
  return(unique(edgelist[which( is.na(match(edgelist[,1], edgelist[,2])) == TRUE)]))
}

.countChildren <- function(node, edgelist, count = -1){
  children <- edgelist[which(edgelist[,1] %in% node),2]
  if(node %in% edgelist[,1]){
    count <- .countChildren(children[1], edgelist, count)
    count <- .countChildren(children[2], edgelist, count)
  }
  return(count+1)
}

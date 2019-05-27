#' Normalization of the igraph node sizes
#'
#' Returns the given igraph object with the node sizes normalized to the
#' range indicated by the user. If you also want to normalize the legend scales,
#' it is necessary to use this function after aplying \code{\link[TreeAndLeaf]{formatTree}} or 
#' \code{\link[RedeR]{att.setv}}
#'
#' @param gg An igraph object with a nodeSize component set <igraph>.
#' @param maxt The target upper limit for the size normalization <numeric>.
#' @param mint The target bottom limit for the size normalization <numeric>.
#'
#' @return an igraph object with its nodeSize and legNodeSize components normalized
#'
#' @seealso \code{\link[TreeAndLeaf]{treeAndLeaf}}
#' @seealso \code{\link[RedeR]{addGraph}}
#' @seealso \code{\link[igraph]{V}}
#'
#' @examples
#' data(gg)
#' gg <- gg$g
#' gg <- normggSize (gg, 150, 50)
#'
#' @export

normggSize <- function (gg, mint, maxt) {

    if(!is.null(gg$legNodeSize["scale"])){
        max <- max(gg$legNodeSize[["scale"]])
        min <- min(gg$legNodeSize[["scale"]])
        range1 <- max - min
        range2 <- maxt - mint
        gg$legNodeSize[["scale"]] <- (gg$legNodeSize[["scale"]]- min)/range1
        gg$legNodeSize[["scale"]] <- (gg$legNodeSize[["scale"]] * range2) + mint
    }
    

    max <- max(igraph::vertex_attr(gg, "nodeSize"))
  
    min <- min(igraph::vertex_attr(gg, "nodeSize"))
    
    range1 <- max - min
    print(range1)
    idx <- vector(length = length(V(gg)$name))
    for(i in 1:(length(V(gg)$name)/2 - 0.5)){
      idx[match(paste0("N", i), V(gg)$name)] <- TRUE
    }
    
  
    igraph::vertex_attr(gg, "nodeSize")[!idx] <- (igraph::vertex_attr(gg, "nodeSize")[!idx] - min)/range1
    igraph::vertex_attr(gg, "nodeSize")[idx] <- 1
    
    range2 <- maxt - mint

    igraph::vertex_attr(gg, "nodeSize")[!idx] <- (igraph::vertex_attr(gg, "nodeSize")[!idx] * range2) + mint

    return(gg)
}

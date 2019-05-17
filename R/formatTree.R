#' Formatting of an igraph to be shown in RedeR
#'
#' Applies formatting patterns to an igraph object according to predefined themes
#' and/or user choices. This formatting is used for plotting on the RedeR app interface.
#' You may choose among using the original themes, personalizing just some of the parameters
#' or setting all of them.
#'
#' @param gg An igraph object <igraph>.
#' @param dataframe A dataframe containing at least rownames set to be the same
#' used to create the igraph, a column with the data to be shown as node sizes
#' and a column with the data to create the color gradient <dataframe>.
#' @param theme Selects the predefined theme to be the base of the igraph formatting.
#' The first character represents the size of the tree - S for small, M for medium and L for large.
#' See \code{\link{treeAndLeaf}} for more information about tree sizes.
#' The second character represents the type of color palette - S for sequential, D for divergent.
#' The remaining characters represent the color palette - Greens and Reds for sequential palettes
#' and PuOr for a Purple/Orange divergent palette. Options are: SSGreens, SSReds, SDPuOr,
#' MSGreens, MSReds, MDPuOr, LSGreens, LSReds, LDPuOr (default = "SSGreens") <string>.
#' @param cleanalias A logical that removes the node aliases when set to TRUE (default = FALSE) <logical>.
#' @param nodeSize The name of the column for size differentiation (must be a column of given "dataframe") <string>.
#' @param nszleg Number of size legend components <numeric>.
#' @param nodeColor The name of the column color differentiation (must be a column of given "dataframe") <string>.
#' @param palqt How many color quantiles the data will be divided into <numeric>.
#' @param colpal Which RColorBrewer palette will be used <string>.
#' @param nodeFontSize The alias font size <numeric>.
#' @param nodeFontColor The alias font color <string>.
#' @param edgeWidth The edge width <numeric>.
#' @param edgeColor The edge color <numeric>.
#'
#' @return An igraph object with standard formatting for RedeR application.
#'
#' @seealso \code{\link[RedeR]{addGraph}}
#' @seealso \code{\link{treeAndLeaf}}
#'
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' gg <- hclust2igraph(hc)
#' gg <- formatTree(gg = gg,
#'                  dataframe = USArrests,
#'                  theme = "SSReds",
#'                  nodeSize = "UrbanPop",
#'                  nodeColor = "Murder")
#'
#' @export

formatTree <- function(gg, dataframe, ..., theme = "SSGreens", cleanalias = FALSE){
    
    #Initializing the list of received parameters
    param <- list(...)
    
    #If no parameters were passed, this allows the application of defaults
    if(is.null(param)){
      param <- NULL
    }
    
    #Inserting the dataframe inside the igraph object
    dataframe$idfornodes <- rownames(dataframe)
    gg <- RedeR::att.mapv(gg, dataframe, refcol=ncol(dataframe))
    
    #Setting the node alias
    idx <- match(igraph::V(gg)$name, rownames(dataframe))
    igraph::V(gg)$nodeAlias <- igraph::V(gg)$name
    igraph::V(gg)$nodeAlias[is.na(idx)]<-""

    #Setting the node font size
    if (!is.null(param$nodeFontSize)) {
        igraph::V(gg)$nodeFontSize<-param$nodeFontSize
    } else {
        switch (theme,
                SSGreens = igraph::V(gg)$nodeFontSize <- 45,
                SSReds = igraph::V(gg)$nodeFontSize <- 45,
                SDPuOr = igraph::V(gg)$nodeFontSize <- 45,
                MSReds = igraph::V(gg)$nodeFontSize <- 30,
                MSGreens = igraph::V(gg)$nodeFontSize <- 30,
                MDPuOr = igraph::V(gg)$nodeFontSize <- 30,
                LSReds = igraph::V(gg)$nodeFontSize <- 15,
                LSGreens = igraph::V(gg)$nodeFontSize <- 15,
                LDPuOr = igraph::V(gg)$nodeFontSize <- 15)
                
    }

    #Setting the node font color
    if (!is.null(param$nodeFontColor)) {
        igraph::V(gg)$nodeFontColor <- param$nodeFontColor
    } else {
        switch (theme,
                SSGreens = igraph::V(gg)$nodeFontColor <- "green",
                SSReds = igraph::V(gg)$nodeFontColor <- "red",
                SDPuOr = igraph::V(gg)$nodeFontColor <- "black",
                MSReds = igraph::V(gg)$nodeFontColor <- "red",
                MSGreens = igraph::V(gg)$nodeFontColor <- "green",
                MDPuOr = igraph::V(gg)$nodeFontColor <- "black",
                LSReds = igraph::V(gg)$nodeFontColor <- "red",
                LSGreens = igraph::V(gg)$nodeFontColor <- "green",
                LDPuOr = igraph::V(gg)$nodeFontColor <- "black")
                 #VERIFY QUALITY OF THE STANDARD / ADD MORE OPTIONS
    }

    #Setting the node size
    if (is.null(param$nszleg)){
      param$nszleg <- 5
    }
    if (!is.null(param$nodeSize)) {
        minaux <- paste0(deparse(quote(dataframe)),"$",param$nodeSize)
        minsz <- as.numeric(min(eval(parse(text=minaux))))
        maxaux <- paste0(deparse(quote(dataframe)),"$",param$nodeSize)
        maxsz <- as.numeric(max(eval(parse(text=maxaux))))
        gg <- RedeR::att.setv(gg, param$nodeSize, to="nodeSize", xlim = c(minsz,maxsz,1), nquant = param$nszleg)
    } else {
        switch (theme,
                SSGreens = igraph::V(gg)$nodeSize <- 100,
                SSReds = igraph::V(gg)$nodeSize <- 100,
                SDPuOr = igraph::V(gg)$nodeSize <- 100,
                MSReds = igraph::V(gg)$nodeSize <- 90,
                MSGreens = igraph::V(gg)$nodeSize <- 90,
                MDPuOr = igraph::V(gg)$nodeSize <- 90,
                LSReds = igraph::V(gg)$nodeSize <- 80,
                LSGreens = igraph::V(gg)$nodeSize <- 80,
                LDPuOr = igraph::V(gg)$nodeSize <- 80)
      
        igraph::V(gg)$nodeSize[igraph::V(gg)$nodeAlias==""] <- 1
    }

    #Setting the edge width
    if (!is.null(param$edgeWidth)) {
        igraph::E(gg)$edgeWidth <- param$edgeWidth
    } else {
        switch (theme,
                SSGreens = igraph::E(gg)$edgeWidth <- 15,
                SSReds = igraph::E(gg)$edgeWidth <- 15,
                SDPuOr = igraph::E(gg)$edgeWidth <- 15,
                MSReds = igraph::E(gg)$edgeWidth <- 12,
                MSGreens = igraph::E(gg)$edgeWidth <- 12,
                MDPuOr = igraph::E(gg)$edgeWidth <- 12,
                LSReds = igraph::E(gg)$edgeWidth <- 10,
                LSGreens = igraph::E(gg)$edgeWidth <- 10,
                LDPuOr = igraph::E(gg)$edgeWidth <- 10)
    }
    #Setting the edge color
    if (!is.null(param$edgeColor)) {
        igraph::E(gg)$edgeColor <- param$edgeColor
    } else {
        switch (theme,
                SSGreens = igraph::E(gg)$edgeColor <- "black",
                SSReds = igraph::E(gg)$edgeColor <- "black",
                SDPuOr = igraph::E(gg)$edgeColor <- "black",
                MSReds = igraph::E(gg)$edgeColor <- "black",
                MSGreens = igraph::E(gg)$edgeColor <- "black",
                MDPuOr = igraph::E(gg)$edgeColor <- "black",
                LSReds = igraph::E(gg)$edgeColor <- "black",
                LSGreens = igraph::E(gg)$edgeColor <- "black",
                LDPuOr = igraph::E(gg)$edgeColor <- "black")
    }

    #Setting the node colors
    if (is.null(param$palqt)){
      param$palqt <- 5
    }
    if (!is.null(param$nodeColor)) {
            if (!is.null(param$colpal)) {
               pal.aux <- RColorBrewer::brewer.pal(9, param$colpal)
               pal <- grDevices::colorRampPalette(pal.aux)(param$palqt)
            } else {
                switch (theme,
                        SSGreens = pal.aux <- RColorBrewer::brewer.pal(9, "Greens"),
                        SSReds = pal.aux <- RColorBrewer::brewer.pal(9, "Reds"),
                        SDPuOr = pal.aux <- RColorBrewer::brewer.pal(9, "PuOr"),
                        MSReds = pal.aux <- RColorBrewer::brewer.pal(9, "Reds"),
                        MSGreens = pal.aux <- RColorBrewer::brewer.pal(9, "Greens"),
                        MDPuOr = pal.aux <- RColorBrewer::brewer.pal(9, "PuOr"),
                        LSReds = pal.aux <- RColorBrewer::brewer.pal(9, "Reds"),
                        LSGreens = pal.aux <- RColorBrewer::brewer.pal(9, "Greens"),
                        LDPuOr = pal.aux <- RColorBrewer::brewer.pal(9, "PuOr"))
                pal <- grDevices::colorRampPalette(pal.aux)(param$palqt)
            }
      gg <- RedeR::att.setv(gg, param$nodeColor, to = "nodeColor", cols = pal, nquant = param$palqt, na.col = "black")
    } else {
      switch (theme,
              SSGreens = igraph::V(gg)$nodeColor <- "green",
              SSReds = igraph::V(gg)$nodeColor <- "red",
              SDPuOr = igraph::V(gg)$nodeColor <- "purple",
              MSReds = igraph::V(gg)$nodeColor <- "red",
              MSGreens = igraph::V(gg)$nodeColor <- "green",
              MDPuOr = igraph::V(gg)$nodeColor <- "purple",
              LSReds = igraph::V(gg)$nodeColor <- "red",
              LSGreens = igraph::V(gg)$nodeColor <- "green",
              LDPuOr = igraph::V(gg)$nodeColor <- "purple")
        }
    
    #Cleaning the node aliases
    if (cleanalias == TRUE){
        igraph::V(gg)$nodeAlias <-""
    }

    return(invisible(gg))
}

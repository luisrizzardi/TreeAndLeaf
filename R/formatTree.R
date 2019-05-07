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
#' @param szdata The data for size differentiation (must be a column of given "dataframe") <numeric vector>.
#' @param coldata The data for color differentiation (must be a column of given "dataframe") <numeric vector>.
#' @param palqt How many color quantiles the data will be divided into <numeric>.
#' @param colpal Which RColorBrewer palette will be used <string>.
#' @param fsize The alias font size <numeric>.
#' @param fcolor The alias font color <string>.
#' @param edgewd The edge width <numeric>.
#' @param edgecol The edge color <numeric>.
#' @param nszleg Number of size legend components <numeric>.
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
#'                  szdata = USArrests$UrbanPop,
#'                  coldata = USArrests$Murder)
#'
#' @export

formatTree <- function(gg, dataframe, ..., theme = "SSGreens", cleanalias = FALSE){
    param <- list(...)

    idx <- match(igraph::V(gg)$name, rownames(dataframe))
    igraph::V(gg)$nodeAlias <- igraph::V(gg)$name
    igraph::V(gg)$nodeAlias[is.na(idx)]<-""

    # NODE FONT SIZE
    if (!is.null(param$fsize)) {
        igraph::V(gg)$nodeFontSize<-param$fsize
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
                #VERIFY QUALITY OF THE STANDARD / ADD MORE OPTIONS
    }

    #NODE FONT COLOR
    if (!is.null(param$fcolor)) {
        igraph::V(gg)$nodeFontColor <- param$fcolor
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

    #MAKE CONNECTING NODES "INVISIBLE"
    igraph::V(gg)$nodeFontSize[igraph::V(gg)$nodeAlias==""]<- 1
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

    #NODE SIZE SETUP
    if (!is.null(param$szdata)) {
        sz <- param$szdata
        names(sz) <- rownames(dataframe)
        idx <- match(names(sz), igraph::V(gg)$name)
        suppressWarnings(igraph::V(gg)$nodeSize[idx] <- sz)
        igraph::V(gg)$nodeSize[igraph::V(gg)$nodeAlias==""] <- 1
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
                 #VERIFY QUALITY OF THE STANDARD / ADD MORE OPTIONS
        igraph::V(gg)$nodeSize[igraph::V(gg)$nodeAlias==""] <- 1
    }

    # EDGE STYLE SETUP

    if (!is.null(param$edgewd)) {
        igraph::E(gg)$edgeWidth <- param$edgewd
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
        #VERIFY QUALITY OF THE STANDARD / ADD MORE OPTIONS
    }

    if (!is.null(param$edgecol)) {
        igraph::E(gg)$edgeColor <- param$edgecol
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
                #VERIFY QUALITY OF THE STANDARD / ADD MORE OPTIONS
    }


    #NODE COLORS SETUP

    if (!is.null(param$coldata)) {
        ndcolor <- param$coldata
        names(ndcolor) <- rownames(dataframe)
        if (!is.null(param$palqt)) {
            qnts <- quantile(ndcolor, seq(0,1,(1/param$palqt)))
            if (!is.null(param$colpal)) {
                pal <- RColorBrewer::brewer.pal(param$palqt, param$colpal)
            } else {
                switch (theme,
                        SSGreens = pal <- RColorBrewer::brewer.pal(param$palqt, "Greens"),
                        SSReds = pal <- RColorBrewer::brewer.pal(param$palqt, "Reds"),
                        SDPuOr = pal <- RColorBrewer::brewer.pal(param$palqt, "PuOr"),
                        MSReds = pal <- RColorBrewer::brewer.pal(param$palqt, "Reds"),
                        MSGreens = pal <- RColorBrewer::brewer.pal(param$palqt, "Greens"),
                        MDPuOr = pal <- RColorBrewer::brewer.pal(param$palqt, "PuOr"),
                        LSReds = pal <- RColorBrewer::brewer.pal(param$palqt, "Reds"),
                        LSGreens = pal <- RColorBrewer::brewer.pal(param$palqt, "Greens"),
                        LDPuOr = pal <- RColorBrewer::brewer.pal(param$palqt, "PuOr"))
                        #VERIFY QUALITY OF THE STANDARD / ADD MORE OPTIONS
            }
        } else {
            qnts <- quantile(ndcolor, seq(0,1,0.2))
            if (!is.null(param$colpal)) {
                pal <- RColorBrewer::brewer.pal(5, param$colpal) #VERIFY QUALITY OF THE STANDARD / ADD MORE OPTIONS
            } else {
                switch (theme,
                        SSGreens = pal <- RColorBrewer::brewer.pal(5, "Greens"),
                        SSReds = pal <- RColorBrewer::brewer.pal(5, "Reds"),
                        SDPuOr = pal <- RColorBrewer::brewer.pal(5, "PuOr"),
                        MSReds = pal <- RColorBrewer::brewer.pal(5, "Reds"),
                        MSGreens = pal <- RColorBrewer::brewer.pal(5, "Greens"),
                        MDPuOr = pal <- RColorBrewer::brewer.pal(5, "PuOr"),
                        LSReds = pal <- RColorBrewer::brewer.pal(5, "Reds"),
                        LSGreens = pal <- RColorBrewer::brewer.pal(5, "Greens"),
                        LDPuOr = pal <- RColorBrewer::brewer.pal(5, "PuOr"))
                 #VERIFY QUALITY OF THE STANDARD / ADD MORE OPTIONS
            }
        }
        col <- ndcolor
        for(i in 1:(length(qnts)-1)){
            if (i < (length(qnts)-1))
                col[ndcolor >= qnts[i] & ndcolor < qnts[i+1]] <- pal[i]
            if (i == (length(qnts)-1))
                col[ndcolor >= qnts[i] & ndcolor <= qnts[i+1]] <- pal[i]
        }
        idx <- match(names(col), igraph::V(gg)$nodeAlias)
        igraph::V(gg)$nodeColor[idx] <- col
    }
    if (cleanalias == TRUE){
        igraph::V(gg)$nodeAlias <-""
    }

  #Legend Creation
    qnts <- round(qnts)
    gg$col_leg <- NULL
    for(i in 2:(length(qnts)) - 1) {
      gg$col_leg[i] <- paste(qnts[i], " - ", qnts[i+1])
    }


    min <- signif(min(sz), digits = 0)
    max <- signif(max(sz), digits = 0)
    if (!is.null(param$nszleg)) {
      gg$sz_leg <- seq(min, max, length.out = param$nszleg)
    } else {
      gg$sz_leg <- seq(min, max, length.out = 4)
    }

    gg$pal <- pal

    return(invisible(gg))
}

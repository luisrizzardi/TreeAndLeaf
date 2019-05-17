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
#' gg <- normggSize (gg, 150, 50)
#'
#' @export

normggSize <- function (gg, mint, maxt) {

    if(!is.null(gg$legNodeSize$scale)){
        max <- max(gg$legNodeSize$scale)
        min <- min(gg$legNodeSize$scale)
        range1 <- max - min
        gg$legNodeSize$scale <- gg$legNodeSize$scale/range1
        range2 <- maxt - mint
        gg$legNodeSize$scale <- (gg$legNodeSize$scale * range2) + mint
    }
    
    max <- max(igraph::V(gg)$nodeSize)
    min <- sort(unique(igraph::V(gg)$nodeSize))[2]
    range1 <- max - min

    idx <- igraph::V(gg)$nodeSize != min(igraph::V(gg)$nodeSize)

    igraph::V(gg)$nodeSize[idx] <- (igraph::V(gg)$nodeSize[idx] - min)/range1
    igraph::V(gg)$nodeSize[is.nan(igraph::V(gg)$nodeSize)] <- 0
    range2 <- maxt - mint

    igraph::V(gg)$nodeSize[idx] <- (igraph::V(gg)$nodeSize[idx] * range2) + mint

    return(gg)
}

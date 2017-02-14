##' scatter pie plot
##'
##'
##' @title geom_scatterpie
##' @param mapping aes mapping
##' @param data data
##' @param cols cols the pie data
##' @param ... additional parameters
##' @importFrom ggforce geom_arc_bar
##' @importFrom utils modifyList
##' @importFrom tidyr gather_
##' @importFrom ggplot2 aes_
##' @export
##' @return layer
##' @examples
##' library(ggplot2)
##' d <- data.frame(x=rnorm(5), y=rnorm(5))
##' d$A <- abs(rnorm(5, sd=1))
##' d$B <- abs(rnorm(5, sd=2))
##' d$C <- abs(rnorm(5, sd=3))
##' ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=c("A", "B", "C")) + coord_fixed()
##' @author guangchuang yu
geom_scatterpie <- function(mapping=NULL, data, cols, ...) {
    if (is.null(mapping))
        mapping <- aes_(x=~x, y=~y)
    mapping <- modifyList(mapping, aes_(r0=0, fill=~type,
                                        amount=~value))

    if (!'r' %in% names(mapping)) {
        xvar <- as.character(mapping)["x"]
        size <- diff(range(data[, xvar]))/50
        mapping <- modifyList(mapping, aes_(r=size))
    }

    names(mapping)[match(c("x", "y"), names(mapping))] <- c("x0", "y0")

    df <- gather_(data, "type", "value", cols)
    ## df$type <- factor(df$type, levels=cols)
    geom_arc_bar(mapping, data=df, stat='pie', inherit.aes=FALSE, ...)
}



##' scatter pie plot
##'
##'
##' @title geom_scatterpie
##' @param mapping aes mapping
##' @param data data
##' @param cols cols the pie data
##' @param sorted_by_radius whether plotting large pie first
##' @param ... additional parameters
##' @importFrom ggforce geom_arc_bar
##' @importFrom utils modifyList
##' @importFrom tidyr gather
##' @importFrom rlang enquo
##' @importFrom rlang !!
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
geom_scatterpie <- function(mapping=NULL, data, cols, sorted_by_radius = FALSE, ...) {
    if (is.null(mapping))
        mapping <- aes_(x=~x, y=~y)
    mapping <- modifyList(mapping, aes_(r0=0, fill=~type,
                                        amount=~value))

    if (!'r' %in% names(mapping)) {
        xvar <- as.character(mapping$x)
        size <- diff(range(data[, xvar]))/50
        data$r <- size
        mapping <- modifyList(mapping, aes_(r=size))
    }

    names(mapping)[match(c("x", "y"), names(mapping))] <- c("x0", "y0")
    data <- data[rowSums(data[, cols]) > 0, ]

    ## df <- gather_(data, "type", "value", cols)
    cols2 <- enquo(cols)
    df <- gather(data, "type", "value", !!cols2)

    ## df$type <- factor(df$type, levels=cols)
    if (!sorted_by_radius) {
        return(geom_arc_bar(mapping, data=df, stat='pie', inherit.aes=FALSE, ...))
    }

    lapply(split(df, df$r)[as.character(sort(unique(df$r), decreasing=TRUE))], function(d) {
        geom_arc_bar(mapping, data=d, stat='pie', inherit.aes=FALSE, ...)
    })
}



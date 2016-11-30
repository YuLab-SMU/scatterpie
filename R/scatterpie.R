##' scatter pie plot
##'
##'
##' @title geom_scatterpie
##' @param mapping aes mapping
##' @param data data
##' @param cols cols the pie data
##' @param scale_fun scale function for pie size
##' @param ... additional parameters
##' @importFrom ggforce geom_arc_bar
##' @importFrom utils modifyList
##' @importFrom tidyr gather_
##' @importFrom ggplot2 aes_
##' @export
##' @return layer
##' @author guangchuang yu
geom_scatterpie <- function(mapping, data, cols, scale_fun=function(x) x/sum(x), ...) {
    mapping <- modifyList(mapping, aes_(r0=0, fill=~type,
                                        amount=~value))
    if (!'r' %in% names(mapping)) {
        data$size <- scale_fun(rowSums(data[, cols]))
        mapping <- modifyList(mapping, aes_(r=~size))
    }

    names(mapping)[match(c("x", "y"), names(mapping))] <- c("x0", "y0")

    df <- gather_(data, "type", "value", cols)
    geom_arc_bar(mapping, data=df, stat='pie', inherit.aes=FALSE, ...)
}

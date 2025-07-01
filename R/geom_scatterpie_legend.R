##' legend of scatterpie
##'
##'
##' @title geom_scatterpie_legend
##' @param radius radius vector
##' @param x x position
##' @param y y position
##' @param n number of circle
##' @param breaks A character vector of breaks, default is NULL.
##' @param labeller function to label radius
##' @param label_position a character string indicating the position of labels,
##'   "right" (default) or "left" or any abbreviation of these
##' @param ... other text arguments passed on to \code{\link[ggplot2:layer]{ggplot2::layer()}}
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 geom_text
##' @importFrom rlang .data
##' @export
##' @return layer
##' @author Guangchuang Yu
geom_scatterpie_legend <- function(radius, x, y, n=5, breaks = NULL, labeller, label_position = "right", ...) {
    ## rvar <- as.character(mapping)["r"]
    ## if (is_fixed_radius(rvar)) {
    ##     radius <- as.numeric(rvar)
    ## } else {
    ##     rr <- range(data[, rvar])
    ##     radius <- sapply(seq(min(rr), max(rr), length.out=5), roundDigit)
    ## }

    #if (length(radius) > n) {
    #    radius <- unique(sapply(seq(min(radius), max(radius), length.out=n), round_digit))
    #}
    
    if (n <= 1 && is.null(breaks)){
        stop('The n argument requires larger than 1.')
    }

    if (is.null(breaks)){
        radius <- scales::breaks_extended(n = n)(radius)
    }else{
        radius <- breaks
    }

    label <- FALSE
    if (!missing(labeller)) {
        if (!inherits(labeller, "function")) {
            stop("labeller should be a function for converting radius")
        }
        label <- TRUE
    }

    dd <- data.frame(r=radius, start=0, end=2*pi, x=x, y=y + radius - max(radius), maxr=max(radius))

    if(label) {
        dd$label <- labeller(dd$r)
    } else {
        dd$label <- dd$r
    }

    label_position <- match.arg(label_position, c("right", "left"))
    if (label_position == "right") {
      hjust <- "left"
      sign <- `+`
    } else {
      hjust <- "right"
      sign <- `-`
    }

    list(
        geom_arc_bar(aes(x0=!!sym("x"), y0=!!sym("y"), r0=!!sym("r"), r=!!sym("r"), start=!!sym("start"), end=!!sym("end")), data=dd, inherit.aes=FALSE),
        geom_segment(aes(x=.data[["x"]], xend=sign(.data[["x"]], .data[["maxr"]]*1.5), y=.data[["y"]]+.data[["r"]], yend=.data[["y"]]+.data[["r"]]), data=dd, inherit.aes=FALSE),
        geom_text(aes(x=sign(.data[["x"]], .data[["maxr"]]*1.6), y=.data[["y"]]+.data[["r"]], label=.data[["label"]]), data=dd, hjust=hjust, inherit.aes=FALSE, ... = ...)
    )
}

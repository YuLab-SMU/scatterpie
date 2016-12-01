##' legend of scatterpie
##'
##'
##' @title geom_scatterpie_legend
##' @param radius radius vector
##' @param x x position
##' @param y y position
##' @param n number of circle
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 geom_text
##' @export
##' @return layer
##' @author Guangchuang Yu
geom_scatterpie_legend <- function(radius, x, y, n=5) {
    ## rvar <- as.character(mapping)["r"]
    ## if (is_fixed_radius(rvar)) {
    ##     radius <- as.numeric(rvar)
    ## } else {
    ##     rr <- range(data[, rvar])
    ##     radius <- sapply(seq(min(rr), max(rr), length.out=5), roundDigit)
    ## }

    if (length(radius) > n) {
        radius <- sapply(seq(min(radius), max(radius), length.out=n), round_digit)
    }

    dd <- data.frame(r=radius, start=0, end=2*pi, x=x, y=y-rev(radius), maxr=max(radius))

    list(
        geom_arc_bar(aes_(x0=~x, y0=~y, r0=~r, r=~r, start=~start, end=~end), data=dd, inherit.aes=FALSE),
        geom_segment(aes_(x=~x, xend=~x+maxr*1.5, y=~y+r, yend=~y+r), data=dd, inherit.aes=FALSE),
        geom_text(aes_(x=~x+maxr*1.6, y=~y+r, label=~r), data=dd, hjust='left', inherit.aes=FALSE)
    )
}

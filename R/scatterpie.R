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
##' @author guangchuang yu
geom_scatterpie <- function(mapping, data, cols, ...) {
    mapping <- modifyList(mapping, aes_(r0=0, fill=~type,
                                        amount=~value))

    if (!'r' %in% names(mapping)) {
        xvar <- as.character(mapping)["x"]
        size <- diff(range(data[, xvar]))/50
        mapping <- modifyList(mapping, aes_(r=size))
        show.legend <- FALSE
    } 
    
    names(mapping)[match(c("x", "y"), names(mapping))] <- c("x0", "y0")

    df <- gather_(data, "type", "value", cols)
    geom_arc_bar(mapping, data=df, stat='pie', inherit.aes=FALSE, ...)
}

roundDigit <- function (d) {
    i <- 0
    while (d < 1) {
        d <- d * 10
        i <- i + 1
    }
    round(d)/10^i
}


is_fixed_radius <- function(rvar) {
    x <- suppressWarnings(as.numeric(rvar))
    if (is.na(x)) {
        return(FALSE)
    }
    return(TRUE)
}


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
        radius <- sapply(seq(min(radius), max(radius), length.out=n), roundDigit)
    }
    
    dd <- data.frame(r=radius, start=0, end=2*pi, x=x, y=y-rev(radius), maxr=max(radius))
    
    list(
        geom_arc_bar(aes_(x0=~x, y0=~y, r0=~r, r=~r, start=~start, end=~end), data=dd, inherit.aes=F),
        geom_segment(aes_(x=~x, xend=~x+maxr*1.5, y=~y+r, yend=~y+r), data=dd, inherit.aes=F),
        geom_text(aes_(x=~x+maxr*1.6, y=~y+r, label=~r), data=dd, hjust='left', inherit.aes=F)
    )
}

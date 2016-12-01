
round_digit <- function (d) {
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


round_digit <- function (d) {
    if (d > 1) {
        round(d)
    } else {
        round(d, -as.integer(floor(log10(abs(d)))))
    }
}



is_fixed_radius <- function(rvar) {
    x <- suppressWarnings(as.numeric(rvar))
    if (is.na(x)) {
        return(FALSE)
    }
    return(TRUE)
}

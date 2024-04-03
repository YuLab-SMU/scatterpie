
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

.check_donut_radius <- function(x){
    if (x > 1){
        cli::cli_warn("The `donut.radius` should be range 0 and 1, it was set to 0.5 automatically.")
        x <- 0.5
    }
    return(x)
}

.extract_mapping_df <- function(data,
                                mapping, 
                                extract_aes = c('x0', 'y0'),
                                col_var = NULL
                                ){
    extract.var <- lapply(extract_aes, function(x)get_aes_var(mapping, x)) |> unlist()
    extract.var <- union(col_var, extract.var)
    df <- data[, colnames(data) %in% extract.var, drop=FALSE] |> dplyr::distinct()
    return(df)
}

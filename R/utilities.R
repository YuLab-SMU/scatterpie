
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

.add_circle_layer <- function(data, mapping, rvar, bg_circle_radius, ...){
    mapping.circle <- mapping[names(mapping) %in% c('x0', 'y0', 'r', 'color', 'colour')]
    dt <- .extract_mapping_df(data, mapping, extract_aes = c('x0', 'y0', 'color', 'colour'), col_var = rvar)
    dt[[rvar]] <- dt[[rvar]] * bg_circle_radius
    params <- list(data = dt, mapping = mapping.circle, inherit.aes = FALSE, fill = 'white', ...)
    params <- .check_aes_in_params(params, c("color", "colour"))
    circle.layer <- do.call(geom_circle, params)
    return(circle.layer)
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

.check_aes_in_params <- function(params, aes_var){
    for (i in aes_var){
        if (i %in% names(params)){
            params[[i]] <- NULL
        }
    }
    return(params)
}


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


.build_pie_layer <- function(data, mapping, ...){
    params <- list(...)
    if ("label" %in% names(mapping)){
        mapping[['label']] <- NULL
    }
    params <- params[!names(params) %in% c("fontsize", "fontface", "fontfamily")]
    params$data <- data
    params$mapping <- mapping
    params$stat <- "pie"
    params$inherit.aes <- FALSE
    x <- do.call(geom_arc_bar, params)
    return(x)
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

.build_data_for_label <- function(x, threshold, var = 'value', r = "r", rlabel = 1.05){
    end_angle <- 2 * pi * cumsum(x[[var]])/sum(x[[var]])
    start_angle <- dplyr::lag(end_angle, default = 0)
    mid_angle <- 0.5 * (start_angle + end_angle)
    x[[".RATIO"]] <- round(x[[var]]/sum(x[[var]]), 2)
    x[[var]] <- round(x[[var]], 2)
    x[['.RATIO']] <- ifelse(x[['.RATIO']] < threshold, NA, x[[".RATIO"]])
    x[[var]] <- ifelse(x[[var]] < threshold, NA, x[[var]])
    x[['hjust']] <- ifelse(mid_angle > pi, 1, 0)
    x[['vjust']] <- ifelse(mid_angle < pi/2 | mid_angle > 3 * pi/2, 0, 1)
    x[['x']] <- rlabel * x[[r]] * sin(mid_angle) + x[['x']]
    x[['y']] <- rlabel * x[[r]] * cos(mid_angle) + x[['y']]
    return(x)
}

#' @importFrom rlang sym
.set_lab_mapping <- function(mapping, label_radius, label_show_ratio, bg_circle_radius){
    lab.default <- aes(x=!!sym("x"), y=!!sym("y"), hjust=!!sym("hjust"), vjust=!!sym("vjust"))
    lab.mapping <- NULL
    if (!is.null(label_radius)){
        if (!label_show_ratio){
            lab.mapping = aes(label = !!sym("value"))
        }else{
            lab.mapping <- aes(label = !!sym(".RATIO"))
        }
        lab.mapping <- modifyList(lab.default, lab.mapping)
    }

    if ("label" %in% names(mapping)){
        if (is.null(label_radius)) label_radius <- 1.06
        lab.mapping <- mapping['label']
        lab.mapping <- modifyList(lab.default, lab.mapping)
    }

    if (any(c('color', 'colour') %in% names(mapping)) && is.null(bg_circle_radius) && !is.null(lab.mapping)){
        lab.mapping <- modifyList(lab.mapping, mapping['color'] %|aes|% mapping['colour'])
    }
    return(list(mapping=lab.mapping, rlabel=label_radius))
}    

.add_label_layer <- function(pie, data, mapping, label_radius, 
                             label_show_ratio, label_threshold, 
                             bg_circle_radius, ...){
    val <- get_aes_var(mapping, 'amount')
    r.aes <- get_aes_var(mapping, 'r')
    dot.params <- list(...)
    params <- list()
    res1 <- .set_lab_mapping(mapping, label_radius, label_show_ratio, bg_circle_radius)
    
    if (is.null(res1$mapping)){
        return(pie)
    }
    group.var <- get_aes_var(mapping, 'group')
    params$data <- split(data, data[[group.var]]) |> 
          lapply(function(x).build_data_for_label(x, threshold=label_threshold, 
                             var=val, r=r.aes, rlabel = res1$rlabel)) |>
          dplyr::bind_rows()
    params$mapping <- res1$mapping
    params$inherit.aes <- FALSE
    if (!is.null(bg_circle_radius)){
        params$show.legend <- FALSE
    }
    dot.params <- .extract_label_dot_params(dot.params)
    text.layer <- do.call('geom_text', c(params, dot.params))
    return(list(pie, text.layer))
}



`%|aes|%` <- function(a, b){
    if (!is.null(a[[1]]))
        a
    else b
}

.extract_label_dot_params <- function(x){
    nm1 <- c("size", "family", "fontface")
    nm2 <- c("fontsize", "fontfamily", "fontface")
    indx <- match(nm2, names(x)) 
    indx <- indx[!is.na(indx)]
    if (length(indx)==0){
        return(NULL)
    }
    x <- x[indx]
    indx <- match(names(x), nm2)
    names(x) <- nm1[indx]
    return(x)
}


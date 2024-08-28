##' scatter pie plot
##'
##'
##' @title geom_scatterpie
##' @rdname geom-scatterpie
##' @param mapping aes mapping
##' @param data data
##' @param cols cols the pie data
##' @param pie_scale amount to scale the pie size if there is no radius mapping exists
##' @param sorted_by_radius whether plotting large pie first
##' @param legend_name name of fill legend
##' @param long_format logical whether use long format of input data
##' @param label_radius numeric the radius of label position (relative the radius of pie),
##' default is NULL, when it is provided, the ratio or value label will be displayed.
##' @param label_show_ratio logical only work when \code{label_radius} is not NULL,
##' default is TRUE, meaning the ratio of label will be displayed.
##' @param label_threshold numeric the threshold is to control display the label, the ratio of 
##' slice pie smaller than the threshold will not be displayed. default is 0.
##' @param donut_radius numeric the radius of donut chart (relative the radius of circle), default is NULL.
##' it should be between 0 and 1, if it is provided, the donut chart will be displayed instead of pie chart.
##' @param bg_circle_radius numeric the radius of background circle, default is FALSE, we suggest setting it 
##' to between 1 and 1.5 .
##' @param ... additional parameters
##' @importFrom ggforce geom_arc_bar geom_circle
##' @importFrom utils modifyList
##' @importFrom tidyr gather
##' @importFrom rlang enquo
##' @importFrom rlang !!
##' @importFrom ggplot2 aes_ aes
##' @importFrom ggfun get_aes_var
##' @importFrom stats as.formula
##' @importFrom dplyr bind_rows group_by group_split
##' @export
##' @return layer
##' @author Guangchuang Yu
##' @examples
##' library(ggplot2)
##' d <- data.frame(x=rnorm(5), y=rnorm(5))
##' d$A <- abs(rnorm(5, sd=1))
##' d$B <- abs(rnorm(5, sd=2))
##' d$C <- abs(rnorm(5, sd=3))
##' 
##' ggplot() + 
##' geom_scatterpie(
##'   aes(x=x, y=y), data=d, cols=c("A", "B", "C")
##' ) + 
##' coord_fixed()
##' 
##' ggplot() + 
##' geom_scatterpie(
##'   aes(x=x, y=y), data = d, cols=c("A", "B", "C"), 
##'   label_radius=1.05
##' ) + 
##' coord_fixed()
##'
##' d <- tidyr::gather(d, key="letters", value="value", -x:-y)
##' ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols="letters", long_format=TRUE) + coord_fixed()
##' p1 <- ggplot() + 
##'       geom_scatterpie(
##'         mapping = aes(x=x, y=y), data=d, cols="letters", 
##'         long_format=TRUE, 
##'         donut_radius=.5
##'       ) + 
##'       coord_fixed()
##' p1
##' p2 <- ggplot() + 
##'       geom_scatterpie(
##'         mapping = aes(x=x, y=y), data=d, cols="letters", 
##'         long_format=TRUE, 
##'         donut_radius = .5, 
##'         bg_circle_radius = 1.2
##'       ) + 
##'       coord_fixed()
##' p2
##' d |> dplyr::select(c(x, y)) |> dplyr::distinct() |> dplyr::mutate(Cell=c('A','A','B','C','B')) -> d2
##' d |> dplyr::left_join(d2) -> d3
##' d3$r_size <- c(2, 3, 4, 5, 6) * .01
##' 
##' head(d3)
##' p3 <- ggplot() +
##'      geom_scatterpie(data = d3, mapping = aes(x=x, y=y, r = r_size, color=Cell), cols="letters",
##'                      long_format=TRUE, donut_radius=.5, color = NA, linewidth=2,
##'                       bg_circle_radius=1.2) + coord_fixed()
##' p3
##'
##' p4 <- ggplot() +
##'       geom_scatterpie(data = d3,
##'                       mapping = aes(x, y = y, r = r_size),
##'                       cols = 'letters',
##'                       long_format = TRUE,
##'                       label_radius = 1.1,
##'                       label_show_ratio = FALSE,
##'                       label_threshold = 0.06,
##'                       fontsize = 3
##'       ) +
##'       coord_fixed()
##' p4
geom_scatterpie <- function(mapping = NULL, data = NULL, cols, pie_scale = 1, 
                            sorted_by_radius = FALSE, legend_name = "type", 
                            long_format = FALSE, label_radius = NULL, 
                            label_show_ratio = TRUE, label_threshold = 0,
                            donut_radius = NULL, bg_circle_radius = NULL, ...) {

    structure(list(
        mapping = mapping,
        data = data,
        cols = cols,
        pie_scale = pie_scale,
        sorted_by_radius = sorted_by_radius,
        legend_name = legend_name,
        long_format = long_format,
        label_radius = label_radius,
        label_show_ratio = label_show_ratio,
        label_threshold = label_threshold,
        donut_radius = donut_radius,
        bg_circle_radius = bg_circle_radius,
        ...
        ),
        class = "layer_scatterpie"
    )                            
}


##' @rdname geom-scatterpie
##' @export
geom_scatterpie2 <- function(mapping = NULL, data, cols, pie_scale = 1, 
                            sorted_by_radius = FALSE, legend_name = "type", 
                            long_format = FALSE, label_radius = NULL, 
                            label_show_ratio = TRUE, label_threshold = 0,
                            donut_radius = NULL, bg_circle_radius = NULL, ...){
    if (is.null(mapping))
        mapping <- aes_(x = ~x, y = ~y)
    mapping <- modifyList(mapping,
                          aes_(r0 = 0,
                               fill = as.formula(paste0("~", legend_name)),
                               amount=~value)
                          )

    if (!'r' %in% names(mapping)) {
        xvar <- get_aes_var(mapping, "x")
        size <- diff(range(data[, xvar]))/ 50 * pie_scale
        data$r <- size
        mapping <- modifyList(mapping, aes_(r=~r))
        if (!is.null(donut_radius)){
            donut_radius <- .check_donut_radius(donut_radius)
            data$.R0 <- size * donut_radius
            mapping <- modifyList(mapping, aes_(r0 = ~.R0))
        }
    } else {
        if (!is.null(donut_radius)) {
            rvar <- get_aes_var(mapping, 'r')
            donut_radius <- .check_donut_radius(donut_radius)
            data$.R0 <- data[[rvar]] * donut_radius
            mapping <- modifyList(mapping, aes_(r0 = ~.R0))
        }
    }

    names(mapping)[match(c("x", "y"), names(mapping))] <- c("x0", "y0")
    if(long_format==TRUE){
      df <- data
      names(df)[which(names(df) == cols)] = legend_name
      cols2 <- enquo(cols)
    } else{
      data <- data[rowSums(data[, cols]) > 0, ]
      ## df <- gather_(data, "type", "value", cols)
      cols2 <- enquo(cols)
      df <- gather(data, "type", "value", !!cols2)
      df$type <- factor(df$type, levels = cols) # set legend order based on order of "cols"      
      names(df)[which(names(df) == "type")] = legend_name
    }
    
    if (!"group" %in% names(mapping)){
      xvar <- get_aes_var(mapping, 'x0')
      yvar <- get_aes_var(mapping, 'y0')
      df <- df |> dplyr::group_by(!!as.symbol(xvar), !! as.symbol(yvar)) |>
       dplyr::group_split() |> as.list()
      names(df) <- seq_len(length(df))
      df <- dplyr::bind_rows(df, .id=".group.id")
      mapping <- modifyList(mapping, aes_(group = ~.group.id))
    }

    if ('r' %in% colnames(df)){
        rvar <- 'r'
    }else{
        rvar <- get_aes_var(mapping, 'r')
    }

    if (!sorted_by_radius) {
        pie.layer <- .build_pie_layer(df, mapping, ...)
        if (!is.null(bg_circle_radius)){
            circle.layer <- .add_circle_layer(data = df, mapping = mapping, rvar = rvar, 
                                              bg_circle_radius = bg_circle_radius, ...)
            pie.layer <- list(circle.layer, pie.layer)
        }
        pie.layer <- .add_label_layer(pie.layer, df, mapping, label_radius, 
                                      label_show_ratio, label_threshold,
                                      bg_circle_radius, ...)
        return(pie.layer)
    }

    lapply(split(df, df[,rvar, drop=TRUE])[as.character(sort(unique(df[,rvar, drop=TRUE]), decreasing=TRUE))], 
           function(d) 
           {
        pie.layer <- .build_pie_layer(d, mapping, ...)
        if (!is.null(bg_circle_radius)){
            circle.layer <- .add_circle_layer(data = d, mapping = mapping, rvar = rvar, 
                                              bg_circle_radius = bg_circle_radius, ...)
            pie.layer <- list(circle.layer, pie.layer)
        }
	pie.layer <- .add_label_layer(pie.layer, d, mapping, label_radius, 
                                      label_show_ratio, label_threshold, bg_circle_radius, ...)
        return(pie.layer)
      }
   )
}


##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add layer_scatterpie
##' @export
ggplot_add.layer_scatterpie <- function(object, plot, object_name) {
    if (is.null(object$data)) object$data <- plot$data
    layer <- do.call(geom_scatterpie2, object)
    ggplot_add(layer, plot, object_name)
}


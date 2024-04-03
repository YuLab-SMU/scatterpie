##' scatter pie plot
##'
##'
##' @title geom_scatterpie
##' @param mapping aes mapping
##' @param data data
##' @param cols cols the pie data
##' @param pie_scale amount to scale the pie size if there is no radius mapping exists
##' @param sorted_by_radius whether plotting large pie first
##' @param legend_name name of fill legend
##' @param long_format logical whether use long format of input data
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
##' @examples
##' library(ggplot2)
##' d <- data.frame(x=rnorm(5), y=rnorm(5))
##' d$A <- abs(rnorm(5, sd=1))
##' d$B <- abs(rnorm(5, sd=2))
##' d$C <- abs(rnorm(5, sd=3))
##' ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=c("A", "B", "C")) + coord_fixed()
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
##' @author Guangchuang Yu
geom_scatterpie <- function(mapping=NULL, data, cols, pie_scale = 1, 
                            sorted_by_radius = FALSE, legend_name = "type", 
                            long_format=FALSE, donut_radius=NULL, bg_circle_radius=NULL, ...) {
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
        mapping <- modifyList(mapping, aes_(r=size))
        if (!is.null(donut_radius)){
            donut_radius <- .check_donut_radius(donut_radius)
            mapping <- modifyList(mapping, aes_(r0 = ~size * donut_radius))
        }
    }else{
        if (!is.null(donut_radius)){
            rvar <- get_aes_var(mapping, 'r')
            donut_radius <- .check_donut_radius(donut_radius)
            mapping <- modifyList(mapping, aes_(r0 = ~rvar * donut_radius))
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
    ## df <- gather_(data, "type", "value", cols)
    # cols2 <- enquo(cols)
    # df <- gather(data, "type", "value", !!cols2)
    # names(df)[which(names(df) == "type")] = legend_name

    ## df$type <- factor(df$type, levels=cols)
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
        pie.layer <- geom_arc_bar(mapping, data=df, stat='pie', inherit.aes=FALSE, ...)
        if (!is.null(bg_circle_radius)){
            mapping.circle <- mapping[names(mapping) %in% c('x0', 'y0')]
            dt <- .extract_mapping_df(df, mapping, extract_aes = c('x0', 'y0'), col_var=rvar)
            mapping.circle <- modifyList(mapping.circle, aes(r = !!as.symbol(rvar) * bg_circle_radius))
            circle.layer <- geom_circle(data = dt, mapping = mapping.circle, inherit.aes=FALSE, ...)
            pie.layer <- list(circle.layer, pie.layer)
        }
        return(pie.layer)
    }

    lapply(split(df, df[,rvar, drop=TRUE])[as.character(sort(unique(df[,rvar, drop=TRUE]), decreasing=TRUE))], 
           function(d) 
           {
        pie.layer <- geom_arc_bar(mapping, data=d, stat='pie', inherit.aes=FALSE, ...)
        if (!is.null(bg_circle_radius)){
            mapping.circle <- mapping[names(mapping) %in% c('x0', 'y0', 'r')]
            d2 <- .extract_mapping_df(d, mapping, extract_aes = c('x0', 'y0', 'r'), col_var = rvar)
            mapping.circle <- modifyList(mapping.circle, aes(r = !!as.symbol(rvar) * bg_circle_radius))
            circle.layer <- geom_circle(data = d2, mapping = mapping.circle, inherit.aes=FALSE, ...)
            pie.layer <- list(circle.layer, pie.layer)
        }
        return(pie.layer)
      }
   )
}



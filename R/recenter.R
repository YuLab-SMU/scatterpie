##' re-center map data
##'
##'
##' @title recenter
##' @param mapdata map data, shoud be a data.frame
##' @param center center
##' @param longitude_column longitude column
##' @param clean whether clean data to prevent cross lines
##' @return updated map data
##' @export
##' @author ygc
recenter <- function(mapdata, center, longitude_column='long', clean=FALSE) {
    if (center <= 0) {
        stop("center should be positive value...")
    }
    md2 <- mapdata
    md2$long <- md2$long + 360
    md2$group <- md2$group + max(md2$group) + 1

    mapdata <- rbind(mapdata, md2)
    long <- mapdata[,longitude_column]

    res <- subset(mapdata, long >= center-180 & long <= center+180)

    return(res)
}



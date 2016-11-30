##' re-center map data
##'
##'
##' @title recenter
##' @param mapdata map data, shoud be a data.frame
##' @param center center
##' @param longitude_column longitude column
##' @return updated map data
##' @export
##' @author ygc
recenter <- function(mapdata, center, longitude_column='long') {
    if (center <= 0) {
        stop("center should be positive value...")
    }
    long <- mapdata[,longitude_column]
    mapdata[,longitude_column] <- ifelse(long < center - 180 , long + 360, long)
    return(mapdata)
}


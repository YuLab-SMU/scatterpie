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
    long <- mapdata[,longitude_column]
    mapdata[,longitude_column] <- ifelse(long < center - 180 , long + 360, long)
    if (clean) {
        ##
        ## https://stackoverflow.com/questions/10620862/use-different-center-than-the-prime-meridian-in-plotting-a-world-map
        ##
        ## here is just my quick and dirty hack

        long <- mapdata[, longitude_column]
        mapdata <- mapdata[long > 1.04 * min(long) & long < 0.96 * max(long),]
    }
    return(mapdata)
}



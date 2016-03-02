#' @title DataFileInfo
#' @description Brief information on the contents of a netCDF data file
#' @details Reads the flight number, project, date/time, position, and variables from
#' the netCDF file and returns a list of properties. Alternately, produces similar
#' information for a saved (.Rdata-format) data.frame produced by reading such a
#' netCDF file.
#' @aliases dataFileInfo
#' @author William Cooper
#' @export DataFileInfo
#' @import ncdf4
#' @param fileLocation A complete address to the netCDF data file
#' @return A list containing named characteristics of the file. For example, 
#' in the example below the variables in the file are returned in FI$Variables.
#' See names(FI) for a list of characteristics that are returned.
#' @examples 
#' FI <-DataFileInfo (sprintf ("%s/extdata/RAFdata.nc", path.package ("Ranadu")))

DataFileInfo <- function (fileLocation) {
  # get information about a netCDF data file
  if (!(grepl ('Rdata$', fileLocation))) {
    netCDFfile <- nc_open (fileLocation)
    namesCDF <- names (netCDFfile$var)
    nms <- names(netCDFfile$dim)
    Time <- ncvar_get (netCDFfile, "Time")
    LATC <- ncvar_get (netCDFfile, "LATC")
    LONC <- ncvar_get (netCDFfile, "LONC")
    time_units <- ncatt_get (netCDFfile, "Time", "units")
    tref <- sub ('seconds since ', '', time_units$value)
    Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
    sampleRate <- 1
    if ('sps25' %in% nms) {sampleRate <- 25}
    if ('sps50' %in% nms) {sampleRate <- 50}
    if ('sps100' %in% nms) {sampleRate <- 100}
    Flight <- list(Number=as.character (ncatt_get (netCDFfile, 0, "FlightNumber")[2]))
    Flight$Project <- as.character (ncatt_get (netCDFfile, 0, 'ProjectName')[2])
    Flight$Platform <- as.character (ncatt_get (netCDFfile, 0, 'Platform')[2])
    Flight$DataFile <- fileLocation
    Flight$Start <- min (Time, na.rm=TRUE)
    Flight$End <- max (Time, na.rm=TRUE)
    Flight$Rate <- sampleRate
    Flight$LatMin <- min (LATC, na.rm=TRUE)
    Flight$LatMax <- max (LATC, na.rm=TRUE)
    Flight$LonMin <- min (LONC, na.rm=TRUE)
    Flight$LonMax <- max (LONC, na.rm=TRUE)
    Flight$Variables <- namesCDF
  } else {
    fileLoad <- load (fileLocation)
    D <- get (fileLoad[1])
    Datts <- getAttributes(D, .print=FALSE)
    nm <- vector ('character', length (Datts))
    for (i in 1:40) {nm[i] <- names (Datts[][[i]])}
    Flight <- list (Number=as.character (Datts[[which ('FlightNumber' == nm)]]))
    Flight$Project <- as.character (Datts[[which ('ProjectName' == nm)]])
    Flight$Platform <- as.character (Datts[[which ('Platform' == nm)]])
    Flight$DataFile <- fileLocation
    Flight$Start <- min (D$Time, na.rm=TRUE)
    Flight$End <- max (D$Time, na.rm=TRUE)
    sampleRate <- 1
    seconds <- as.numeric(D$Time[nrow(D)]-D$Time[1], units='secs')
    if (nrow (D) > 2 * seconds) {sampleRate <- 25}
    if (nrow (D) > 27 * seconds ) {sampleRate <- 50}
    if (nrow (D) > 55 * seconds ) {sampleRate <- 100}
    Flight$Rate <- sampleRate
    Flight$LatMin <- as.character (Datts[[which ("geospatial_lat_min" == nm)]])
    Flight$LatMax <- as.character (Datts[[which ("geospatial_lat_max" == nm)]])
    Flight$LonMin <- as.character (Datts[[which ("geospatial_lon_min" == nm)]])
    Flight$LonMax <- as.character (Datts[[which ("geospatial_lon_max" == nm)]])
    nms <- names (D)
    nms <- nms[nms != 'Time']
    Flight$Variables <- nms
  }
  return(Flight)
}

#' @title DataFileInfo
#' @description Brief information on the contents of a netCDF data file
#' @details Reads the flight number, project, date/time, position, and variables from
#' the netCDF file and returns a list of properties.
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
  return(Flight)
}

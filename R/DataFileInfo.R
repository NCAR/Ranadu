#' @title DataFileInfo
#' @description Brief information on the contents of a netCDF data file
#' @details Reads the flight number, project, date/time, position, and variables from
#' the netCDF file and returns a list of properties. Alternately, produces similar
#' information for a saved (.Rdata-format) data.frame produced by reading such a
#' netCDF file. In addition, produces a list of the long_names associated with the
#' variables and a list of the "measurands" along with the redundant variables
#' representing that measurand.
#' @aliases dataFileInfo
#' @author William Cooper
#' @export DataFileInfo
#' @import ncdf4
#' @param fileLocation A complete address to the netCDF data file.
#' @param LLrange Logical, if TRUE, range in latitude and longitude for the
#' flight is included in the returned list. Set FALSE to get a faster return
#' in case the range in positions is not needed.
#' @return A list containing named characteristics of the file. For example, 
#' in the example below the variables in the file are returned in FI$Variables.
#' See names(FI) for a list of characteristics that are returned.
#' @examples 
#' FI <-DataFileInfo (sprintf ("%s/extdata/RAFdata.nc", path.package ("Ranadu")))

DataFileInfo <- function (fileLocation = setFileName(), LLrange=TRUE) {
  # get information about a netCDF data file or saved data.frame, Rdata format
  if (!(grepl ('Rdata$', fileLocation))) {
    netCDFfile <- nc_open (fileLocation)
    namesCDF <- names (netCDFfile$var)
    nms <- names(netCDFfile$dim)
    ## Check for old-time convention:
    oldTime <- 'base_time' %in% namesCDF
    ## check source/institution:
    ATTG <- ncatt_get (netCDFfile, 0)   # get list of global attributes
    SOURCE <- 'NCAR'
    if ('Source' %in% names (ATTG) && grepl('Wyoming', ATTG$Source)) {
      SOURCE <- 'UWYO'
    }
    ## special section for FAAM data ##
    if ('source' %in% names (ATTG) && grepl('FAAM', ATTG$source)) {
      SOURCE <- 'FAAM'
    }
    FAAM <- ifelse (SOURCE == 'FAAM', TRUE, FALSE)
    UWYO <- ifelse (SOURCE == 'UWYO', TRUE, FALSE)
    if (UWYO) {
      if ('time' %in% nms) {
        Time <- ncvar_get (netCDFfile, "time")
        time_units <- ncatt_get (netCDFfile, "time", "units")        
      }
    } else if (oldTime) {  ## This is old-format without Time variable, with Time dimension
        base_time <- ncvar_get(netCDFfile, 'base_time')
        tref <- ncatt_get (netCDFfile, 'base_time', 'long_name')$value
        tref <- sub('.$', '', sub('Seconds since ', '', tref))
        if (tref == 'Jan 1, 1970') {tref <- '1970-01-01'}
        time_offset <- ncvar_get (netCDFfile, 'time_offset')
        Time <- as.POSIXct (as.POSIXct (tref, tz='UTC') + time_offset + base_time, tz='UTC')
    } else {
        Time <- ncvar_get (netCDFfile, "Time")
        time_units <- ncatt_get (netCDFfile, "Time", "units")
        tref <- sub ('seconds since ', '', time_units$value)
        Time <- as.POSIXct (as.POSIXct (tref, tz='UTC')+Time, tz='UTC')
    }
    if (LLrange) {
      if ('LATC' %in% namesCDF) {LATC <- ncvar_get (netCDFfile, "LATC")}
      if ('LONC' %in% namesCDF) {LONC <- ncvar_get (netCDFfile, "LONC")}
    }
    
    sampleRate <- 1
    if (!UWYO) {
      if ('sps25' %in% nms) {sampleRate <- 25}
      if ('sps50' %in% nms) {sampleRate <- 50}
      if ('sps100' %in% nms) {sampleRate <- 100}
    }
    Flight <- list(Number=as.character (ncatt_get (netCDFfile, 0, "FlightNumber")[2]))
    Flight$Project <- as.character (ncatt_get (netCDFfile, 0, 'ProjectName')[2])
    if (UWYO) {
      Flight$Platform <- as.character (ncatt_get (netCDFfile, 0, 'Aircraft')[2])
    } else {
        if ('Platform' %in% names(ATTG)) {
            Flight$Platform <- as.character (ncatt_get (netCDFfile, 0, 'Platform')[2])
        } else {
            Flight$Platform <- as.character (ncatt_get (netCDFfile, 0, 'Aircraft')[2])
        }
    }
    Flight$DataFile <- fileLocation
    Flight$Start <- min (Time, na.rm=TRUE)
    Flight$End <- max (Time, na.rm=TRUE)
    Flight$Rate <- sampleRate
    if (LLrange && exists ('LATC')) {
      Flight$LatMin <- min (LATC, na.rm=TRUE)
      Flight$LatMax <- max (LATC, na.rm=TRUE)
    } else {
      Flight$LatMin <- NA
      Flight$LatMax <- NA
    }
    if (LLrange && exists ('LONC')) {
      Flight$LonMin <- min (LONC, na.rm=TRUE)
      Flight$LonMax <- max (LONC, na.rm=TRUE)
    } else {
      Flight$LonMin <- NA
      Flight$LonMax <- NA
    }
    Flight$Variables <- sort(namesCDF)
    if (SOURCE == 'NCAR') {
      ## also get a list of "measurands"
      measurand <- vector()
      vls <- vector()
      ## get the associated long_name if available
      Flight$LongNames <- Flight$Variables
      for (v in Flight$Variables) {
        ATT <- ncatt_get (netCDFfile, v)
        if ('long_name' %in% names(ATT)) {
          Flight$LongNames[which(v == Flight$Variables)] <- ATT$long_name
        }
        if ('standard_name' %in% names(ATT)) {
          measurand <- c(measurand, ATT$standard_name)
          vls <- c(vls, c(ATT$standard_name, v))
        }
      }
      dim(vls) <- c(2, length(vls) / 2)
      Measurands <- TRUE
      if (Measurands) {
        measurand <- unique (vls[1, ])
        ml <- list(measurand)
        
        ## now find all variables with specified measurand:
        for (ms in measurand) {
          for (i in 1:dim(vls)[2]) {
            if (vls[1, i] == ms) {
              ims <- length(ml[[ms]]) + 1
              ml[[ms]][[ims]] <- vls[2, i]
            }
          }
          ## original, very slow, way to do this:
          # for (v in Flight$Variables) {
          #   ATT <- ncatt_get (netCDFfile, v)
          #   if ('standard_name' %in% names(ATT)) {
          #     if (ATT$standard_name == ms) {
          #       ml[[ms]][[ims]] <- v
          #       ims <- ims + 1
          #     }
          #   }
          # }
        }
        Flight$Measurands <- ml
      }
    }
    if (FAAM) {
      ## get short names for variables instead of netCDF var name
      snames <- namesCDF
      for (VFAAM in namesCDF) {
        # print (VFAAM)
        ATTV <- ncatt_get (netCDFfile, VFAAM)
        if ('short_name' %in% names(ATTV)) {
          snames <- snames [-which (VFAAM == snames)]
          snames[VFAAM] <- sub (' ', '', ATTV$short_name)
          # print (sprintf ('short_name %s', ATTV$short_name))
        } else {
          # print (sprintf (' remove variable %s', VFAAM))
          snames <- snames [-which (VFAAM == snames)]
        }
      }
      Flight$Variables <- sort(unname(snames))
    }
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
    nms <- nms[nms != 'Time' && nms != 'time']
    Flight$Variables <- nms
  }
  nc_close(netCDFfile)
  return(Flight)
}

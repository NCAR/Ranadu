#' @title standardVariables
#' @description Standard set of variables, usually used in call to getNetCDF 
#' @details Sets a standard list of variable names in VarList suitable for use in a call to getNetCDF. Optionally, add "list" to the variables.
#' @details The standard variables are ATX, DPXC, EWX, GGALT, LATC, LONC, MACHX, MR, PALT, PSXC, QCSC, TASX, WDC, WSC, WIC.
#' @aliases standardVariables StandardVariables
#' @author William Cooper
#' @export standardVariables
#' @param list An optional list of variable names to add to the standard list
#' @examples 
#' standardVariables (c("VEW", "PLWCC"))
standardVariables <- function (list=NULL) {
  VarList <-c("ATX", "DPXC", "EWX", "GGALT", "LATC", "LONC", 
              "MACHX", "MR", "PALT", "PSXC", "QCXC", "TASX", 
              "WDC", "WSC", "WIC") 
  if (length(list) > 0) {
    VarList <- c(VarList, list)
  }  
  return (VarList)
}

#' @title getNetCDF
#' @description Loads selected variables in a specified RAF-aircraft data file into a data.frame.
#' @details 'Time' is converted to a POSIXcf variable, and other variables specified in a VarList list are included in the data.frame. By default, the entire file is loaded, but optional arguments Start and End limit the time range. After reading the data, the netCDF file is closed before returning the data.frame to the calling program.
#' @details If you get an error message with a long list of variable names, it probably indicates that one of the specified variables is not in this netCDF file.
#' @details This routine handles 25-Hz files, but the Start-End option does not yet work for those files. The returned variables are single-dimension, and for 25 Hz files Time is returned as a sequence with fractional-second values.
#' @aliases getNetCDF getnetcdf
#' @author William Cooper
#' @import "ncdf"
#' @export getNetCDF
#' @param fname string, full file name 'e.g., "/scr/raf_data/PREDICT/PREDICTrf01.nc"
#' @param VarList vector of variable names to load from the netCDF file.
#' @param Start An optional numeric giving the desired start time in HHMMSS format
#' @param End An optional numeric giving the desired end time in HHMMSS format
#' @param F An optional numeric entered in the data.frame as a column 'RF' all set to this integer. This may be useful when the resulting data.frame is combined with others, to have a variable distinguishing different flights.
#' @return data.frame containing the specified variables as columns, along with 'Time' and optionally the flight number 'FNo'
#' @examples 
#' \dontrun{D <- getNetCDF ("PathToFile.nc", c("Var1", "Var2", "Var3"))}
#' \dontrun{D <- getNetCDF ("PathToFile.nc", c("Var1", "Var2"), 133000, 143000, 5)}
getNetCDF <- function (fname, VarList, Start=0, End=0, F=0) {
# This function reads the netCDF file 'fname' and extracts 
# the variables specified in 'VarList', returning the
# results in a data.frame. It includes the flight number F
# in the data.frame, as variable RF. It converts "Time",
# seconds after a reference time in the netCDF files, to
# a POSIXct date/time variable.
  netCDFfile = open.ncdf(fname)
  NV <- length (VarList)
  Time <- get.var.ncdf (netCDFfile, "Time")
  DL <- length (Time)
  # Expand Time to be high-rate
  if ("sps25" %in% names(netCDFfile$dim)) {
    T <- vector ("numeric", 25*length(Time))
    for (i in 1:length(Time)) {
      for (j in 0:24) {
        T[(i-1)*25+j+1] <- Time[i]+0.04*j
      }  
    }
    Time <- T
  }
  time_units <- att.get.ncdf (netCDFfile, "Time", "units")
  tref <- sub ('seconds since ', '', time_units$value)
  Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
          # see if limited time range wanted:
  i1 <- ifelse ((Start != 0), getIndex (Time, Start), 1)
  i2 <- ifelse ((End != 0), getIndex (Time, End), length(Time))
  r <- i1:i2
  # for a 25-Hz file, r is appropriate 25-Hz index, but also need
  # the 1-Hz index for extrapolation:
  if ("sps25" %in% names(netCDFfile$dim)) {
    r2 <- ((i1-1)/25+1):((i2-1)/25+1)
    
  } else {
    r2 <- r
  }
  DL <- length(r2)
  Time <- Time[r]
  SE <- getStartEnd (Time)
  d <- data.frame(Time)
  for (i in 1:NV) {
    X <- (get.var.ncdf(netCDFfile, VarList[i]))
    if ("sps25" %in% names(netCDFfile$dim)) {
      DM <- length(dim(X))           
      if (DM == 2) {    # flatten
        X <- X[,r2]
        dim(X) <- dim(X)[1]*dim(X)[2]
        d[VarList[i]] <- X
      } else {
        X <- X[r2]
	# for variables not 25-Hz, interpolate to 25 Hz, then filter
        T <- vector ("numeric", 25*DL)
        for (k in 2:(DL-1)) {
          if ((is.na(X[k])) | (X[k] == -32767) |
                is.na(X[k-1]) | is.na(X[k+1])) {
            for (j in 1:25) {
              T[(k-1)*25+j] <- -32767
            }
          } else {
            for (j in 0:12) {
              T[(k-1)*25+j+1] <- X[k-1]+0.04*(j+13)*(X[k]-X[k-1])
            }
            for (j in 13:24) {
              T[(k-1)*25+j+1] <- X[k]+0.04*(j-12)*(X[k+1]-X[k])
            }
          }
        }
	# just replicate the start and end measurements
        k <- 1
        if (is.na(X[k]) | is.na(X[k+1])) {
          for (j in 1:25) {
            T[j] <- -32767.
          } 
        } else {
          for (j in 0:12) {
	          T[(k-1)*25+j+1] <- X[k]
	        }
	        for (j in 13:24) {
	          T[(k-1)*25+j+1] <- X[k]+0.04*(j-12)*(X[k+1]-X[k])
	        }
        }
	      k <- DL
        if (is.na(X[k]) | is.na(X[k-1])) {
          for (j in 1:25) {
            T[j] <- -32767.
          } 
        } else {
	        for (j in 0:12) {
	          T[(k-1)*25+j+1] <- X[k-1]+0.04*(j+13)*(X[k]-X[k-1])
	        }
	        for (j in 13:24) {
	          T[(k-1)*25+j+1] <- X[k]
	        }
        }
	      #T <- filter(butter(3,2./25.),T)
	      T <- filter(sgolay(4,75),T)      
        d[VarList[i]] <- T
      }
    } else {
      d[VarList[i]] <- X[r2]
    }
  }
  if (F != 0) {    # if specified, include the flight number
    RF <- rep (F, times=length(Time))    # label flight number
    d["RF"] <- RF[r]
  }
  close.ncdf (netCDFfile)
  d[d == -32767.] <- NA   # replace missing-value with NA
  return (d)
}


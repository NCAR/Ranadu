#' @title standardVariables
#' @description Standard set of variables, usually used in call to getNetCDF 
#' @details Sets a standard list of variable names in VarList suitable for use in a call to getNetCDF. Optionally, add "list" to the variables.
#' The standard variables are ATX, DPXC, EWX, GGALT, LATC, LONC, MACHX, MR, PALT, PSXC, QCXC, TASX, WDC, WSC, WIC.
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
#' @details 'Time' is converted to a POSIXct variable, and other variables specified in VarList are included in the data.frame. By default, the entire file is loaded, but optional arguments Start and End limit the time range. After reading the data, the netCDF file is closed before returning the data.frame to the calling program.
#' @details The global attributes in the netCDF file are loaded as attributes of the returned data.frame,
#' and attributes of each requested variable are also assigned to that column in the data.frame from the variable attributes in the netCDF file.
#' When working with attributes, it is a feature of R data.frames that subsetting loses all the assigned variable attributes.
#' If you want to preserve them, copy them via A <- attributes (Data$VAR), remove A$dim (e.g., A$dim <- NULL),
#' and re-assign via attributes (DataNew$VAR) <- A. The function does not handle multi-dimensional
#' variables like CCDP yet; it does work for 25-Hz files, with fractional-second times.
#' @aliases getNetCDF getnetcdf
#' @author William Cooper
#' @import "ncdf4"
#' @export getNetCDF
#' @param fname string, full file name, e.g., "/scr/raf_data/PREDICT/PREDICTrf01.nc"
#' @param VarList vector of variable names to load from the netCDF file. Use "ALL" to load everything.
#' @param Start An optional numeric giving the desired start time in HHMMSS format
#' @param End An optional numeric giving the desired end time in HHMMSS format
#' @param F An optional numeric entered in the data.frame as a column 'RF' all set to this integer. This may be useful when the resulting data.frame is combined with others, to have a variable distinguishing different flights.
#' @return data.frame containing the specified variables as columns, along with 'Time' and optionally the flight number 'RF'
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
  
  ## get the header information
  netCDFfile = nc_open(fname)
  if ("ALL" %in% VarList) {
    VarList <- names (netCDFfile$var)
  }
  ## check that requested variables are present in netCDF file; fail otherwise
  for (V in VarList) {
    if (length (which (grepl (V, names (netCDFfile$var))))) {next}
    cat (sprintf ("requested variable %s not in netCDF file;\n ---->ngetNetCDF returning with error", V))
    return (-1)
  }
  Time <- ncvar_get (netCDFfile, "Time")
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
  time_units <- ncatt_get (netCDFfile, "Time", "units")
  tref <- sub ('seconds since ', '', time_units$value)
  Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
  # see if limited time range wanted:
  i1 <- ifelse ((Start != 0), getIndex (Time, Start), 1)
  if (End != 0) {
    i2 <- getIndex (Time, End)
    if ("sps25" %in% names (netCDFfile$dim)) {i2 <- i2 + 24}
  } else {
    i2 <- length (Time)
  }
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
  ## save 'Time' attributes:
  ATT <- ncatt_get (netCDFfile, "Time")   # get list of Time attributes
  for (A in names (ATT)) {
    attr(Time, A) <- ATT[[A]]
  }
  d <- data.frame(Time)
  ## save the dimensions, useful if ever re-writing to netCDF:---------------------
  ##    but, to save space, omit the list of times
  nf <- netCDFfile
  nf$dim[1]$Time$vals <- NULL
  attr (d, "Dimensions") <- nf$dim
  ## Save all the global attributes in the netCDF file as 'd' attributes:----------
  ATT <- ncatt_get (netCDFfile, 0)   # get list of global attributes
  for (A in names (ATT)) {
    attr(d, A) <- ATT[[A]]
  }
  attr (d, "R_dataframe_created") <- date()    # add one global attribute:
  
  ## Add the requested variables:------------------------------------------------
  for (V in VarList) {
    ## save dimensions for the variable:
    datt <- list()
    for (dd in netCDFfile$var[[V]]$dim) {
      datt[[length(datt)+1]] <- dd$name
    }    ## later, save datt as an attribute of V
    X <- ncvar_get (netCDFfile, V)
    ATT <- ncatt_get (netCDFfile, V)
    if ("sps25" %in% names(netCDFfile$dim)) {
      DM <- length(dim(X))           
      if (DM == 2) {    # flatten
        X <- X[,r2]
        dim(X) <- dim(X)[1]*dim(X)[2]
        ## add variable attributes as in netCDF file
        for (A in names (ATT)) {
          attr (X, A) <- ATT[[A]]
        }
        attr (X, "Dimensions") <- datt
        d[V] <- X
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
        T <- signal::filter(sgolay(4,75),T)
        ## add variable attributes as in netCDF file
        for (A in names (ATT)) {
          attr (T, A) <- ATT[[A]]
        }
        attr (T, "Dimensions") <- datt
        d[V] <- T
      }
    } else {      ## this is the 1-Hz section
      X <- X[r2]
      ## add variable attributes as in netCDF file
      for (A in names (ATT)) {
        attr (X, A) <- ATT[[A]]
      }
      attr (X, "Dimensions") <- datt
      d[V] <- X
    }
  }
  if (F != 0) {    # if specified, include the flight number
    RF <- rep (F, times=length(Time))    # label flight number
    d["RF"] <- RF
  }
  nc_close (netCDFfile)
  d[d == -32767. ] <- NA   # replace missing-value with NA
  return (d)
}

#' @title standardVariables
#' @description Standard set of variables for data.frame
#' @details Sets a standard list of variable names in VarList suitable for use in a call to getNetCDF. Optionally, add "list" to the variables.
#' @details The standard variables are ATX, DPXC, EWX, GGALT, LATC,m LONC, MACHX, MR, PALT, PSXC, QCSC, TASX, WDC, WSC, WIC.
#' @aliases standardVariables StandardVariables
#' @author William Cooper
#' @export standardVariables
#' @param list An optional list of variable names to add to the standard list
#' @examples 
#' standardVariables (c("WIC", "PLWCC"))
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
#' @details 'Time' is converted to a POSIXcf variable, and other variables specified in a VarList list are included in the data.frame. By default, the entire file is loaded, but optional arguments Start and End limit the time range.
#' @details If you get a long list of variable names, it probably indicates that one of the specified variables is not in this netCDF file.
#' @aliases getNetCDF getnetcdf
#' @author William Cooper
#' @import "ncdf"
#' @export getNetCDF
#' @param fname string, full file name 'e.g., "/home/Data/PREDICT/PREDICTrf01.nc"
#' @param VarList vector of variable names to load from the netCDF file
#' @param Start An optional numeric giving the desired start time in HHMMSS format
#' @param End An optional numeric giving the desired end time in HHMMSS format
#' @param F An optional numeric entered in the data.frame as a column 'FNo' all set to this integer
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
  time_units <- att.get.ncdf (netCDFfile, "Time", "units")
  tref <- sub ('seconds since ', '', time_units$value)
  Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
  SE <- getStartEnd (Time)
  print (c("File Times: From ", format(Time[1]), " to ", 
           format(Time[length(Time)])))
          # see if limited time range wanted:
  i1 <- ifelse ((Start != 0), getIndex (Time, Start), 1)
  i2 <- ifelse ((End != 0), getIndex (Time, End), length(Time))
  r <- i1:i2
  Time <- Time[r]
  d <- data.frame(Time)
  for (i in 1:NV) {
    d[VarList[i]] <- (get.var.ncdf(netCDFfile, VarList[i]))[r]
  }
  if (F != 0) {    # if specified, include the flight number
    RF <- rep (F, times=length(Time))    # label flight number
    d["RF"] <- RF[r]
  }
  close.ncdf (netCDFfile)
  d[d == -32767.] <- NA   # replace missing-value with NA
  return (d)
}


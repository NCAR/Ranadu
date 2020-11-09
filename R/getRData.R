## selectRAFdata -- select a netCDF file, load selected variables, save 
## the data.frame as .Rdata, also return the data.frame as function value.
#' @title getRData
#' @description Prepares RAF netCDF data, in data.frame, with user interaction. Returns
#' it and saves it as an .Rdata file.
#' @details For specified variables and time period, constructs a data.frame, saves it 
#' in Rdata format, and places it as a specified file. Also returns the data.frame
#' as the value of the function.
#' @aliases getRdata 
#' @author William Cooper
#' @export getRData
#' @param subsetTime FALSE by default, which causes all available times to be returned.
#' If this is TRUE, the user is asked for the time range.
#' @param newFileName FALSE by default, in which case the default name and location are
#' used to store the file (/Data/Work/FD.Rdata). If set TRUE, the user is asked for the
#' new file name. The data.frame is returned as the value of the function in any case.
#' @return The constructed data.frame, which is also stored as a .RData-format file.  
## @examples 
## \dontrun{RdataFile <- getRData ()}

getRData <- function(subsetTime = FALSE, newFileName = FALSE) {
  library(Ranadu)
  Start <- 0
  End <- 0
  fname <- setFileName()
  FI <- DataFileInfo(fname)
  VarList <- setVariableList(fname, standardVariables())
  if (subsetTime) {
    print(sprintf('file time limits are %s to %s', FI$Start, FI$End))
    x <- readline (sprintf ('CR to accept or enter two new limit times:'))
    if (nchar(x) > 1) {
      x <- sub(', *', ' ', x)
      Start <- as.integer(sub(' .*', '', x))
      End <- as.integer(sub('^.* ', '', x))
    }
  }
  FlightData <- getNetCDF(fname, VarList, Start, End)
  FDpath <- '/Data/Work/FD.Rdata'
  if (newFileName) {
    print (sprintf ('default location and name: %s', FDpath))
    x <- readline (sprintf ('CR to accept or enter new path:'))
    if (nchar(x) > 1) {
      FDpath <- x
    }
  }
  save(FlightData, file = FDpath)
  # to use the data.frame with a different name:
  # DF <- get(load(FDpath))
  # Also, the .Rdata file can be read into Python via the pyreadr package.
  return(FlightData)
}




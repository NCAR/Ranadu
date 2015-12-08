
#' @title getRAFData
#' @description Prepares RAF netCDF data, in data.frame, for FTP download
#' @details For specified variables and time period, constructs a data.frame, saves it 
#' in Rdata format, and places it on $FTP with a supplied name. This is probably not
#' generally useful but is used by the author to facilitate data transfers to a home
#' computer while reducing the size of the file that is transferred and avoiding the
#' need to do this via VPN. If $FTP does not exist, the script attempts to put the
#' file on "/net/ftp/pub/temp/users/cooperw" and will likely fail for users other
#' than the author, so it is important to set $FTP to use this routine. CAVEAT: This
#' script will overwrite the output file without warning.
#' @aliases getRAFdata 
#' @author William Cooper
#' @export getRAFData
#' @param .Rdata The name to be given to the data file placed on $FTP. Example: "Temp" 
#' to get "Temp.Rdata". Default "Temp".
#' @param .Directory The root directory for the data, e.g., /scr/raf/ProdData or 
#' /scr/raf_data (character string). Default is DataDirectory ().
#' @param .Project Project Name (e.g., "CONTRAST") -- a directory on /scr/raf_data 
#' (character string)
#' @param .Flight Flight name (e.g., "rf05") (character string)
#' @param .Start Start time in HHMMSS format (numeric); default is the first time in the file
#' @param .End   End time in HHMMSS format (numeric); default is the last time in the file
#' @param .VarList List of variables to include in the file (vector of character strings), 
#' default is the list of variables provided by 'standardVariables()'.
#' @param .F A numeric flight number to include in the data frame as 'RF'. Defaults to 0 
#' and, if 0, is not included.
#' @return The full address where the compressed Rdata file was saved, or NULL in 
#' case of failure.  
## @examples 
## \dontrun{RdataFile <- getRAFData ("PREDICTrf05", NA, "PREDICT", "rf05")}

########################
# getRAFData.R
# Use: runs only on tikal, constructs a data.frame with requested data
#      by reading from /scr/raf_data, then saves the data on $FTP
#      where it can be downloaded by FTP to an external computer.
#      The purpose is to avoid downloading the large netCDF files 
#      by downloading only the needed data.
#######################

getRAFData <- function (.Rdata="Temp", .Directory=NA, .Project, .Flight, 
                        .Start=0, .End=0, .VarList=NA, .F=0) {
  if (is.na(.Directory)) {.Directory <- DataDirectory ()}
  if (is.na(.VarList)) {
    .VarList <- standardVariables ()
  }
  fname <- sprintf ("%s/%s/%s%s.nc", .Directory, .Project, .Project, .Flight)
  d <- getNetCDF (fname, .VarList, .Start, .End, .F)
  ftp <- "/net/ftp/pub/temp/users/cooperw"
  FTPdir <- Sys.getenv ("FTP")  
  if (file.exists (FTPdir)){
    ftp <- FTPdir
  }
  save (d, file=sprintf("%s/%s.Rdata", ftp, .Rdata))
  return (sprintf("%s/%s.Rdata", ftp, .Rdata))
}

# f <- getRAFData ("PREDICTrf08", .Project="PREDICT", .Flight="rf08")
# print (f)

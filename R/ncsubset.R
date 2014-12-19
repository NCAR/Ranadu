#' @title ncsubset 
#' @description Make a new netCDF file containing a subset of an existing netCDF file. 
#' @details A smaller netCDF file is constructed by including only specified variables and a restricted time span
#' @aliases ncsubset
#' @author William Cooper
#' @export ncsubset
#' @param .OldFileName The path to the existing netCDF file.
#' @param .NewFileName The path to the new subset netCDF file. Will be silently removed if present before writing the new file.
#' @param .Start The start time (in HHMMSS format) for the new file. Default is the start time for the old file.
#' @param .End The end time (in HHMMSS format) for the new file. Default is the end time for the old file.
#' @param .VarList A vector of names of variables to include in the new file. Default is the list of variables in the old file.
#' @return NULL; the result is a new file in a specified location.
## @examples 
## \dontrun{ncsubset ("/scr/raf_data/CONTRAST/CONTRASTrf14hrt.nc", 
##     "/h/eol/cooperw/Data/CSTrf14hrt.nc", 72300, 72800, 
##     standardVariables(c("GGVNSB","GGVEWB")))}
ncsubset <- function (.OldFileName, .NewFileName, .Start=NA, .End=NA, .VarList=NULL) {
# 
# This uses ncks to extract a subset of a netCDF file, and resets the 
# global attribute TimeInterval to the new time limits. For this to work,
# it was necessary to make some changes in Xanadu, in the header routine
# and the arc routine, to truncate the TimeInterval attribute to 17
# characters; apparently ncks adds some extra ones to the end, although
# I do not understand how/why this happened. R could work with these
# files without this change; Xanadu and ncplot apparently needed this
# truncation.
# 
  
#    These were for testing:
#   .Start <- 72411
#   .End <- 72416
#   .OldFileName <- "/home/Data/CONTRAST/CONTRASTrf14.nc"
#   .NewFileName <- "/home/Data/CONTRAST/work.nc"
#   .VarList <- standardVariables()

  .Data <- getNetCDF(.OldFileName, "Time")
  SE <- getStartEnd(.Data$Time)
  if (is.na(.Start)) {.Start <- SE[1]}
  if (is.na(.End))   {.End   <- SE[2]}
  # is the file hrt?
  .HR <- ((.Data$Time[27]-.Data$Time[26]) < .5)
  IS <- getIndex(.Data$Time, .Start)
  IE <- getIndex(.Data$Time, .End)
  if (.HR) {
    IS = IS %/% 25
    IE = IE %/% 25
  }
  if (length(.VarList) == 0) {
    Cmd <- sprintf("ncks -d Time,%d,%d %s %s", IS, IE, .OldFileName, .NewFileName)
  } else {
    Cmd <- sprintf("ncks -v %s -d Time,%d,%d %s %s",  paste(list=.VarList, collapse=','), IS, IE, .OldFileName, .NewFileName)
    print(sprintf("Cmd=%s",Cmd))
    print(sprintf(" .VarList=%s", paste(list=.VarList)))
    print(sprintf(" start and end indices are %d, %d", getIndex(.Data$Time, .Start), getIndex(.Data$Time, .End)))
    print(sprintf("Cmd=%s",Cmd))
  }
  system(sprintf("rm %s", .NewFileName), wait=TRUE)
  system(Cmd, wait=TRUE)
  .netCDFfile = open.ncdf(.NewFileName, write=TRUE)
  sh <- .Start %/% 10000
  sm <- (.Start %% 10000) %/% 100
  ss <- as.integer(.Start %% 100)
  eh <- .End %/% 10000
  em <- (.End %% 10000) %/% 100
  es <- as.integer(.End %% 100)
  # may need to fix these:
  #z <- att.put.ncdf (.netCDFfile, 0, "time_coverage_start", "2014-02-11T07:23:00 +0000")
  #z <- att.put.ncdf (.netCDFfile, 0, "time_coverage_end", "2014-02-11T07:28:00 +0000")
  z <- att.put.ncdf (.netCDFfile, 0, "TimeInterval", sprintf("%02d:%02d:%02d-%02d:%02d:%02d", sh, sm, ss, eh, em, es), prec="text")
  close.ncdf(.netCDFfile)
  return()
}

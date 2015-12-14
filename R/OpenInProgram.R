#' @title OpenInProgram
#' @description Transfers a data.frame to ncplot or Xanadu for analysis
#' @details ncplot is a widely used plotting program supported by NCAR/EOL. 
#' Xanadu is a legacy package for data analysis. This function writes
#' a specified data.frame to a netCDF file and opens either ncplot or Xanadu with 
#' that file. This is a mechanism to use some of the ncplot capabilities for plots or
#' the Xanadu special capabilities like those for spectral analysis, for constructing 
#' vertical sections and sounding plots, and for producing plots using python routines 
#' with python code that can be tailored for special needs.
#' @aliases openInProgram
#' @author William Cooper
#' @export OpenInProgram
#' @param Data The data.frame containing the variables that will be transferred for
#' use in Xanadu. This data.frame should have attributes preserved from the original
#' netCDF file; otherwise, 'makeNetCDF()' which is called by this function will fail.
#' The data.frame must always contain the Time variable but can contain only a subset 
#' of the variables and times in the original file. The function Ranadu::RSubset() can
#' be used to construct subsets while preserving attributes.
#' @param Program A character string with the name of the program. Default is "ncplot".
#' @param dataDirectory A directory into which the netCDF file containing data from
#' this data.frame will be written. The default will be a subdirectory named 'R'
#' in the main data directory specified by Ranadu::DataDirectory().
#' @param netCDFfileName The character string representing a name for the
#' netCDF data file that will be created. The file will be created in 'dataDirectory'.
#' @param warnOverwrite A logical variable that can be set FALSE to suppress warnings
#' about overwriting existing data files. Overwriting will then occur silently.
#' @param openProgram A logical parameter, with default of TRUE, that will open an X window
#' for access to Xanadu routines. If FALSE, Xanadu will not be started but system calls
#' like 'system("Xanadu tracy")' can be used then to process the new netCDF file in
#' batch mode.
#' @return None
#' @examples 
#' \dontrun{OpenInProgram (RAFdata, "ncplot", ".", "RAFdata4Xanadu")}

OpenInProgram <- function(Data, Program="ncplot", dataDirectory=sprintf("%s/R", DataDirectory()), 
                         netCDFfileName='RtoXanadu', warnOverwrite=TRUE, openProgram=TRUE) {

  nF <- sprintf("%s/%s.nc", dataDirectory, netCDFfileName)
  if (file.exists (nF)) {
    if (warnOverwrite) {
      x <- readline ("file exists: OK to overwrite? (Y/n): ")
      if (x[1] != 'Y' && x[1] != 'y') {
        return("no action, rejected overwriting data file")
      }
    }
    unlink (nF)
  }
  Z <- makeNetCDF (Data, nF)
  if ((Program == 'ncplot') && openProgram) {
    system (sprintf ("ncplot %s", nF), wait=FALSE)
  } else {
  ## edit the .def files for the Xanadu call
  WD <- getwd()
  setwd ("~/Xanadu/R")
  lines <- readLines ("Xanadu.def")
  newlines <- vector ("character")
  for (line in lines) {
    if (grepl ("XANFILE", line)) {
      line <- gsub ("=.*", sprintf ("=%s", gsub ("\\.nc", '', netCDFfileName)), line)
    }
    newlines[length (newlines) + 1] <- line
    if (grepl ("XANDATA", line)) {
      line <- gsub ("=.*", sprintf ("=%s", dataDirectory), line)
    }
    newlines[length (newlines) + 1] <- line
  }
  writeLines (newlines, "Xanadu.def")
  if (openProgram) {system ("Xanadu", wait=FALSE)}
  setwd(WD)
  }
}

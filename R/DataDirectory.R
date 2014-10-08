#' @title DataDirectory
#' @description Returns the location of the local data directory. 
#' @details This is intended to make it easier to write programs that work on different computers where the netCDF files by convention are stored in different locations. It returns the local location, or NA if none of the standard locations are found.
#' @aliases DataDirectory
#' @author William Cooper
#' @export DataDirectory
#' @return Directory The path to the local data directory, or NA if none found
#' @examples 
#' Directory <- DataDirectory ()
DataDirectory <- function() {
  
  # provides a local data location that varies with system:
  #    tikal: /scr/raf_data/ 
  #    laptop: /Data/
  #    LookoutHaven: /home/Data/
  # add others as needed
  
  DataDir <- "/scr/raf_data/"
  if (file.exists(DataDir)){
    return(DataDir)
  }
  DataDir <- "/Data/"
  if (file.exists(DataDir)){
    return(DataDir)
  }
  DataDir <- "/home/Data/"
  if (file.exists(DataDir)){
    return(DataDir)
  }
  return(NA)
} 
#' @title TellAbout
#' @description Utility to show characteristics of a variable
#' @details Prints the variable type, dimensions, and summary.
#' @aliases TellAbout tellAbout
#' @author William Cooper
#' @export TellAbout
#' @param V A variable that may be scalar, vector, data.frame
#' @return The summary, which is also printed by the function.
#' @examples 
#' \dontrun{TellAbout("TASX")}
TellAbout <- function (V) {
  print(c(sprintf("Variable class is %s, length = %d, dim = ", class(V), length(V)), dim(V)))
  if (is.vector(V)) {
    print(sprintf("Variable rms = %g", sd(V, na.rm=TRUE))) 
  }  
  print (summary(V))
}

#' @title ValueOf
#' @description Returns value of a variable at a specified time
#' @details In a dataframe, finds the index corresponding to
#' the specified time and returns the value of the specified
#' variable at that index.
#' @aliases ValueOf
#' @author William Cooper
#' @export ValueOf
#' @param Variable A variable in a dataframe named Data that also includes Time. The
#' variable can be in the form Data$TASX or, if Data is attached,
#' TASX.
#' @param HHMMSS A time in hour-minute-second format (e.g., 134513) 
#' @param DataFrame The dataframe containing Variable
#' @return The value of the supplied variable at the specified time.
#' @examples 
#' \dontrun{x <- ValueOf (ATX, 140233)}
ValueOf <- function(Variable, HHMMSS, DataFrame=Data) {
  return (Variable[getIndex(DataFrame$Time, HHMMSS)])
}

#' @title ValueOfAll
#' @description See/Get values of dataframe variables at specified time. 
#' @details Returns and prints values of all variables in a specified
#' dataframe that includes Time, at the time specified.
#' @aliases ValueOfAll
#' @author William Cooper
#' @export ValueOfAll
#' @param DataFrame The specified dataframe, which must contain Time as
#' a POSIXgt variable.
#' @param HHMMSS The time, in hour-minute-second format (e.g., 140321) 
#' at which to report the variables in the dataframe.
#' @return A vector containing all the variables at the specified
#' time. These values are also printed.
#' @examples 
#' \dontrun{vlist <- ValueOfAll (Data, 143329)}
ValueOfAll <- function (DataFrame=Data, HHMMSS) {
  print (DataFrame[getIndex(DataFrame$Time, HHMMSS),])
  return (DataFrame[getIndex(DataFrame$Time, HHMMSS),])
}

#' @title GetAttributes
#' @description List the netCDF attributes for a specified variable.
#' @details Prints the netCDF attributes and their values, and returns
#' a text vector with the names of the attributes, for a specified
#' netCDF file and variable. The name of the variable should be
#' specified as a string.
#' @aliases GetAttributes
#' @author William Cooper
#' @export GetAttributes
#' @param fname The name of the netCDF file.
#' @param vname The name of a variable in the netCDF file. This should
#' be specified as a string variable (e.g., "TASX")
#' @return A list of the names of the attributes
#' @examples 
#' \dontrun{attr <- GetAttributes ("/src/raf_data/Projectrf01.nc", "WDC"}
GetAttributes <- function (fname=fname, vname) {
  rPython::python.load ("/home/cooperw/RStudio/Ranadu/getAttributes.py")
  if (typeof (vname) != "character") {
    print ("Usage of GetAttributes: Must provide a character string for vname")
    return (NULL)
  } else {
    attr <- rPython::python.call ("getAttributes", fname, vname)
  }
  return (attr)
}  
  


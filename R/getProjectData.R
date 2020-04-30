#' @title getProjectData
#' @description Given a project name, this function constructs a data.frame
#' containing all research flights and, optionally, test and ferry flights,
#' for the project. 
#' @details Individual flights are distinguished by RF, which is the research
#' flight number or the test flight number + 50 or the ferry flight number + 100.
#' Only the specified variables are included unless .Variables == 'ALL', in which
#' case all are included (but the resulting data.frame may be unmanageably large).
#' A size limit is imposed by maxMemory, and data.frame construction stops if
#' this size is exceeded. Variables have a "label" attribute added that provides
#' an appropriate identifying label, with units, where possible.
#' @aliases getProjectData
#' @author William Cooper
#' @export getProjectData
#' @param .Project The name of the project, for which data files are assumed
#' stored in DataDirectory()/Project with names like Projectrf01.nc. If this is
#' not the desired location, the next parameter can be used to specify the
#' directory. There is no default, so omission of the project name generates an error.
#' @param .Variables A list of variables to be loaded. "ALL" loads everything; 
#' the default is the set of variables specified by 'standardVariables()'. If
#' the variable does not exist in the data.file an error will be reported and
#' instead an all-missing-value variable with that name will be included.
#' @param DataDir The full path to the data directory. The default is the path
#' obtained by DataDirectory().
#' @param Flights An optional vector of integers denoting the research flights to include.
#' The default is to include all research flights.
#' @param .Test If TRUE, test flights will be included, identified by flight numbers
#' starting at 51. The default is FALSE.
#' @param .Ferry If TRUE, ferry flights will be included, identified by flight
#' numbers starting at 101. The default is FALSE.
#' @param maxMemory The cutoff at which data.frame construction will be suspended to
#' avoid very large data.frame generation. The default is 1000000000. When the data.frame
#' exceeds this size, subsequent requested flights are skipped.
#' @return A new data.frame containing all the flights.
getProjectData <- function (.Project, .Variables = standardVariables(), 
  DataDir=DataDirectory(), Flights=NA, .Test=FALSE, .Ferry=FALSE, maxMemory=1000000000) {
  .Data <- data.frame()
  PD <- .Project
  if (DataDir != DataDirectory()) {
    PD <- ''
  }
  # print (sprintf ('DataDir=%s, .Project=%s', DataDir, .Project))
  Fl <- sort (list.files ( ## get list of available flights
    sprintf ("%s%s/", DataDir, PD),
    sprintf ("%srf...nc$",.Project)))
  if (.Test) {
    Fl <- c(Fl, sort (list.files ( ## get list of available flights
      sprintf ("%s%s/", DataDir, PD),     
      sprintf ("%stf...nc$", .Project)))
    )
  }
  if (.Ferry) {
    Fl <- c(Fl, sort (list.files ( ## get list of available flights
      sprintf ("%s%s/", DataDir, PD),     
      sprintf ("%sff...nc$", .Project)))
    )
  }

  First <- TRUE
  for (flt in Fl) {
    fname = sprintf("%s%s/%s", DataDir, PD, flt)
    if (as.numeric (object.size(.Data)) > maxMemory) {
      print (sprintf ("Warning: data.frame size > %d; skipping %s and subsequent files", maxMemory, fname))
      print ('Change the maxMemory limit if you really want a larger file.')
      break
    }
    fno <- as.numeric(sub('.*f([0-9]*).nc', '\\1', flt))
    if (!is.na(Flights) && (!(fno %in% Flights))) {next}
    if (grepl ('tf', fname)) {
      fno <- fno + 50
    } else if (grepl ('ff', fname)) {
      fno <- fno + 100
    }
    FI <- DataFileInfo (fname, LLrange=FALSE)
    if (.Variables == 'ALL' || .Variables == 'all' || .Variables == 'All') {
      if (First) {
        .Var <- .Variables <- FI$Variables
        First <- FALSE    ## Use consistent set; don't reset for each flight
      }
    } else {
      .Var <- .Variables[.Variables %in% FI$Variables]  ## Request only those present
    }
    if (interactive()) {
      print (sprintf ('loading netCDF file %s', fname))
    }
    D <- getNetCDF (fname, .Var, F=fno)
    ## Now add all-missing vectors for requested variables not present:
    .V <- .Variables [!(.Variables %in% .Var)]
    if (length(.V) > 0) {
      for (V in .V) {
        print (sprintf ('requested variable %s not found for file %s; all-NA loaded', V, fname))
        D[, V] <- rep(NA, nrow(D))
      }
    }
    .Data <- rbind(.Data, D)
    # print (sprintf ('data.frame size is %s', object.size(.Data)))
  }
  ## Add appropriate labels to the variable attributes:
  .Data <- addLabels(.Data)
  return(.Data)
}

# for (V in names(Data)) {
#   if (!is.null (sname <- attr(Data[,V], 'standard_name'))) {
#     print (sprintf ('Variable %s has standard_name %s', V, sname))
#     attr(Data[,V], 'measurand') <- sname
#   }
# }

# for (V in names(Data)) {
#   print (sprintf ('variable %s label %s', V, attr(Data[,V], 'label')))
# }

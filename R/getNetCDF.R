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
#' @import ncdf4
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
  namesCDF <- names (netCDFfile$var)
  for (V in VarList) {
    if (is.na(V)) {next}
    if (length (which (grepl (V, namesCDF)))) {next}
    cat (sprintf ("requested variable %s not in netCDF file;\n ---->ngetNetCDF returning with error", V))
    return (-1)
  }
  Time <- ncvar_get (netCDFfile, "Time")
  DL <- length (Time)
  ## set the maximum data rate (but not above 100 Hz):
  Rate <- 1
  nms <- names(netCDFfile$dim)
  if ("sps25" %in% nms) {Rate <- 25}
  if ("sps50" %in% nms) {Rate <- 50}
  if ("sps100" %in% nms) {Rate <- 100}
  # print (sprintf ("output rate for this data.frame is %d", Rate))
  # Expand Time to be high-rate
  if (Rate > 1) {
    T <- vector ("numeric", Rate*length(Time))
    for (i in 1:length(Time)) {
      for (j in 0:(Rate-1)) {
        T[(i-1)*Rate+j+1] <- Time[i]+1/Rate*j
      }  
    }
    Time <- T
  }
  time_units <- ncatt_get (netCDFfile, "Time", "units")
  tref <- sub ('seconds since ', '', time_units$value)
  Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
  # see if limited time range wanted:
  i1 <- ifelse ((Start != 0), getIndex (Time, Start), 1)
  i2 <- ifelse ((End != 0), getIndex (Time, End) + Rate - 1, length (Time))
  # if (End != 0) {
  #   i2 <- getIndex (Time, End) + Rate - 1
  # } else {
  #   i2 <- length (Time)
  # }
  r <- i1:i2
  # r is the appropriate index for any rate, but also need
  # the 1-Hz index for extrapolation:
  r2 <- ((i1-1)/Rate+1):((i2-1)/Rate+1)
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
  attr (d, "R_dataframe_created") <- date()    # add one global attribute
  
  ######------------------------------------------------------------------
  IntFilter <- function (X, inRate, outRate) {
    if (inRate == outRate) {return (X)}
    ratio <- as.integer(outRate/inRate)    ## expected to be an integer
    DL <- length (X) / inRate
    x <- 0:(length(X)-1)
    A <- approx (x, X, n=DL*outRate/inRate)
    T <- A$y
    T <- signal::filter(signal::sgolay(4,75),T)
    return (T)
  }
  ######------------------------------------------------------------------
  
  ## Add the requested variables:------------------------------------------------
  for (V in VarList) {
    if (is.na(V)) {next}
    ## fill in location-tag for variable name if needed:
    if (substr(V, nchar(V), nchar(V)) == '_') {
      for (ncn in namesCDF) {
        if (grepl (V, ncn)) {V <- ncn}
      }
    }
    ## save dimensions for the variable:
    datt <- list()
    for (dd in netCDFfile$var[[V]]$dim) {
      datt[[length(datt)+1]] <- dd$name
    }    ## later, save datt as an attribute of V
    X <- ncvar_get (netCDFfile, V)
    ATT <- ncatt_get (netCDFfile, V)
    ## for Rate == 1, nothing special is needed:
    if (Rate == 1) {
      X <- X[r2]
    } else { ## other rates require flattening and possibly interpolation and filtering
      DM <- length(dim(X))           
      if (DM == 2) {    # flatten
        X <- X[,r2]
        inputRate <- dim(X)[1]
        dim(X) <- dim(X)[1]*dim(X)[2]
        ## see if adjustment to max rate is needed
        if (dim(X)[1] != Rate) {X <- IntFilter(X, inputRate, Rate)}
      } else {  ## single-dimension (1 Hz) in high-rate file
        X <- X[r2]
        X <- IntFilter (X, 1, Rate)
      }
    } 
    ## add variable attributes as in netCDF file
    for (A in names (ATT)) {
      attr (X, A) <- ATT[[A]]
    }
    attr (X, "Dimensions") <- datt
    d[V] <- X
  }
  if (F != 0) {    # if specified, include the flight number
    RF <- rep (F, times=length(Time))    # label flight number
    d["RF"] <- RF
  }
  nc_close (netCDFfile)
  return (d)
}

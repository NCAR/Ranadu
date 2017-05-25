#' @title makeNetCDF
#' @description  Write a netCDF file using data from a data.frame
#' @details This function writes a netCDFfile that includes all variables in a data.frame.
#' It will not overwrite an existing file, so the file 'newNetCDFname' should be deleted
#' before a call to makeNetCDF if overwriting is desired.
#' For this routine to work effectively, the data.frame should be produced by a call to getNetCDF()
#' because that function also assigns attributes to the data.frame and to the variables it
#' contains that match those in the original netCDF file. This function extracts those
#' attributes and adds them to the netCDF file that is created. Note that, when subsetting,
#' it will be necessary to preserve the attributes; see help for getNetCDF. The code
#' includes an inactive function transferAttributes() for this purpose. At present, this
#' will work for low-rate and 25-Hz variables but not for multi-dimensional variables like
#' the hydrometeor size distributions.
#' @aliases MakeNetCDF
#' @author William Cooper
#' @export makeNetCDF
#' @import ncdf4
#' @param .data A data.frame produced by getNetCDF() or otherwise converted to the corresponding
#' structure from that function.
#' @param newNetCDFname A character path name for the file to be created
#' @return a text message indicating successful creation of the new file.
#' @examples 
#' makeNetCDF (RAFdata, "RAFdata.nc")
#' \dontrun{makeNetCDF (Data, "./newFile.nc")}

makeNetCDF <- function (.data, newNetCDFname) {
  if (file.exists (newNetCDFname)) {
    cat (sprintf ("file %s exists. \nmakeNetCDF won't overwrite; quitting\n", newNetCDFname))
    return ("makeNetCDF returned without file creation")
  }
  # saveFrame <- sub('.*_', '_', tempfile('_WAC'))
  # assign(saveFrame, .data, envir = .GlobalEnv)  ## save the data.frame
  ## modify the data.frame, hoping to force a copy:
  .d <- .data
  attr (.d, "R_dataframe_date") <- date()
  ## get the dimensions
  Dimensions <- attr(.d, "Dimensions")
  if (is.null(Dimensions)) {
    print ("error in makeNetCDF: No Dimensions in data.frame")
    return (-1)
  }
  ## fix missing vals in Time dimension:
  tref <- sub ("seconds since ", "", Dimensions$Time$units)
  tstart <- as.integer(difftime (.d$Time[1], tref, units='secs', tz='UTC'))
  tend   <- as.integer(difftime (.d$Time[nrow(.d)],tref, units='secs', tz='UTC'))
  ## search for gaps in time:
  # r <- NULL
  # if (tend-tstart+1 > nrow (.d)) {
  #   for (i in 2:nrow(.d)) {
  #     if ((.d$Time[i] - .d$Time[i-1]) > 1) {
  #       print (sprintf ('i=%d, Time=%s, next is %s', i, .d$Time[i-1], .d$Time[i]))
  #       r1 <- as.integer (difftime (.d$Time[i-1], tref, units='secs', tz='UTC')) + 1
  #       r2 <- as.integer (difftime (.d$Time[i], tref, units='secs', tz='UTC')) - 1
  #       r <- c(r, r1:r2)
  #     }
  #   }
  # }
  # Dimensions[["Time"]]$vars <- tstart:tend
  if (tend-tstart+1 > nrow(.d)) {
    tt <- vector ('integer', nrow(.d))
    for (i in 1:nrow(.d)) {
      tt[i] <- as.integer (difftime (.d$Time[i], tref, units='secs', tz='UTC'))
    }
  } else {tt <- tstart:tend}
  Dimensions[["Time"]]$len <- nrow(.d)
  ## must redefine Time to get it to be integer as is convention in RAF netCDF
  Dimensions[["Time"]] <- ncdim_def ("Time", Dimensions[["Time"]]$units,
                                        vals=tt, create_dimvar=TRUE)
  HR <- 0
  if ("sps25" %in% names (Dimensions)) {
    HR <- 25
    Dim <- list(Dimensions[["sps25"]], Dimensions[["Time"]])
  } else if ("sps50" %in% names (Dimensions)) {
    HR <- 50
    Dim <- list(Dimensions[["sps50"]], Dimensions[["Time"]])
  }
  vdef <- list()   # start with empty list, add variables to it
  for (V in names(.d)) {
    if (V == "Time") {next}
    var_units <- attr (eval (parse (text=sprintf (".d$%s", V))), "units")
    if (is.null(var_units)) {var_units <- "not defined"}
    if (HR > 1) {
      vd <- ncvar_def (V,
                 units=var_units,
                 dim=Dim, missval=as.integer(-32767), prec='float')
    } else {
      vd <- ncvar_def (V, 
                units=var_units, 
                Dimensions[["Time"]], missval=as.integer(-32767), prec='float')
    }
    vdef[[length(vdef)+1]] <- vd
  }
  nc <- nc_create (newNetCDFname, vdef)
  ## global attributes
  ##  (but skip row.names, names, Dimensions, class)
  ATG <- attributes (.d)
  for (i in 1:length(ATG)) {
    ATT <- ATG[i]
    if (grepl ("row.names", names(ATT)))   {next}
    if (grepl ("names", names (ATT)))      {next}
    if (grepl ("Dimensions", names (ATT))) {next}
    if (grepl ("class", names (ATT)))      {next}
    ## fix the time interval
    if (grepl ("TimeInterval", names (ATT))) {
      Start <- as.POSIXlt (.d$Time[1])
      End <- as.POSIXlt (.d$Time[nrow(.d)])
      av <- sprintf("%02d:%02d:%02d-%02d:%02d:%02d", Start$hour, Start$min, 
                    as.integer(Start$sec), End$hour, End$min, as.integer (End$sec))
      ATT[[1]] <- av
    }
    if (is.numeric (ATT[[1]])) {
      ncatt_put (nc, 0, attname=names(ATT), attval=as.numeric (ATT))
    } else {
      ncatt_put (nc, 0, attname=names(ATT), attval=as.character(ATT))
    }
  }
  nc_redef (nc)
  for (V in names(.d)) {
    ATV <- attributes (eval (parse (text=sprintf (".d$%s", V))))
    for (i in 1:length(ATV)) {
      ATT <- ATV[i]
      aname <- names(ATT)
      if (length (ATT) < 1) {
        ncatt_put (nc, V, attname="_FillValue", attval=as.integer(-32767), definemode=TRUE)
        next
      }
      if ("dim" == aname) {next}
      if ("Dimensions" == aname) {next}
      if ("units" == aname) {next}
      if ("class" == aname) {next}
      if ("tzone" == aname) {next}
      if ("actual_range" == aname) {next}
      if ("_FillValue" == aname) {
        ncatt_put (nc, V, attname=aname, attval=as.integer(ATT), definemode=TRUE)
      } else {
        avalue <- as.character (ATT)
        ncatt_put (nc, V, attname=aname, attval=avalue, definemode=TRUE)
      }
    }
  }
  nc_enddef (nc)
  ## revise time to be RAF-netCDF convention:
  dT <- as.integer (.d$Time - as.integer(as.POSIXct(strptime (attr (.d$Time, "units"), 
                                                     attr (.d$Time, "strptime_format"), 
                                                     tz="UTC"))))
  #HR <- ("sps25" %in% names (Dimensions))
  if (HR == 25) {
    dT <- dT[seq(1, nrow(.d), by=25)]
  } else if (HR == 50) {
    dT <- dT[seq(1, nrow(.d), by=50)]
  }
  for (V in names (.d)) {
    if (V == "Time") {
      ncvar_put (nc, V, dT, count=length(dT))
    } else {
      A <- eval(parse(text=sprintf(".d$%s", V)))
      A[is.na(A)] <- -32767
      if (HR == 25) {
        ncvar_put (nc, V, A, count=c(25, nrow(.d)/25))
      } else if (HR == 50) {
        ncvar_put (nc, V, A, count=c(50, nrow(.d)/50))
      } else {
        Vvalue <- eval (parse (text=sprintf ('.d$%s', V)))
        Vvalue[is.na(Vvalue)] <- -32767
        ncvar_put (nc, V, Vvalue)
      }
      ## Note: strangely, the preceding puts -32767 into .d data.frame, so fix it,
      ## although I don't understand why this is necessary
      .data[!is.na(.data[,V]) & abs (.data[,V]+32767) < 1, V] <- NA
    }
  }
  nc_close (nc)
  return (sprintf ("wrote file %s", newNetCDFname))
}

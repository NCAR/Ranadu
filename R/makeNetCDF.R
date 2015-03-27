#' @title makeNetCDF
#' @description  Write a netCDF file using data from a data.frame
#' @details This function writes a netCDFfile that includes all variables in a data.frame.
#' It will not overwrite an existing file, so the file 'newNetCDFname' should be deleted
#' before a call to makeNetCDF if overwriting is desired.
#' For this routine to work effectively, the data.frame should be produced by a call to getNetCDF()
#' because that function also assigns attributes to the data.frame and to the variables it
#' contains that match those in the original netCDF file. This function extracts those
#' attributes and adds them to the netCDF file that is created. Note that, when subsetting,
#' it will be necessary to preserve the attributes; see help for getNetCDF. At present, this
#' will work for low-rate and 25-Hz variables but not for multi-dimensional variables like
#' the hydrometeor size distributions.
#' @aliases makeNetCDF
#' @author William Cooper
#' @export makeNetCDF
#' @param d A data.frame produced by getNetCDF() or otherwise converted to the corresponding
#' structure from that function.
#' @param newNetCDFname A character path name for the file to be created
#' @return none The result is the file on disk
#' @examples 
#' \dontrun{makeNefCDF (Data, "./newFile.nc")}

makeNetCDF <- function (d, newNetCDFname) {
  if (file.exists (newNetCDFname)) {
    cat (sprintf ("file %s exists. \nmakeNetCDF won't overwrite; quitting\n", newNetCDFname))
    return ("makeNetCDF returned without file creation")
  }
  ## get the dimensions
  Dimensions <- attr(d, "Dimensions")
  if (is.null(Dimensions)) {
    print ("error in makeNetCDF: No Dimensions in data.frame")
    return (-1)
  }
  ## fix missing vals in Time dimension:
  tref <- sub ("seconds since ", "", Dimensions[["Time"]]$units)
  tstart <- as.integer(difftime (d$Time[1], tref, units='secs', tz='UTC'))
  tend   <- as.integer(difftime (d$Time[nrow(d)],tref, units='secs', tz='UTC'))
  Dimensions[["Time"]]$vars <- tstart:tend
  Dimensions[["Time"]]$len <- tend - tstart + 1
  ## must redefine Time to get it to be integer as is convention in RAF netCDF
  Dimensions[["Time"]] <- dim.def.ncdf ("Time", Dimensions[["Time"]]$units,
                                        vals=tstart:tend, create_dimvar=TRUE)
  if (HR <- "sps25" %in% names (Dimensions)) {
    Dim <- list(Dimensions[["sps25"]], Dimensions[["Time"]])
  }
  vdef <- list()   # start with empty list, add variables to it
  for (V in names(d)) {
    if (V == "Time") {next}
    if (HR) {
      vd <- var.def.ncdf (V,
                 units=attr (eval (parse (text=sprintf ("d$%s", V))), "units"),
                 dim=Dim, missval=as.single(-32767.), prec='single')
    } else {
      vd <- var.def.ncdf (V, 
                units=attr (eval (parse (text=sprintf ("d$%s", V))), "units"),
                Dimensions[["Time"]], missval=-32767., prec='single')
    }
    vdef[[length(vdef)+1]] <- vd
  }
  ncdf <- create.ncdf (newNetCDFname, vdef)
  ## global attributes
  ##  (but skip row.names, names, Dimensions, class)
  ATG <- attributes (d)
  for (i in 1:length(ATG)) {
    ATT <- ATG[i]
    if (grepl ("row.names", names(ATT)))   {next}
    if (grepl ("names", names (ATT)))      {next}
    if (grepl ("Dimensions", names (ATT))) {next}
    if (grepl ("class", names (ATT)))      {next}
    ## fix the time interval
    if (grepl ("TimeInterval", names (ATT))) {
      Start <- as.POSIXlt (d$Time[1])
      End <- as.POSIXlt (d$Time[nrow(d)])
      av <- sprintf("%02d:%02d:%02d-%02d:%02d:%02d", Start$hour, Start$min, 
                    as.integer(Start$sec), End$hour, End$min, as.integer (End$sec))
      ATT[[1]] <- av
    }
    if (is.numeric (ATT[[1]])) {
      att.put.ncdf (ncdf, 0, attname=names(ATT), attval=as.numeric (ATT))
    } else {
      att.put.ncdf (ncdf, 0, attname=names(ATT), attval=as.character(ATT))
    }
  }
  redef.ncdf(ncdf)
  for (V in names(d)) {
    ATV <- attributes (eval (parse (text=sprintf ("d$%s", V))))
    for (i in 1:length(ATV)) {
      ATT <- ATV[i]
      if ("dim" == names (ATT)) {next}
      if ("Dimensions" == names (ATT)) {next}
      if ("units" == names (ATT)) {next}
      if ("class" == names (ATT)) {next}
      if ("tzone" == names (ATT)) {next}
      aname <- names(ATT)
      avalue <- as.character (ATT)
      att.put.ncdf (ncdf, V, attname=aname, attval=avalue, definemode=TRUE)
    }
  }
  enddef.ncdf (ncdf)
  ## revise time to be RAF-netCDF convention:
  dT <- as.integer (d$Time - as.integer(as.POSIXct(strptime (attr (d$Time, "units"), 
                                                     attr (d$Time, "strptime_format"), 
                                                     tz="UTC"))))
  transferAttributes <- function (dsub, d) {    ## unused function, just saved here
    ds <- dsub
    for (nm in names (ds)) {
      var <- sprintf ("d$%s", nm)
      A <- attributes (eval (parse (text=var)))
      A$dim <- NULL
      A$class <- NULL
      attributes (ds[,nm]) <- A
    }
    return(ds)
  }
  #HR <- ("sps25" %in% names (Dimensions))
  if (HR) {
    dT <- dT[seq(1, length(d), by=25)]
  }
  for (V in names (d)) {
    if (V == "Time") {
      put.var.ncdf (ncdf, V, dT, count=length(dT))
    } else {
      if (HR) {
        put.var.ncdf (ncdf, V, eval (parse (text=sprintf ("d$%s", V))), count=c(25, nrow(d)/25))
      } else {
        put.var.ncdf (ncdf, V, eval (parse (text=sprintf ("d$%s", V))))
      }
    }
  }
  close.ncdf (ncdf)
  return (sprintf ("wrote file %s", newNetCDFname))
}

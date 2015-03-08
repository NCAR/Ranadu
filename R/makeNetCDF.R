#' @title makeNetCDF
#' @description  Write a netCDF file using data from a data.frame
#' @details This function writes a netCDFfile that includes all variables in a data.frame.
#' For this to work effectively, the data.frame should be produced by a call to getNetCDF()
#' because that function also assigns attributes to the data.frame and to the variables it
#' contains that match those in the original netCDF file. This function extracts those
#' attributes and adds them to the netCDF file that is created. Note that, when subsetting,
#' it will be necessary to preserve the attributes; see help for getNetCDF.
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
  ## get the dimensions
  Dimensions <- attr(d, "Dimensions")
  if (is.null(Dimensions)) {
    print ("error in makeNetCDF: No Dimensions in data.frame")
    return (-1)
  }
  ## fix missing vals in Time dimension:
  tref <- sub ("seconds since ", "", Dimensions[1]$Time$units)
  tstart <- as.integer(difftime (d$Time[1], tref, units='secs', tz='UTC'))
  tend   <- as.integer(difftime (d$Time[nrow(d)],tref, units='secs', tz='UTC'))
  Dimensions[1]$Time$vars <- tstart:tend
  vdef <- list()
  for (V in names(d)) {
    if (V == "Time") {next}
    vd <- var.def.ncdf (V, 
                  units=attr (eval (parse (text=sprintf ("d$%s", V))), "units"),
                  Dimensions[1], missval=-32767)
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
    att.put.ncdf (ncdf, 0, attname=names(ATT), attval=as.character(ATT))
    # print (sprintf ("%s %s", names (ATT), ATT))
  }
  redef.ncdf(ncdf)
  for (V in names(d)) {
    ATV <- attributes (eval (parse (text=sprintf ("d$%s", V))))
    for (i in 1:length(ATV)) {
      ATT <- ATV[i]
      if ("dim" == names (ATT)) {next}
      if ("Dimensions" == names (ATT)) {next}
      if ("units" == names (ATT)) {next}
      aname <- names(ATT)
      avalue <- as.character (ATT)
      att.put.ncdf (ncdf, V, attname=aname, attval=avalue, definemode=TRUE)
    }
  }
  enddef.ncdf (ncdf)
  for (V in names (d)) {
    put.var.ncdf (ncdf, V, eval (parse (text=sprintf ("d$%s", V))))
  }
  close.ncdf (ncdf)
  return ()
}

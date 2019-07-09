#' @title selectTime
#' @description Given a RANADU-convention data.frame or tibble, this function
#' returns a subset in a specified range of times.
#' @details The function uses the routine setRange but, instead of returning
#' a set of indices to use when subsetting the data.frame, it returns a
#' new subsetted data.frame. This is suitable for use in pipes like
#' D %>% selectTime(Start, End) %>% dplyr::select(Time, ATX) %>% plotWAC()
#' @aliases selectTime, selectTimes
#' @author William Cooper
#' @export selectTime
#' @param .d A data.frame or tibble that follows RANADU conventions. It
#' should contain a POSIXCT-format variable named "Time".
#' @param StartTime An initial time in HHMMSS format (e.g., 124500). The
#' default is 0.
#' @param EndTime An ending time in HHMMSS format. Default = 400000.
#' @return A subset tibble or data.frame restricted to the specified 
#' time range. Limits are inclusive.
#' @example DS <- selectTime(RAFdata, 201100, 201200)
selectTime <- function (.d, StartTime, EndTime) {
  dt <- .d[setRange(.d, StartTime, EndTime),]
  return(transferAttributes(.d, dt))
}

#' @title transferAttributes
#' @description Preserves data.frame attributes.
#' @details Given a RANADU-convention reference data.frame or tibble 
#' and a second (usually subset) data.frame, this function transfers the 
#' attributes ro variables in the second from the same variables in the
#' first.  This avoids the loss of attributes that often occurs when 
#' subset data.frames are constructed.
#' @aliases TransferAttributes, transferAttributes
#' @author William Cooper
#' @export transferAttributes
#' @param d A data.frame or tibble that follows RANADU conventions. 
#' @param dsub A second data.frame or tibble, often a subset of the first,
#' with variables that all appear in the first.
#' @return A modified tibble or data.frame with attributes added to match those
#' in the original data.frame. 
transferAttributes <- function (d, dsub) {  
  ds <- dsub
  ## ds and dsub contain the new variables or the subset data.frame; 
  ## d is the original, which is not modified.
  for (nm in names (ds)) {
    var <- sprintf ("d$%s", nm)
    A <- attributes (eval (parse (text=var)))
    if (!grepl ('Time', nm)) {
      A$dim[1] <- nrow(ds)
      A$class <- NULL
    } else {
      A$dim <- nrow (ds)
    }
    attributes (ds[,nm]) <- A
  }
  A <- attributes (d)
  A$Dimensions$Time$len <- nrow (ds)
  A$row.names <- 1:nrow (ds)
  A$names <- names (ds)
  attributes (ds) <- A
  return(ds)
}

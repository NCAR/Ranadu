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
  transferAttributes <- function (d, dsub) {  
    ds <- dsub
    ## ds and dsub are the new variables; d is the original
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
  
  dt <- .d[setRange(.d, StartTime, EndTime),]
  return(transferAttributes(.d, dt))
}

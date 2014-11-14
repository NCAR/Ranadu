#' @title getIndex
#' @description Returns the index in 'Time' matching HHMMSS
#' @details Find the index in a POSIXct time variable that corresponds to a specified time in HHMMSS format.
#' @aliases getIndex
#' @author William Cooper
#' @export getIndex
#' @param Time A POSIXct-format vector
#' @param HHMMSS An integer representing time in HHMMSS format 
#' @return A numeric index in the Time vector
#' @examples 
#' \dontrun{index <- getIndex (Time, 142503)}
getIndex <- function (Time, HHMMSS) {
  # This function returns the index in Time corresponding to HHMMSS,
  # where Time should be POSIXct-format and HHMMSS an integer.
  idx = 1:length(Time)
  # is this a hrt file?
  #.HR <- ((Time[27]-Time[26]) < .5)
  t <- as.POSIXlt(Time[1], tz="UTC", origin="1970-01-01")
  hour1 <- t$hour
  t$hour <- as.integer(HHMMSS/10000)
  t$min <- as.integer((HHMMSS%%10000)/100)
  t$sec <- as.integer(HHMMSS%%100)
  tc <- as.POSIXct(t, tz='UTC')
  tr <- as.POSIXct(Time, tz="UTC", origin="1970-01-01")
  if (t$hour < hour1) {
    tc <- tc + 86400
  }
  index <- idx[abs(tr-tc) < 0.02]
  return (index)
}

#' @title getStartEnd
#' @description Find the start and end times of 'Time' 
#' @details Returns a two-element vector of the start and end times in 'Time'
#' @aliases getStartEnd getstartend 
#' @author William Cooper
#' @export getStartEnd
#' @param Time A POSIXct format vector
#' @return c(Start_Time, End_Time), a numeric 2-element vector in HHMMSS format
#' @examples 
#' \dontrun{SE <- getStartEnd (Time)}
getStartEnd <- function (Time) {
# Function to return the start and end times in a 2-element
# vector in HHMMSS format
  Tlt <- as.POSIXlt (Time[1], tz="UTC", origin="1970-01-01")
  StartTime = Tlt$hour*10000+Tlt$min*100+Tlt$sec
  Tlt <- as.POSIXlt (Time[length(Time)], tz="UTC", origin="1970-01-01")
  EndTime = Tlt$hour*10000+Tlt$min*100+Tlt$sec
  return(c(StartTime, EndTime))
}

#' @title setRange
#' @description Set the index range based on requested time limits
#' @details For time variable Time, finds indices that match the supplied start and end times and returns the result as a sequence
#' @aliases setRange
#' @author William Cooper
#' @export setRange
#' @param Time A POSIXct-format Time variable
#' @param Start The desired start time in HHMMSS format (defaults to 0, which gives first index 1)
#' @param End The desired end time in HHMMSS format (defaults to the last time in the array)
#' @examples 
#' \dontrun{r <- setRange (Time, 103000, 113000)}
setRange <- function (Time, Start=0, End=0) {
  if(length(Time[is.na(Time)]) > 0) {
    print(sprintf("setRange failed, NA in time sequence; consider D <- D[!is.na(D$Time),]"))
    return(1)
  }
  SE <- getStartEnd(Time)
  if (Start == 0.) {Start <- SE[1]}
  if (End == 0.) {End <- SE[2]}
  DayWrap <- ifelse ((SE[1] > SE[2]),TRUE, FALSE)
  if (!DayWrap && (Start < SE[1] || End > SE[2])) {
    print ("setRange error, requested times outside file limits")
    return(1)
  } else {
    r <- getIndex(Time, Start):getIndex(Time, End)
    return (r)
  }
}

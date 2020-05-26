#' @title removeSpikes
#' @description Identifies spikes and replaces them via interpolation
#' @details The function calculates a running mean and standard deviation, then flags
#' points for which the deviation from the mean exceeds a specified multiplier times
#' the running standard deviation. It then replaces those points by interpolation.
#' It repeats this up to a specified number of times, in case the running values are changed by
#' the removal-and-interpolation process.
#' @aliases removeSpikes RemoveSpikes removespikes
#' @author William Cooper
#' @importFrom zoo rollapply
#' @export removeSpikes
#' @param v The variable from which to remove spikes.
#' @param sdLimit The multiple of the running standard deviation at which to remove
#' spikes. Default is 4.
#' @param runLength The number of measurements over which to calculate the running
#' mean and standard deviation. The default is 99.
#' @param loops The number of times to iterate the calculation.
#' @return The input vector with identified spikes removed and replaced by interpolation.
#' @examples 
#' RAFdata$DSPIKED <- removeSpikes(RAFdata$ATTACK)
#' RAFdata$DSPIKED <- removeSpikes(RAFdata$ATTACK, sdLimit=2, runLength=49, loops=10)
removeSpikes <- function (v, sdLimit=4, runLength=99, loops=4) {
  for (loop in 1:loops) {
    mn <- zoo::rollapply(v, width=runLength, FUN=mean, fill=NA)
    sdv <- zoo::rollapply(v, width=runLength, FUN=sd, fill=NA)
    ix <- which(abs(v - mn) / sdv > sdLimit)
    # print (sprintf ('spikes removed: %d on loop %d', length(ix), loop))
    if (length(ix) < 1) {break}
    v[ix] <- NA
    v <- SmoothInterp (v, .Length=0)
  }
  return(v)
}

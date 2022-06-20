#' @title ComplementaryFilter
#' @description Calculates a result from combining a fast-response and slow-response signal
#' @details Applies a Butterworth filter to calculate the low-pass-filtered result of the 
#' difference between the two signals, and adds this filtered result to the signal with 
#' fast response.
#' @details The use for this function is to combine two measurements of the same quantity, 
#' one that has fast response but perhaps drifts and the other that maintains absolute 
#' accuracy. The technique has been applied, for example, to the measurement of wind 
#' through the combination of GPS measurements and IRS measurements because GPS measurements 
#' have long-term accuracy while faster-responding IRS measurements suffer from drift and 
#' oscillation. Caveat: The filter function will fail if there are NA values in the sequence,
#' so these should be handled before calling this function, e.g., by replacing the NA values
#' by interpolation.
#' @aliases ComplementaryFilter
#' @author William Cooper
#' @export ComplementaryFilter
#' @importFrom signal filter butter 
#' @param FastSignal A numeric vector with a fast-response signal.
#' @param SlowSignal A numeric vector that is slower but has less absolute error than the 
#' fast-response signal.
#' @param tau The time constant for the low-pass filter, in units of the spacing of 
#' measurements in the input arrays. (Default: 150)
#' @return The vector of measurements that combines the two input signals.
#' @examples 
#' \dontrun{ComplementaryFilter(RAFdata$VNS, RAFdata$GGVNS, 150)}
ComplementaryFilter <- function (FastSignal, SlowSignal, tau=150) {
  xf <- FastSignal + 
    signal::filter (signal::butter(3, 2/tau), (SlowSignal - FastSignal))
  return (as.vector(xf))
}

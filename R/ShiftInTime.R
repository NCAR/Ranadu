## This function shifts a vector, usually representing a time series, 
## forward or backward, filling the end with duplication of the first
## or last value. It interpolates the series to a 125-Hz series, shifts
## that interpolated series, and then picks variables at the original
## rate.
#' @title ShiftInTime
#' @description Shift a time-series vector forward or backward.
#' @details For a vector assumed to be a time series with sampling
#' rate .rate, shift by .shift milliseconds. Fractional shifts are
#' handled by interpolating to a 125-Hz sequence, then picking values
#' from that interpolated sequence after shifting. The shift adds 
#' duplicate values at one end and removes values from the other
#' end. The .smooth logical variable gives some optional smoothing
#' of the interpolated series before subsetting to the original
#' rate. The result is the shifted time series.
#' @aliases ShiftInTime
#' @author William Cooper
#' @export ShiftInTime
#' @importFrom stats approx
#' @importFrom signal filter sgolay
#' @param .X A numeric vector, usually representing a measurement history.
#' @param .rate The assumed sampling rate in Hz (default 1 Hz). 
#' @param .shift The shift to be applied, in milliseconds. Can be positive 
#' or negative.
#' @param .smooth The smoothing interval in milliseconds.
#' Controls smoothing of the time series after interpolation
#' to high rate but before discrete points are selected from that series to
#' represent the shifted series. .smooth may be either zero  
#' or a positive odd integer. Default is 0, in which case no smoothing 
#' is performed. This is also the case for smoothing intervals less than
#' 40 ms (or five samples at 125-Hz). If .smooth is larger than 40, 
#' smoothing is performed using 3rd-order Savitzky-Golay polynomials 
#' spanning that interval.
#' @return The same series after shifting in time, possibly by fractions
#' of the sampling period.
#' @examples 
#' THDG2 <- ShiftInTime (RAFdata$THDG, .shift=-60)
#' newVariable <- ShiftInTime (1:50, .rate=1, .shift=-500)
  

ShiftInTime <- function (.X, .rate=1, .shift=0, .smooth=0) {
  ## negative .shift moves the series forward in time and so
  ## compensates for an assumed delay in recorded values.
  ## .shift has units of ms.
  iRate <- 125
  if (.shift == 0) {return(.X)}
  NL = length(.X)
  ## create iRate-Hz file for shifting, then average back to original rate
  ratio <- as.integer (iRate / .rate)
  ND <- ratio * NL
  n <- ifelse (.shift >= 0, as.integer (.shift*iRate/1000+0.5), 
               as.integer (.shift*iRate/1000-0.5))
  x <- 0:(NL-1)
  At <- stats::approx (x, .X, n=ND-ratio+1)
  ## now shift to match original
  j <- as.integer (ratio / 2)
  j2 <- ratio - j - 1
  T <- c(rep(.X[1], j), At$y, rep(.X[NL], j2))
  if (n < 0) {
    T <- c(T[(1-n):ND],rep(T[ND],-n))
  } else if (n > 0) {
    T <- c(rep(T[1], n), T[1:(ND-n)])
  }
  if (.smooth >= 40) {
    rtio <- as.integer (.smooth * iRate / 1000 )
    rtio <- ifelse (rtio %% 2, rtio, rtio+1)  ## ensure value is odd
    T <- signal::filter (signal::sgolay (3, rtio), T)
  }
  ## now pick off discrete values representing average at original rate
  tstart <- as.integer ((ratio + 1) / 2)
  T <- T[seq (tstart, ND, by=ratio)]
  return (T)
}

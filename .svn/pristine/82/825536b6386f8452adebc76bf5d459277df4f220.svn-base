#' @title ButterworthFilter
#' @description Applies a low-pass Butterworth filter recursively 
#' @details See the description in"ProcessingAlgorithms.pdf". Missing values are skipped in the recursive filter and are replicated in the output.
#' @aliases ButterworthFilter
#' @author William Cooper
#' @export ButterworthFilter
#' @param x A numeric vector giving the elements in the series
#' @param tau A numeric that determines the time constant of the filter, in units of the steps in the input time series.
#' @return The filtered numeric series
#' @examples 
#' \dontrun{xf <- ButterworthFilter (x, tau)}
ButterworthFilter <- function (x, tau=200.) {
  # input x is the unfiltered signal
  # output is the low-pass-filtered input
  # tau determines the cutoff
  a <- 2.*pi/tau
  c <- a*1.5**0.5
  s <- sin(c)
  c <- cos(c)
  a2 <- a*exp(-a/2.)*(c+(1./3.)**0.5*s)
  a3 <- 2.*exp(-a/2.)*c
  a4 <- exp (-a)
  zf <- vector (mode="numeric", length=5)
  xf <- vector (mode="numeric", length=length(x))
  # eventually, see if this loop can be made vector
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      zf[2] <- -a*x[i] + a2*zf[5] + a3*zf[3] -a4*zf[4]
      zf[1] <- a*x[i] + a4*zf[1]
      zf[4] <- zf[3]
      zf[3] <- zf[2]
      zf[5] <- x[i]
      xf[i] <- zf[1] + zf[2]
    } else {
      xf[i] <- NA
    }
  }
  return (xf)
}

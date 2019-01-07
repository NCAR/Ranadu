

#' @title memCoef
#' @description Calculates the Burg-algorithm coefficients for MEM spectral estimation
#' @details The resulting coefficients can be used as input to memEstimate to calculate the variance
#' spectrum. The full output list as returned from this function should be provided to memEstimate.
#' @references p.  568, Numerical Recipes in C, Second Edition, Press et al. 
#' (but here coded independently in R, following the algorithm specified there)
#' @aliases memCoef
#' @author William Cooper
#' @export memCoef
#' @param .data Numeric vector of data points. Often this should first have mean and trend removed,
#' e.g., using detrend() provided here, but that should be performed using a data.frame that includes
#' a 'Time' variable as the first variable. The numeric vector .data then can be the output from that
#' function.
#' @param .poles Number of poles to use in the calculation. A small number produces a smooth spectrum,
#' which a large number is needed for high-resolution features. A reasonable number (and the default)
#' is 5\% of the length of the .data vector. Values comparable to the length of the .data vector can 
#' take an awkwardly long time.
#' @return A list with these components:
#'   [[1]] .poles As used in the calculation
#'   [[2]] cf0    The mean-square discrepancy
#'   [[3]] cf     Numeric vector of coefficients, length=.poles
#' @examples 
#' S <- memCoef(RAFdata[, 'TASX'])

## This routine calculates to Burg-algorithm coefficients:
memCoef <- function (.data, .poles=NA) {
  .data <- as.numeric (.data)  ## ensure int is numeric and strip attributes
  if (is.na(.poles)) {.poles <- as.integer(length (.data) / 20)} ## default value
  cf <- vector ('numeric', .poles)
  ln <- length (.data)
  cf0 <- sum (.data^2, na.rm=TRUE) / ln
  a <- .data
  b <- c(.data[-1], 0)
  for (p in 1:.poles) {
    .r <- 1:(ln-p)
    cf[p] <- 2 * sum((a*b)[.r]) / sum((a^2+b^2)[.r])
    cf0 <- cf0 * (1-cf[p]^2)
    if (p > 1) {
      .r <- 1:(p-1)
      cf[.r] <- cf[.r] - cf[p] * rev(cf[.r])
    }
    if (p != .poles) {
      .r <- 1:(ln - p - 1)
      da <- cf[p] * b[.r]
      b[.r] <- b[.r+1] - cf[p] * a[.r+1]
      a[.r] <- a[.r] - da
      # for (j in 1:(ln-p-1)) {
      #   a[j] = a[j] - cf[p] * b[j];
      #   b[j] = b[j+1] - cf[p] * a[j+1];
      # }
    }
  }
  return (list (.poles, cf0, cf))
}

#' @title memEstimate
#' @description Calculates the estimated value of the MEM-based 
#' spectral amplitude at the specified frequency or frequencies.
#' @references p. 575, Numerical Recipes in C, second edition (Press et al.), but here coded
#' independently in R following the algorithm specified there.
#' @details Uses the list provided by memCoef(), which contains the Burg-algorithm coefficient,
#' as input. Calculates the spectral amplitude as a function of (frequency * deltaT) where deltaT
#' is the time between samples. The variance spectrum P is then
#' the square of the magnitude of the complex number that is
#' returned. The result is normalized so that twice the integral over positive
#' frequencies > 0 gives the variance; i.e., Var=2 (integral sign) P(f*deltaT) d(f*deltaT) where P is the
#' spectral variance and f is the frequency.
#' @author William Cooper
#' @aliases memEstimate, memEval
#' @export memEstimate
#' @param freq The frequency multiplied by the sample interval. For example, when using a time
#' series sampled at 10 Hz, the Nyquist frequency is 5 Hz and this is specified by freq=5*0.1 or
#' freq=0.5. When plotted or used to calculate variance, the sample interval must be considered
#' unless the sample interval is 1 s. A vector of frequencies will produce a corresponding vector 
#' of spectral estimates in the output.
#' @param .cf The list produced by a call to memCoef(). See that function for details.
#' @return A possibly single-element complex vector of estimates corresponding to the specified values of freq.
#' These are complex numbersi representing the amplitude, and the spectral estimate is usually obtained from the square of the modulus (R function 
#' mod()) of these numbers, or equivalently the product of this
#' amplitude and its complex conjugate. 
#' The estimates are returned as complex numbers for possible use in the
#' calculation of covariance, quadrature, coherence, and phase when multiplied by the complex conjugate
#' of the amplitudes returned for a second variable. The amplitude is
#' double-sided in frequency, so the estimate of spectral density should
#' be multiplied by 2 to obtain a spectrum for the positive frequency range.
#' @examples
#' ps <- 2 * Mod(memEstimate (0.25, memCoef(RAFdata[, 'TASX'])))^2
#' 
## variance estimate in units (frequency * diffTime), diffTime=time between samples
##  (Note: vectorized wrt freq)
memEstimate <- function (freq, .cf) {
  .poles <- .cf[[1]]
  cf0 <- .cf[[2]]
  cf <- .cf[[3]]
  freqR <- freq * 2 * pi
  z <- complex (real=1, imaginary=0)
  # zr <- cos(freqR); zi <- sin(freqR)
  # qr <- 1; qi <- 0
  # sumr <- 1; sumi <- 0
  # for (i in 1:.poles) {
  #   qt <- qr
  #   qr <- qr*zr-qi*zi
  #   qi <- qi*zr+qt*zi
  #   sumr <- sumr - cf[i] * qr
  #   sumi <- sumi - cf[i] * qi
  # }
  q <- complex(modulus=1, argument=freq*2*pi)
  A <- vector ('complex', .poles*length(freq))
  dim (A) <- c(.poles, length(freq))
  for (p in 1:.poles) {
    if (p == 1) {
      A[p,] <- z * q
    } else {
      A[p,] <- A[p-1,] * q
    }
  }
  .r <- 1:.poles
  if (length(freq) > 1) {
    s <- z - colSums (cf[.r] * A[.r,], dims=1, na.rm=TRUE)
  } else {
    s <- z - sum (cf[.r] * A[.r,], na.rm=TRUE)
  }
  return (sqrt(cf0) / s)    ## complex amplitude
  # return (c(cf0 / Mod (s)^2), cf0 / (sumr * sumr + sumi * sumi)))
}

#' @title detrend
#' @description Simple removal of mean and trend from a time series.
#' @details Input must be a data.frame where the first column is 'Time'
#' and the second is the variable from which to remove the mean and trend.
#' Removing the mean often provides less numerical inaccuracy, and removing
#' a trend (for spectral analysis) avoids the effect of a ramp function
#' on an fft-produced spectrum for which the time series is assumed repetitive.
#' For this calculation, NA values are replaced by interpolation or, where that
#' is not possible, by zeroes.
#' @author William Cooper
#' @importFrom stats lm
#' @importFrom stats coef
#' @aliases detrend deTrend
#' @export detrend
#' @param .data A data.frame containing variable 'Time' as the first column and
#' a second-column from which to remove the mean and trend. 
#' @return A modified version of data[, 2] only, with mean and trend removed.
#' @examples 
#' AT <- detrend (RAFdata[, c('Time', 'ATX')])
detrend <- function (.data) {
  .data[,2] <- SmoothInterp (.data[,2], .Length=0)  ## .Length=0 suppresses smoothing
  meanV <- mean (.data[,2], na.rm=TRUE)
  TS <- as.numeric(.data[,1]-mean(.data[,1], na.rm=TRUE))
  trendV <- coef (stats::lm (.data[,2] ~ TS))[2]
  print (sprintf ('mean and trend %.2f %.5f', meanV, trendV))
  return (.data[,2] - meanV - trendV * TS)
}




#' @title CohPhase
#' @description Plots the coherence and phase for a cross-spectrum. Optionally,
#' returns the cospectrum in a (frequency, covariance) data.frame.
#' @details For the variables provided, which must be in the supplied data.frame 
#' that must also contain the variables "Time" and a "Rate" attribute, this 
#' function constructs a plot of the squared coherence and phase for the cross-spectrum
#' of those two variables. The only method now available is the standard "spectrum" function of R
#' (via the spec.pgram() function. The mean and trend are removed before constructing the spectrum,
#' and missing values are replaced by interpolation where possible. Smoothing is applied via the
#' "spans" argument, approximately a running average in frequency, and additional
#' smoothing in logarithmic intervals in frequency is available
#' through the "smoothBins" parameter. To add approximate confidence intervals, this additional
#' smoothing is necessary because the confidence intervals are based on the standard deviations
#' in the bins. 
#' @aliases CohPhase, cohPhase, cohphase
#' @author William Cooper
#' @import utils
#' @import scales graphics
#' @importFrom zoo na.approx
#' @importFrom stats fft
#' @export CohPhase
#' @param .data A data.frame containing at least the variables "Time", ".Var1" and ".Var2".
#' It should also have an attribute "Rate"
#' if its rate is different from 1 Hz (the default). Any restrictions on the time range
#' should be applied to the data.frame before it is supplied to this function. See the
#' examples below. If subsetting removes the "Rate" attributes from the data.frame, the
#' value from the original data.frame should be added to .data.
#' @param .Var1 The (character) name of a variable that is a column in .data and for which the
#' variance cross-spectrum with .Var2 will be constructed. If this variable is not in .data an error message 
#' is generated.
#' @param .Var2 The (character) name of a second variable that is a column in .data and for which the
#' variance cross-spectrum with .Var1 will be constructed. If this variable is not in .data an error message 
#' is generated.
#' @param col The color to use when plotting this variable. The default is NA, and
#' in this case the following plot colors will be used in order: blue, forestgreen, black, 
#' brown.
#' @param spans An odd integer (or forced odd by incrementing upward if even) specifying the 
#' number of frequencies to span when averaging the spectral variance estimate produced by the R routine
#' "spectrom". The smoothing uses modified Daniell smoothers. This parameter can also be a vector of odd
#' integers, and in that case they will be applied consecutively. See help for that function for more 
#' information about the nature of this averaging. The default value is 25. If spans=NULL or spans <= 4 this averaging
#' is suppressed. In that case "smoothBins" below should be used because the coherence for an individual
#' frequency without smoothing is always 1.
#' @param smoothBins If a value larger than 5 is provided, the frequency range is divided 
#' into this number of intervals evenly spaced in the logarithm of the frequency. Then
#' estimates of the coherence and phase are binned into those intervals and averaged to
#' smooth the spectrum. Initial smoothing can be provided by "spans" (if larger than 4); the
#' smoothing by the "smoothBins" parameter is applied after and in addition to the smoothing via
#' "spans". The default is smoothBins=50.
#' @param plotType The type of plot generated. The default is 'ggplot', for which specifications
#' for aa faceted plot using ggplot2 are generated and returned to the calling program for plotting.
#' Any other value of this parameter causes a standard-R-graphics plot to be generated instead.
#' @param showErrors A non-zero value shows ribbon plots of the estimated uncertainty. 
#' The uncertainty estimate is based on the calculated sample standard deviation (not the
#' standard deviation in the mean) in each bin specified by the "smoothBins" parameter. The
#' value is the number of standard deviations represented by the ribbon; a value of 1 shows
#' a ribbon extending one standard deviation above and below the mean value. The default is
#' 0, and in that case no ribbon is plotted. The ribbon is plotted using color "grey" but
#' "alpha" of 0.15 for partial transparency. The "showErrors" parameter has no effect unless
#' the (default) plotType='ggplot' is used. The uncertainty band is not plotted where there are
#' fewer than two values to average in a bin. 
#' @param returnCospectrum Default is FALSE. If set TRUE, instead of a plot definition, 
#' the cospectrum and quadrature are returned in the form of a data.frame with columns "freq",
#' "cospec' and 'quad'. These are not smoothed and probably need smoothing when they are used.
#' The two variance spectra are also returned, as "spec1" and "spec2".
#' @return A ggplot2 definition for the plot of ((squared) coherence and phase as a function 
#' of frequency, if plotType == 'ggplot'. The resulting plot definition
#' can be plotted (via, e.g., 'print (CohPhase(...))) or
#' saved for later addition of more variables or for later plotting. The plot is returned
#' with the Ranadu theme "theme_WAC()", but that can be changed by adding another theme to the
#' plot definition before plotting. if plotType is not 'ggplot', the plots are generated 
#' using standard R plots and a data.frame is returned that contains the frequency, coherence, 
#' and phase. That can be used to plot separately and perhaps combine with other plots.
#' @examples 
#' \dontrun{CohPhase(RAFdata, 'GGVEW', 'VEW')}
#' \dontrun{CohPhase(RAFdata, 'ATX', 'DPXC', col='red', spans=15, smoothBins=25, showErrors=1)}

CohPhase <- function (.data, .Var1, .Var2, col='blue', spans=25, smoothBins=50, plotType='ggplot', 
  showErrors=0, returnCospectrum=FALSE) {
  if (is.data.frame(.data)) {
    if (.Var1 %in% names(.data)) {
      Z <- capture.output (Vr <- SmoothInterp(detrend (.data[, c('Time', .Var1)]), .Length=0))
    } else {
      print(sprintf('CohPhase ERROR: Variable %s is not in the supplied data.frame', .Var1))
      return (NA)
    }
    if (.Var2 %in% names(.data)) {
      Z <- capture.output (VrC <- SmoothInterp(detrend (.data[, c('Time', .Var2)]), .Length=0))
    } else {
      print(sprintf('CohPhase ERROR: Variable %s is not in the supplied data.frame', .Var2))
      return (NA)
    }
  } else {
    print('CohPhase ERROR: first argument is not a data.frame.')
    return (NA)
  }
  if (is.null(attr(.data, 'Rate'))) {
    print ('CohPhase warning: Rate attribute missing from data.frame, so using Rate=1')
    Rate <- 1
  } else {
    Rate <- attr(.data, 'Rate')
  }
  vcv <- cbind(ts(Vr, frequency=Rate), ts(VrC, frequency=Rate))
  P <- spec.pgram(vcv, detrend=FALSE, fast=TRUE, plot=FALSE, spans=spans)
  df1 <- data.frame(P$coh, log(P$freq))
  df2 <- data.frame (P$phase, log(P$freq))
  pf1 <- binStats (df1, bins=smoothBins)
  pf2 <- binStats (df2, bins=smoothBins)
  pf1 <- pf1[!is.na (pf1$ybar), ]
  pf2 <- pf2[!is.na (pf2$ybar), ]
  # pf1$sigma[pf1$nb > 1] <- pf1$sigma[pf1$nb > 1] / sqrt(pf1$nb[pf1$nb > 2])
  pf1$sigma[pf1$nb <= 1] <- NA # pf1$ybar[pf1$nb <= 1] * 0.5
  # pf2$sigma[pf2$nb > 1] <- pf2$sigma[pf2$nb > 1] / sqrt(pf2$nb[pf2$nb > 2])
  pf2$sigma[pf2$nb <= 1] <- NA # pf2$ybar[pf2$nb <= 1] * 0.5
  if (plotType != 'ggplot') {
    layout(matrix(1:2, ncol = 1), widths = c(5,5), heights = c(5,7))
    op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
    plotWAC (exp(pf1$xc), pf1$ybar, xlab='frequency [Hz]', col=col, logxy='x', ylab='coherence')
    title (sprintf('%s x %s', .Var1, .Var2))
    op <- par (mar=c(5,4,1,2)+0.1)
    plotWAC (exp(pf2$xc), pf2$ybar * 180 / pi, xlab='frequency [Hz]', col=col, logxy='x', ylab='phase [degrees]')
    abline(h=0, col='gray', lty=3)
    layout(matrix(1:1, ncol = 1), widths = c(5), heights = c(5))
    op <- par (mar=c(5,4,1,2)+0.1, oma=c(1.1,0,0,0))
    return (data.frame(freq = exp(pf1$xc), coherence=pf1$ybar, phase=pf2$ybar * 180 / pi,
            sigma1=pf1$sigma, sigma2=pf2$sigma * 180 / pi, nb1=pf1$nb, nb2=pf2$nb))
  } else {
    
    d2 <- data.frame(Time=exp(pf1$xc), coherence=pf1$ybar, phase=pf2$ybar*180/pi, 
      clo=(pf1$ybar-showErrors*pf1$sigma), chi=pf1$ybar+showErrors*pf1$sigma, 
      plo=(pf2$ybar-showErrors*pf2$sigma)*180/pi, phi=(pf2$ybar+showErrors*pf2$sigma)*180/pi)
    d2$clo[!is.na(d2$clo) & (d2$clo < 0)] <- 0
    labelP <- c('coherence', 'phase [degrees]')
    g <- ggplotWAC(d2[, c(1, 2, 3)], panels=2, labelP=labelP,
      col=col, lwd=c(1.0), lty=c(1), xlab='freq') 
    g <- g + xlab('frequency [Hz]') + ylab (sprintf ('%s x %s', .Var1, .Var2)) 
    g <- g + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=4),
      labels = trans_format("log10", math_format(expr = 10^.x))) + xlab('frequency [Hz]') 
    if (showErrors > 0 && smoothBins > 5) {
      da <- data.frame(d2[, c(1,4,5)])
      db <- data.frame(d2[, c(1,6,7)])
      names(da) <- c('Time', 'ymin', 'ymax')
      names(db) <- c('Time', 'ymin', 'ymax')
      da$PanelGroup <- labelP[1]
      db$PanelGroup <- labelP[2]
      d <- rbind(db,da)
      g <- g + geom_ribbon(data=d, aes(x=Time, ymin=ymin, ymax=ymax), colour='grey', 
        alpha=0.15, inherit.aes=FALSE)
    }
    g <- g + theme_WAC(1)+theme(legend.position='none')
    if (returnCospectrum) {
      CS <- sqrt(P$coh[,1] * P$spec[, 1] * P$spec[, 2] / (1 + tan(P$phase[,1])^2))
      v1 <- SmoothInterp(.data[, .Var1], .Length=0)
      v2 <- SmoothInterp(.data[, .Var2], .Length=0)
      v1 <- detrend(data.frame(Time=.data$Time, v1))
      v2 <- detrend(data.frame(Time=.data$Time, v2))
      ff1 <- fft(v1)
      ff2 <- fft(v2)
      G <- Re(ff1 * Conj(ff2))/nrow(.data)
      GQ <- Im(ff1 * Conj(ff2))/nrow(.data)
      N <- nrow(.data) %/% 2 
      S1 <- Re(ff1 * Conj(ff1) / nrow(.data))
      S2 <- Re(ff2 * Conj(ff2) / nrow(.data))
      G <- G[2:(N+1)]
      GQ <- GQ[2:(N+1)]
      S1 <- S1[2:(N+1)]
      S2 <- S2[2:(N+1)]
      frq <- c(1:N) * Rate / nrow(.data)
      spec1 <- 2 * S1 / Rate
      spec2 <- 2 * S2 / Rate
      cospec <- 2 * G / Rate
      quad <- 2 * GQ / Rate
      # cospectrum - i * quadrature = (gain spectrum) * exp(i*(phase spectrum))
      # sqrt(cospectrum^2 + quadrature^2) is the amplitude or gain spectrum
      return(data.frame(freq=frq, cospec=cospec, quad=quad, spec1=spec1, spec2=spec2))
    } else {
      return(g)
    }
  }
}





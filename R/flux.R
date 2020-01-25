#' @title flux
#' @description Plot cospectrum for flux and calculate flux.
#' @details For a specified scalar variable, calculates the cospectrum with WIC
#' and the exceedance function. Scales by specified variables (e.g., air density
#' and specific heat for sensible heat flux) and accepts specifications for the
#' units of the result and for the upper-wavelength limit. Optionally adds a
#' line representing averaged values in equal-log-interval spacing, optionally
#' with standard-deviation shading. Returns a data.frame with the values,
#' and plots the result unless the "plot" argument is set FALSE.
#' @aliases flux
#' @author William Cooper
#' @importFrom dplyr pull
#' @export flux
#' @param .data A Ranadu-convention data.frame containing at least the variables
#' Time, TASX, WIC, and the scalar to use for the flux calculation. 
#' @param .A The name of a variable in .data to use as the scalar in the flux
#' calculation.
#' @param Units A character string or bquote expression for the units. For
#' math expressions this is best provided as a bquote term; e.g., for heat flux,
#' Units = bquote("W"~m^-2). The default is '' giving no units.
#' @param scaleFactor A vector of length equal to the rows in .data that specifies
#' a weight factor that will be multiplied with the cospectrum to obtain the
#' flux cospectrum. Default: 1. For sensible heat flux, for example, this should
#' be the product of air density and specific heat at constant pressure.
#' @param spans The length of the Daniell-smoother sequence to use smoothing the cospectrum.
#' Default is 49.
#' @param smoothBins If a value larger than 5 is provided, the frequency range is 
#' divided into this number of intervals evenly spaced in the logarithm of the 
#' frequency. Then estimates of the spectral density are binned into those intervals 
#' and averaged to smooth the spectrum. Initial smoothing can be provided by "spans" 
#' (if larger than 4). The smoothing by the "smoothBins" parameter is applied after 
#' and in addition to those smoothing methods. The default (0) suppresses this smoothing.
#' @param legend.position This parameter specifies the position for the legend. 
#' The default is 'bottomleft'. The legend can also be suppressed by setting this 
#' parameter to NA. Then a legend can still be added after the plot generation.
#' @param .plot Should the result be plotted? Default is TRUE, but if FALSE no
#' plot is generated but the plot-line definitions are still returned in a data.frame.
#' @param plotRibbon Should the standard-deviation ribbon be shown with the bin-averaged
#' values? Default is TRUE unless the bin-averaged line is suppressed by a low value of
#' smoothBins.
#' @param printTitle Should the title be printed containing flux values? Default is TRUE. 
#' @param wavelengthLimit The largest wavelength to include in the "FluxL" calculation.
#' The default is 2000 [meters].
#' @param Par Optional time-response characteristics of the variable .A to use to
#' correct for the time response. If provided, this should be a data.frame with variables
#' a, tau1, and tau2. The argument a represents the fraction of a two-time-constant
#' response caused by the direct measurement (e.g., by exposure to the temperature of
#' the airstream) and tau1 is the characteristic time constant for that response. If
#' a is not 1, tau2 should represent a second time constant, e.g., the response
#' characteristic of the wire support in the case of a temperature sensor.
#' @param ... Additional arguments to pass to plot().
#' @return A data.frame containing the frequency, the smoothed cospectrum (not weighted by
#' frequency), and the exceedance values. The data.frame also has attributes "Flux" and "FluxL"
#' representing the total flux and the flux from wavelengths smaller than
#' the wavelength wavelengthLimit. The wavelengthLimit is also included as an attribute.
#' If smoothBins > 5, the bin-averaged values are also returned as a data.frame named
#' "smoothed data.frame".
#' @examples 
#' #' X <- flux(RAFdata, 'ATX', scaleFactor=RAFdata$PSXC*100/((RAFdata$ATX+273.15)*287)*1005)

flux <- function(.data, .A, Units = '', scaleFactor = 1, spans = 49, smoothBins = 0,
                 legend.position = 'bottomleft', .plot = TRUE, plotRibbon = TRUE,
                 printTitle = TRUE, wavelengthLimit = 2000, Par = NA, ...) {
  WP <- .data$WIC - mean(.data$WIC, na.rm=TRUE)
  # Cp <- SpecificHeats(D$EWX / D$PSXC)[, 1]
  # Rho <- 100 * D$PSXC / ((D[, .A] + 273.15) * 
  #     SpecificHeats(D$EWX / D$PSXC)[, 3])
  # scaleFactor <- Cp * Rho
  TP <- .data[, .A] * scaleFactor
  TP <- TP - mean(TP, na.rm=TRUE)
  Tasm <- mean(.data$TASX, na.rm=TRUE)
  fL <- Tasm / wavelengthLimit
  DCP <- data.frame('Time' = .data[, 'Time'])
  DCP$WP <- WP
  DCP$TP <- TP
  attr(DCP, 'Rate') <- attr(.data, 'Rate')
  CS <- CohPhase(DCP, 'TP', 'WP', returnCospectrum = TRUE)
  CS$csUncorrected <- CS$cospec
  if (is.na(Par[1])) {
    ylab <- bquote("f x flux cospectrum ["*.(Units)*"]")    
  } else {
    ylab <- bquote("f x flux cospectrum (corrected) ["*.(Units)*"]")
    ## Correct for the time response of the sensor:
    a <- Par$a
    tau1 <- Par$tau1
    tau2 <- Par$tau2
    frq <- CS$freq
    zeta <- -atan(2*pi*frq*tau2)
    b <- cos(zeta)
    ## Use the Laplace-transform solution
    C1 <- 1 / (1 + 4 * pi^2 * frq^2 * tau1^2) * 
      (-(a + (1 - a) * b * cos(zeta)) * 2 * pi * frq * tau1 +
         (1 - a) * b * sin(zeta)) 
    C2 <- 1 / (1 + 4 * pi^2 * frq^2 * tau1^2) * 
      ((a + (1 - a) * b * cos(zeta)) + 
         (1 - a) * b * sin(zeta) * 2 * pi * frq * tau1)
    cTC <- sqrt(C1^2 + C2^2)
    phiTC <- atan2(C1, C2)
    CS$cospec <- CS$cospec / (cTC * cos(phiTC))
    CS$quad <- CS$quad / (cTC)
  }
  CSogive <- cumsum(CS$cospec) * CS$freq[1]
  CSogive <- CSogive[length(CSogive)]-CSogive
  CS$ogive <- CSogive
  CSUCogive <- cumsum(CS$csUncorrected) * CS$freq[1]
  CSUCogive <- CSUCogive[length(CSUCogive)]-CSUCogive
  CS$UCogive <- CSUCogive
  # CS$cospec <- SmoothInterp(CS$cospec, .Length=spans)
  CSogiveQ<- cumsum(CS$quad) * CS$freq[1]
  CSogiveQ <- CSogiveQ[length(CSogiveQ)]-CSogiveQ
  CS$ogiveQ <- CSogiveQ
  # CS$cospec <- SmoothInterp(CS$cospec, .Length=spans)
  # CS$quad <- SmoothInterp(CS$quad, .Length=spans)
  CS$cospec <- SmoothInterp(CS$cospec, .Length=0)  # treat NAs
  s25 <- spans %/% 25; s10 <- spans %/% 10; s3 <- spans %/% 3
  s25 <- s25 + (s25 + 1) %% 2
  s10 <- s10 + (s10 + 1) %% 2
  s3 <- s3 + (s3 + 1) %% 2
  CS$cospec <- zoo::rollapply(CS$cospec, FUN = mean, fill='extend', width = s25)
  CS$cospec[CS$freq > 0.01] <- zoo::rollapply(CS$cospec, FUN = mean, fill='extend', width = s10)[CS$freq > 0.01]
  CS$cospec[CS$freq > 0.1] <- zoo::rollapply(CS$cospec, FUN = mean, fill='extend', width = s3)[CS$freq > 0.1]
  CS$cospec[CS$freq > 1] <- zoo::rollapply(CS$cospec, FUN = mean, fill='extend', width = spans)[CS$freq > 1]
  CS$quad <- SmoothInterp(CS$quad, .Length=0)  # treat NAs
  CS$quad <- zoo::rollapply(CS$quad, FUN = mean, fill='extend', width = s25)
  CS$quad[CS$freq > 0.01] <- zoo::rollapply(CS$quad, FUN = mean, fill='extend', width = s10)[CS$freq > 0.01]
  CS$quad[CS$freq > 0.1] <- zoo::rollapply(CS$quad, FUN = mean, fill='extend', width = s3)[CS$freq > 0.1]
  CS$quad[CS$freq > 1] <- zoo::rollapply(CS$quad, FUN = mean, fill='extend', width = spans)[CS$freq > 1]
  FluxL <- CSogive[which(CS$freq > fL)[1]]
  Flux <- mean(WP * TP, na.rm=TRUE)
  Flux <- CSogive[which(CS$freq > 0.01)[1]]
  attr(CS, 'Flux') <- Flux
  attr(CS, 'FluxL') <- FluxL
  attr(CS, 'wavelengthLimit') <- wavelengthLimit
  ## Construct the plot:
  CS$ncospec <- -1 * CS$cospec
  CS$nquad <- -1 * CS$quad
  ## Weight by frequency for log-abscissa plot:
  CS$cospec <- CS$cospec * CS$freq
  CS$ncospec <- CS$ncospec * CS$freq
  CS$quad <- CS$quad * CS$freq
  CS$nquad <- CS$nquad * CS$freq
  # plotWAC(CS, xlab='frequency [Hz]', ylab=ylab, log='xy',
  #     col = c('skyblue', 'forestgreen', 'red'), lwd = c(2, 2, 2), 
  #     lty = c(1, 2, 1),
  #     xlim=c(0.05, 15), ylim=c(0.01,250), legend.position=NA)
  if (smoothBins > 5) {
    BS <- binStats(data.frame(CS$cospec, log(CS$freq)), bins = smoothBins)
    # lines(exp(BS$xc), BS$ybar, lwd=2, col='brown')
    BS$nybar <- -1 * BS$ybar
    BS$ybar[BS$ybar < 0] <- NA
    BS$nybar[BS$nybar < 0] <- NA
    BS$xc <- exp(BS$xc)
    # lines(exp(BS$xc), BS$nybar, lwd=2, col='magenta')
    BSQ <- binStats(data.frame(CS$quad, log(CS$freq)), bins = smoothBins)
    # lines(exp(BS$xc), BS$ybar, lwd=2, col='brown')
    BSQ$nybar <- -1 * BSQ$ybar
    BSQ$ybar[BSQ$ybar < 0] <- NA
    BSQ$nybar[BSQ$nybar < 0] <- NA
    BSQ$xc <- exp(BSQ$xc)
    BS$ybarQ <- BSQ$ybar
    BS$sigmaQ <- BSQ$sigma
    BS$nbQ <- BSQ$nb  # Is this needed? Always same as BS$nb?
    BS$nybarQ <- BSQ$nybar
    attr(CS, 'smoothed data.frame') <- BS
    bse <- data.frame(x = BS$xc, ymin = BS$ybar - BS$sigma, ymax = BS$ybar + BS$sigma,
                      yminN = BS$nybar - BS$sigma, ymaxN = BS$nybar + BS$sigma)
    # lines(exp(BS$xc), BS$nybar, lwd=2, col='magenta')
  }
  xlim=c(0.01, 15)
  ylim=c(0.01, 250)
  if ("xlim" %in% names(list(...))) {
    ix <- which ('xlim' == names(list(...)))
    xlim <- list(...)[[ix]]
  }
  if ("ylim" %in% names(list(...))) {
    ix <- which ('ylim' == names(list(...)))
    ylim <- list(...)[[ix]]
  }
  bse$ymin[bse$ymin < ylim[1]] <- ylim[1]
  bse$yminN[bse$yminN < ylim[1]] <- ylim[1]
  g <- ggplot(data = CS, aes(x=freq))
  g <- g + geom_path(aes(y = cospec, colour='cospectrum', linetype='cospectrum'))
  g <- g + geom_path(aes(y = ncospec, colour='-cospectrum', linetype='-cospectrum'))
  g <- g + geom_path(aes(y = ogive, colour='exceedance', linetype='exceedance'), lwd=1.2)
  if (!is.na(Par[1])) {
    g <- g + geom_path(aes(y = UCogive, colour='exceedance'), lty=2, lwd=1.2)
  }
  if (smoothBins > 5) {
    # g <- g + geom_path(data = BS, aes(x=xc, y=ybar), colour='blue', lwd=1.2)
    # g <- g + geom_path(data = BS, aes(x=xc, y=nybar), colour='deeppink3', lwd=1.2)
    g <- g + geom_point(data = BS, aes(x=xc, y=ybar), colour='black', pch=19)
    g <- g + geom_point(data = BS, aes(x=xc, y=nybar), colour='darkred', pch=19)
    if (plotRibbon) {
      # GeomRibbon$handle_na <- function(data, params) {  data }
      g <- g + geom_ribbon(data=bse, aes(x=x, ymin=ymin, ymax=ymax),
        fill='blue', alpha=0.2, show.legend=FALSE, inherit.aes=FALSE, na.rm=FALSE)
      g <- g + geom_ribbon(data=bse, aes(x=x, ymin=yminN, ymax=ymaxN),
                fill='red', alpha=0.2, show.legend=FALSE, inherit.aes=FALSE, na.rm=FALSE)
      
      # g <- g + geom_path(data=bse, aes(x=x, y=ymin), lty=1, lwd=0.5, col='magenta') +
      #          geom_path(data=bse, aes(x=x, y=ymax), lty=1, lwd=0.5, col='magenta')
    }
  }
  g <- g + geom_path(data=data.frame(x=rep(fL, 2), y=ylim), aes(x=x, y=y), linetype=2)
  g <- g + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=4), #limits = xlim, 
    labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n=4), #limits = ylim,             
      labels = trans_format("log10", math_format(10^.x))) +
    annotation_logticks(sides='trbl') +
    coord_cartesian(xlim=xlim, ylim=ylim)
  g <- g + xlab('frequency [Hz]') + ylab(ylab)
  g <- suppressWarnings(g + scale_colour_manual (name='', 
    values=c('cospectrum'='blue', '-cospectrum'='red', 'exceedance'='brown')))
  g <- g + scale_linetype_manual (name='', values=c('cospectrum'=1, '-cospectrum'=1, 'exceedance'=1))
  g <- g + guides(col=guide_legend(reverse = TRUE), linetype=guide_legend(reverse = TRUE))
  ttl <- bquote('Total flux '~.(format(Flux, digits=3))~.(Units)*'; partial <'*.(format((wavelengthLimit/1000), digits=2))~'km:'~.(format(FluxL, digits=3))~.(Units))
  if (printTitle) {
    g <- g + labs(title=ttl)
  }
  suppressWarnings(print(g + theme_WAC(1) + theme(plot.title = element_text(size=12)) +
                             theme(legend.position=c(0.65, 0.91))))
  # par(bg = 'gray95')
  # plotWAC(data.frame(exp(BSF1$xc), BSF1$ybar, BSF1$nybar), 
  #   col = c('blue', 'red'), ylab = ylab,
  #   xlab='frequency [Hz]', log='xy', xlim=c(0.05,15),
  #   ylim=c(0.01,250), legend.position=NA)
  # lines(CSF1, col='gray50')
  # lines(exp(BSF1$xc), BSF1$ybar, col='blue', lwd=2)
  # lines(exp(BSF1$xc), BSF1$ybar+BSF1$sigma, col='blue', lwd=1)
  # lines(exp(BSF1$xc), BSF1$ybar-BSF1$sigma, col='blue', lwd=1)
  return(CS)
}

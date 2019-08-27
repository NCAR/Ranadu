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
#' @param ... Additional arguments to pass to plot().
#' @return A data.frame containing the frequency, the smoothed cospectrum, and
#' the exceedance values. The data.frame also has attributes "Flux" and "FluxL"
#' representing the total flux and the flux from wavelengths smaller than
#' the wavelength wavelengthLimit. The wavelengthLimit is also included as an attribute.
#' @examples 
#' #' X <- flux(RAFdata, 'ATX', scaleFactor=RAFdata$PSXC*100/((RAFdata$ATX+273.15)*287)*1005)

flux <- function(.data, .A, Units = '', scaleFactor = 1, spans = 49, smoothBins = 0,
                 legend.position = 'bottomleft', .plot = TRUE, plotRibbon = TRUE,
                 printTitle = TRUE, wavelengthLimit = 2000, ...) {
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
  CS <- CohPhase(DCP, 'WP', 'TP', returnCospectrum = TRUE)
  CSogive <- cumsum(CS$cospec * CS$freq[1])
  CSogive <- CSogive[length(CSogive)]-CSogive
  CS$ogive <- CSogive
  CS$cospec <- SmoothInterp(CS$cospec, .Length=spans)
  FluxL <- CSogive[which(CS$freq > fL)[1]]
  Flux <- mean(WP * TP, na.rm=TRUE)
  attr(CS, 'Flux') <- Flux
  attr(CS, 'FluxL') <- FluxL
  attr(CS, 'wavelengthLimit') <- wavelengthLimit
  ## Construct the plot:
  #ylab <- expression(paste("flux cospectrum x f [W ",m^-2,"]"))
  ylab <- bquote("flux cospectrum x f ["*.(Units)*"]")
  CS$ncospec <- -1 * CS$cospec
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
    attr(CS, 'smoothed data.frame') <- BS
    bse <- data.frame(x = BS$xc, ymin = BS$ybar - BS$sigma, ymax = BS$ybar + BS$sigma)
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
  g <- ggplot(data = CS, aes(x=freq))
  g <- g + geom_path(aes(y = cospec, colour='cospectrum', linetype='cospectrum'))
  g <- g + geom_path(aes(y = ncospec, colour='-cospectrum', linetype='-cospectrum'))
  g <- g + geom_path(aes(y = ogive, colour='exceedance', linetype='exceedance'))
  if (smoothBins > 5) {
    g <- g + geom_path(data = BS, aes(x=xc, y=ybar), colour='brown')
    if (plotRibbon) {
      g <- g + geom_ribbon(data=bse, aes(x=x, ymin=ymin, ymax=ymax),
        fill='gray50', alpha=0.5, show.legend=FALSE, inherit.aes=FALSE, na.rm=TRUE)
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
    values=c('cospectrum'='skyblue', '-cospectrum'='red', 'exceedance'='darkorange')))
  g <- g + scale_linetype_manual (name='', values=c('cospectrum'=1, '-cospectrum'=1, 'exceedance'=2))
  g <- g + guides(col=guide_legend(reverse = TRUE), linetype=guide_legend(reverse = TRUE))
  ttl <- bquote('Total flux '~.(format(Flux, digits=3))~.(Units)*'; partial <'*.(format((wavelengthLimit/1000), digits=2))~'km:'~.(format(FluxL, digits=3))~.(Units))
  if (printTitle) {
    g <- g + labs(title=ttl)
  }
  suppressWarnings(print(g + theme_WAC() + theme(plot.title = element_text(size=12))))
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

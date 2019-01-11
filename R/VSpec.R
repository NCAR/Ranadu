#' @title VSpec
#' @description Produces a variance spectrum in a ggplot2 file suitable for printing.
#' @details For the variable provided, which must be in the supplied data.frame 
#' that must also contain the variables "Time" and "TASX' and a "Rate" attribute, this 
#' function constructs a plot of the spectral variance of that variable. The method can
#' be the standard "spectrum" function of R (the default), the "Welch" method as provided
#' by the R package "bspec", or the maximum-entropy method ("MEM") as implemented in other
#' function of Ranadu. Some options are provided for each, with defaults that are usually
#' appropriate. If the ADD parameter is set to the ggplot definition returned from a
#' previous call, the new spectrum is added to that plot (up to four plotted spectra
#' on a single plot). The mean and trend are removed before constructing the spectrum,
#' and missing values are replaced by interpolation where possible.
#' Optional additional smoothing in logarithmic intervals in frequency is available
#' through the "smoothBins" parameter. The plot includes background lines showing the
#' -5/3 slope expected for homogeneous isotropic turbulence, and for wind variables
#' the magnitude of these lines represent factor-of-10 changes in the eddy dissipation
#' rate with the larger-dot line representing 1e-4 m^2/s^3. 
#' @aliases vSpec vspec
#' @author William Cooper
#' @import scales bspec
#' @importFrom zoo na.approx
#' @importFrom stats pchisq pnorm qchisq spec.pgram spectrum ts
#' @export VSpec
#' @param .data A data.frame containing at least the variables "Time", "TASX" and ".Variable" where
#' ".Variable" is the second (required) parameter. It should also have an attribute "Rate"
#' if its rate is different from 1 Hz (the default). Any restrictions on the time range
#' should be applied to the data.frame before it is supplied to this function. See the
#' examples below. If subsetting removes the "Rate" attributes from the data.frame, the
#' value from the original data.frame should be added to .data.
#' @param .Variable The (character) name of a variable that is a column in .data and for which the
#' variance spectrum will be constructed. If this variable is not in .data an error message 
#' is generated.
#' @param VLabel A character string that will be used as the label for this variable in
#' the legend. The default is .Variable. The labels should differ if they are to appear
#' separately in the legend. For example, to plot the spectrum for the same variable using
#' different methods, include labels that indicate the different methods. See the
#' examples.
#' @param col The color to use when plotting this variable. The default is NA, and
#' in this case the following plot colors will be used in order: blue, forestgreen, black, 
#' brown.
#' @param type Three choices are avaiable: "spectrum" (the default), which
#' uses the R routine "spectrum()" from the stats package to estimate the spectral
#' density; "Welch", which uses the implementation of the Welch method of averaging
#' segments as implemented in the R package "bspec", and "MEM" to use the maximum-entropy
#' method of spectral estimation as implemented in the Ranadu routines memCoef() and
#' memEstimate(). The parameter "spans" applies only to the "spectrum" method, the
#' argument "segLength" to the Welch method, and the
#' arguments "poles" and "resolution" only to the "MEM" method.
#' @param method This is the same as "type" and over-rides it if present.
#' @param xlim A two-element vector specifying the lower and upper limits for the abscissa of the
#' plot. The default is c(0.001, 15).
#' @param ylim A two-element vector specifying the lower and upper limits for the ordinate of the
#' plot. The default is c(0.0001, 1).
#' @param spans An odd integer (or forced odd by incrementing upward if even) specifying the 
#' number of frequencies to span when averaging the spectral variance estimate produced by the R routine
#' "spectrom". The smoothing uses modified Daniell smoothers. This parameter can also be a vector of odd
#' integers, and in that case they will be applied consecutively. See help for that function for more 
#' information about the nature of this averaging. The default value is 49. If spans=NULL or spans <= 4 this averaging
#' is suppressed.
#' @param ae A factor that scales the predicted lines representing constant eddy dissipation
#' rate to adjust for longitudinal (where ae should be 0.15) or lateral (0.2) spectra. The
#' default is 0.2; use 0.15 for the variables TASX, UXC, etc. This applies to only the first
#' spectrum plotted; others use the same background for the eddy dissipation rate. To make an
#' appropriate scale adjustment for plots that include both longitudinal and lateral
#' variance spectra, consider multiplying the longitudinal variable by sqrt(0.15/0.2) for 
#' display purposes.
#' @param smoothBins If a value larger than 5 is provided, the frequency range is divided 
#' into this number of intervals evenly spaced in the logarithm of the frequency. Then
#' estimates of the spectral density are binned into those intervals and averaged to
#' smooth the spectrum. Initial smoothing can be provided by "spans" (if larger than 4) for
#' the "spectrum" method and by using a small number of poles for the "MEM" method; the
#' smoothing by the "smoothBins" parameter is applied after and in addition to those
#' smoothing methods. The default (0) suppresses this smoothing.
#' @param segLength The length of segments in the time series to use when averaging to
#' find the estimate of spectral density.
#' @param poles The number of poles to use for the maximum-entropy estimates. The default (50)
#' usually is a good first choice. For more structure, try 100. If you go beyond around 150,
#' the method becomes too slow to be practical, esp. for high-rate files.
#' @param resolution The increment (as a fraction of the logarithm of the frequency range) 
#' at which to evaluate the MEM estimate of spectral density. If a fine feature is expected
#' in the spectrum, the resolution should be small enough to isolate it, but small values
#' increase the variance in the individual estimates.
#' @param showErrors A non-zero value shows ribbon plots of the estimated uncertainty. If 
#' smoothBins is > 9, the uncertainty estimate is based on the calculated standard deviation
#' in the mean in each bin; otherwise, the estimate is based on the expected uncertainty
#' for the particular method used. The
#' value is the number of standard deviations represented by the ribbon; a value of 1 shows
#' a ribbon extending one standard deviation above and below the mean value. The default is
#' 0, and in that case no ribbon is plotted. The ribbon is plotted using color "gray50" but
#' "alpha" of 0.5 for partial transparency. 
#' @param WavelengthScale If TRUE (the default), include a wavelength scale on the plot.
#' @param ADD This parameter has the default value NA, which causes the function to plot 
#' only the spectrum for the variable provided. If a spectrum for an other set of variables 
#' has already been defined by previous calls to VSpec, setting ADD to the plot definition 
#' returned by that previous call will add this plot to the previous plot. Up to four
#' variables can be included in the final plot. See the examples.
#' @param add This is only included to make it possible to specify either "ADD" or "add".
#' Default is NA, in which case the value of ADD is used.
#' @param EDR If EDR is set TRUE, the plot will be normalized such that the ordinate  
#' is constant and has the value of the eddy dissipation rate for an inertial subrange. Otherwise
#' this is not a true density function of log(frequency) and so is difficult to determine
#' except for the above specialized use. The variable plotted is (2*pi/V)(C*P(f)*f^(5/3))^(3/2)
#' where V is the airspeed in m/s and C is a constant equal to 1.5 for lateral spectra like
#' WIC and 2 for longitudinal spectra like TASX. The resulting units are m^2/s^3 per interval in the
#' true abscissa coordinate of -1.5C(epsilon)^-(1/3)k^(-2/3), but the plot is labeled by
#' the appropriate values of frequency instead. 
#' @return A ggplot2 definition for the plot of spectral density as a function of frequency.
#' The normalization is one-sided; i.e., the integral of the spectral variance from zero
#' to infinity is the total variance of the variable. The resulting plot definition
#' can be plotted (via, e.g., 'print (VarSpec(...))) or
#' saved for later addition of more variables or for later plotting. The plot is returned
#' with the standard ggplot theme; to use the Ranadu theme "theme_WAC()", add it to the
#' plot definition that is returned before plotting. In addition, to make it possible to 
#' superimpose future plots, the following variables are saved in the global environment: 
#' .clinesVSpec and .VSpecDF{1,2,3}, .VSpecVar{2,3}. The function does not check for 
#' collision with other possible uses of those names in the global environment.
#' @examples 
#' VSpec(RAFdata, 'WSC')
#' g <- VSpec(RAFdata, 'WSC', VLabel='std', xlim=c(0.1,1));
#' VSpec(RAFdata, 'WSC', VLabel='MEM', method='MEM', ADD=g)
#' VSpec(RAFdata,'TASX', spans=11, showErrors=1, xlim=c(0.01,1)) + theme_WAC()
VSpec <- function (.data, .Variable, VLabel=NA, col=NA, type='spectrum', method=NA, xlim=c(0.001, 15), ylim=c(0.0001,1),
  spans=49, ae=0.2, smoothBins=0, segLength=512, poles=50, resolution=0.0001, showErrors=0, 
  WavelengthScale=TRUE, ADD=NA, add=NA, EDR=FALSE) {
  if (is.data.frame(.data)) {  ## must be true, or exit. Needs to contain Time and TASX in addition to .Variable
    if (!is.character(.Variable)) {  ## then try to get name of the vector
      .Variable <- deparse (substitute (.Variable))
    }
    if (.Variable %in% names(.data)) {
      Z <- capture.output (v <- detrend (.data[, c('Time', .Variable)]))
      if (!is.na(VLabel)) {    ## use this alternate name in legend
        V <- VLabel
      } else {
        V <- .Variable
      }
    } else {
      print(sprintf('VSpec ERROR: Variable %s is not in the supplied data.frame', .Variable))
      return (NA)
    }
  } else {
    print('VSpec ERROR: first argument is not a data.frame.')
    return (NA)
  }
  if (is.null(attr(.data, 'Rate'))) {
    print ('VSpec warning: Rate attribute missing from data.frame, so using Rate=1')
    Rate <- 1
  } else {
    Rate <- attr(.data, 'Rate')
  }
  if (!is.na(method)) {type <- method}  ## method over-rides if present
  if (type != 'spectrum' && type != 'Welch' && type != 'MEM' && type != 'mem') {
    print (sprintf ('type %s is unavailable; using type=spectrum', type))
    type <- 'spectrum'
  }
  siglim <- 1  ## 1-sigma error limits
  if (type == 'spectrum') {
    if (!is.null(spans[1])) {
      if (!(spans[1] %% 2)) {spans[1] <- spans[1] + 1}
      if (spans[1] <= 5) {spans <- NULL}
    }
    S <- spectrum (ts(SmoothInterp(v, .maxGap=1000*Rate, .Length=0), frequency=Rate), span=spans, plot=FALSE)
    freq <- S$freq
    fpf <- 2 * S$spec * freq
  } else if (type == 'Welch') {  ## bspec section
    ## force segLength to a power of 2
    segl <- segLength
    rsl <- log(segl) / log(2)
    ns <- round (rsl)
    if (2^ns != segl) {
      if (2^ns > segl) {segl <- 2^(ns-1)}
      else {segl <- 2^(ns+1)}
      segLength <- segl
      print (sprintf ('reset segLength to %d', segLength))
    }
    S2 <- bspec::welchPSD (ts(SmoothInterp(v, .Length=0), frequency=Rate), seglength=segLength,
      windowfun=bspec::hammingwindow)
    # ci <- quantile.bspec(BSP, probs = c(0.025, 0.975),
    #   two.sided = FALSE)
    coverage <- 0.683
    tail <- 1 - coverage
    df <- 2 * 9 * S2$segments / 11 ##1.768849
    upper.quantile <- 1 - tail * pchisq(df, df, lower.tail = FALSE)
    lower.quantile <- tail * pchisq(df, df)
    ci <- 1/(qchisq(c(upper.quantile, lower.quantile), df)/df)
    df <- 1.46 * (S2$segments + 1)
    lower.limit <- qchisq (pnorm(-siglim), df) / df
    upper.limit <- qchisq (pnorm(siglim), df) / df
    # ci <- 0.5 + (ci-0.5) / sqrt(9 * S2$segments / 11)
    # print (sprintf ('ci2=%.3f -- %.3f segments %d', ci[1], ci[2], S2$segments))
    freq <- S2$frequency[-1]
    fpf <- S2$power[-1] * freq
  } else if (type == 'MEM' || type == 'mem') {  ## MEM section
    A <- memCoef (v, poles)
    ld <- nrow(.data)
    fmin <- log (Rate / ld)
    fmax <- log (0.5*Rate)
    bins <- as.integer (1/resolution)
    df <- (fmax-fmin) / bins
    fdtl <- fmin + df * (0:bins)
    freq <- exp (fdtl)
    psComplex <- memEstimate (freq / Rate, A) / Rate
    ps <- 2 * Rate * Mod (psComplex)^2
    fpf <- freq * ps
  }
  tasAverage <- mean(.data$TASX, na.rm=TRUE)
  if (EDR) {
    ps <- fpf / freq
    fpf <- (2*pi/tasAverage)*(1.5*ps)^1.5 * freq^2.5
  }
  if(smoothBins > 9) {
    bs1 <- binStats(data.frame(fpf, log(freq)), bins=smoothBins)
    bs1 <- rbind (bs1, data.frame(xc=bs1$xc[nrow(bs1)], ybar=bs1$ybar[nrow(bs1)],
      sigma=bs1$sigma[nrow(bs1)], nb=1))
    bs1 <- bs1[!is.na(bs1$ybar),]
    freq <- exp(bs1$xc)
    fpf <- bs1$ybar
    bs1$sigma <- ifelse (bs1$nb > 2, bs1$sigma/sqrt(bs1$nb), NA)
    rna <- is.na(bs1$sigma)
    bs1$sigma[rna] <- bs1$ybar[rna] / 2
    # bs1 <<- bs1
  }
  
  DF <- data.frame(freq, fpf)
  if (!is.na(add[1])) {ADD <- add}
  if (is.na(ADD[1])) {
    ## first call: redefine VSpecDF
    assign('.VSpecDF1', DF, envir=.GlobalEnv)
    labx <- 'frequency [Hz]'
    if (EDR) {
      # laby <- sprintf('eddy dissipation rate for %s', .Variable)
      laby <- expression(paste("eddy dissipation rate [m"^"2","s"^"-3","]"))
    } else {
      laby <- sprintf('variance spectrum fP(f) for %s', .Variable)
    }
    # xlim <- c(0.001,15)
    # ylim <- c(0.001, 1)  ## now an input argument
    g <- ggplot(data=.VSpecDF1)         
    g <- g + geom_path (aes(x=freq, y=fpf, colour=V), na.rm=TRUE) +  
      xlab(labx) + ylab (laby) 
    if (is.na(col)) {col <- 'blue'}
    .clinesVSpec <- col
    names(.clinesVSpec) <- V
    .clinesVSpec <<- .clinesVSpec    
  } else {
    ## assign name based on elements in clinesVSpec
    N <- length(.clinesVSpec) + 1
    if (is.na(col)) {
      if (N == 2) {col <- 'forestgreen'}
      if (N == 3) {col <- 'black'}
      if (N == 4) {col <- 'brown'}
    }
    nc <- names(.clinesVSpec)
    .clinesVSpec <- c(.clinesVSpec, col)
    names(.clinesVSpec) <- c(nc, V)
    .clinesVSpec <<- .clinesVSpec
    VName <- sprintf('.VSpecDF%d', N)
    assign(VName, DF, pos=.GlobalEnv)
    if (N == 2) {
      .VSpecVar2 <<- V
      g <- ADD + geom_path (aes(x=freq, y=fpf, colour=.VSpecVar2), data=get(VName), na.rm=TRUE)
    } else if (N == 3) {
      .VSpecVar3 <<- V
      g <- ADD + geom_path (aes(x=freq, y=fpf, colour=.VSpecVar3), data=get(VName), na.rm=TRUE)
    } else if (N == 4) {
      .VSpecVar4 <<- V
      g <- ADD + geom_path (aes(x=freq, y=fpf, colour=.VSpecVar4), data=get(VName), na.rm=TRUE)  
    }
  }
  g <- suppressMessages(g + scale_colour_manual (name='', values=.clinesVSpec))
  if (showErrors > 0) {
    if (smoothBins > 9) {
      bse <- data.frame(x=exp(bs1$xc), ymin=bs1$ybar-showErrors*bs1$sigma, ymax=bs1$ybar+showErrors*bs1$sigma)
      bse$ymin[bse$ymin < ylim[1]] <- ylim[1]
    } else {
      coverage <- pnorm(showErrors)-pnorm(-showErrors)  ## 1-sigma, 0.68269
      tail <- 1 - coverage
      if (type == 'spectrum') {
        df <- S$df
      } else if (type == 'Welch') {
        df <- 1.46 * (S2$segments + 1)
      } else if (type == 'MEM') {
        df <- length(v) / poles
      }
      uq <- 1 - tail * pchisq(df, df, lower.tail = FALSE)
      lq <- tail * pchisq(df, df, lower.tail=TRUE)
      ci <- 1/(qchisq(c(uq, lq), df)/df)
      lower.limit <- qchisq (pnorm(-showErrors), df) / df
      upper.limit <- qchisq (pnorm(showErrors), df) / df
      bse <- data.frame(x=freq, ymin=lower.limit*fpf, ymax=upper.limit*fpf)
    }
    # g <- g + geom_ribbon(data=bs1, aes(x=exp(xc), ymin=max(ylim[1], ybar-showErrors*sigma), ymax=ybar+showErrors*sigma),
    #   fill='cyan', alpha=0.25, show.legend=FALSE, inherit.aes=FALSE, na.rm=TRUE)
    g <- g + geom_ribbon(data=bse, aes(x=x, ymin=ymin, ymax=ymax),
      fill='gray50', alpha=0.5, show.legend=FALSE, inherit.aes=FALSE, na.rm=TRUE)
    # bs1$xc <- exp(bs1$xc)
  }
  if (is.na(ADD[1])) {
    g <- g + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=4), #limits = xlim, 
      labels = trans_format("log10", math_format(10^.x))) +           
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n=4), #limits = ylim,             
        labels = trans_format("log10", math_format(10^.x))) + 
      annotation_logticks(sides='trbl') + 
      coord_cartesian(xlim=xlim, ylim=ylim) 
    # g <- g + theme(panel.grid.minor = element_line(colour = "black"))
    if (EDR) {  ## add line showing highest-decade average EDR
      imx <- length(freq)
      imn <- which (freq > freq[imx]/20)[1]
      aveEDR <- mean(fpf[imn:imx], na.rm=TRUE)
      ttl <- sprintf ('EDR=%.2e', aveEDR)
      DFL <- data.frame(x=c(freq[imn], freq[imx]), y=rep(aveEDR, 2))
      g <- g + geom_path(data=DFL, aes(x=x, y=y), lwd=1.5, colour='red')
      g <- g + ggtitle (bquote(.(ttl) ~ ' m'^2 ~ 's'^-3))
      # g <- g + ggtitle(sprintf(' mean eddy dissipation rate %.2e m^2/s^3', aveEDR))
    } else {
      for (i in (-8:0)) {
        a = ae * 10.^(i*(2/3)) * tasAverage^(2/3)
        lw = ifelse(i == -4, 1.2, 0.5)
        DFL <- data.frame(x=xlim, y=c(a/xlim[1]^(2/3), a/xlim[2]^(2/3)))
        # print(DFL)
        g <- g + geom_path (data=DFL, aes(x=x, y=y), colour='darkorange', lwd=lw, lty=3)
      }
    }
    if (WavelengthScale) {
      yl <- c(ylim[1]*1.4, ylim[1]*2.1)
      lclr <- 'slategrey'
      for (j1 in c(10, 100, 1000, 10000, 100000)) {
        DFL2 <- data.frame(x=rep(tasAverage/j1, 2), y=yl)
        g <- g + geom_path(data=DFL2, aes(x=x, y=y), colour=lclr, lwd=1.0)
        if (j1 != 100000) {
          for (j2 in 2:9) {
            DFL2 <- data.frame(x=rep(tasAverage/(j1*j2),2), y=yl)
            g <- g + geom_path(data=DFL2, aes(x=x, y=y), colour=lclr, lwd=0.6)
          }
        }
      }
      DFL2 <- data.frame (x=tasAverage*c(1/10, 1/100000), y=rep(yl[1], 2))
      g <- g + geom_path(data=DFL2, aes(x=x, y=y), colour=lclr, lwd=1.0)
      g <- g + annotate("text", 
        x = tasAverage*c(1/100000, 1/10000, 1/1000, 1/100, 1/10), 
        y = rep(yl[2]*1.5,5), label = c("100 km", "10 km", "1 km", "100 m", " "))
    }
    # g <- g + theme_WAC()
  }
  
  return(g)
}


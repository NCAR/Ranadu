#' @title contourPlot
#' @description A frequency-plot display of two variables, with frequency denoted by colors.
#' @details For two variables, supplied as the first two columns in a data.frame,
#' this function divides the area spanning the range of the two variables (or a specified 
#' range) in to a grid with specified resolution and then partitions the data into bins
#' in that grid. It then uses the R function "filled.contour()" to display the contours
#' of the area covered by the grid as colored regions, with a legend showing the
#' correspondence between color and number of events in each bin. The function also
#' accepts a function argument ("addLine") that can be used to draw a line on the
#' resulting plot, for example to denote the result of a fit to the data.
#' @aliases contourPlot, ContourPlot
#' @author William Cooper
#' @export contourPlot
#' @importFrom graphics lines
#' @param D A data.frame with at least two columns, with the first two containing
#' the two veriables to be used as the abscissa and ordinate coordinates for the plot. 
#' These two variables should have names, and those names will be used in axis labels.
#' @param Ndelta A two-component integer vector specifying the number of bins to use
#' for the respective abscissa and ordinate coordinates of the display grid. The 
#' default is c(50,50).
#' @param logCounts If TRUE (the default), logarithmic intervals are used when
#' specifying and labeling the contours.
#' @param cols A list of colors to use. The default is c("gray", "skyblue",
#' "forestgreen", "darkorange", "black"). The number of colors supplied determines
#' the number of separate contours that are shown.
#' @param xlim A two-component numeric vector specifying the abscissa limits. 
#' The default is to use the range of the first column in the data.frame.
#' @param ylim As for xlim but for the ordinate limits.
#' @param xlab Character string to use for the label on the abscissa. The value can also be an R expression. 
#' Default is the name of the first variable.
#' @param ylab Character string to use for the label on the ordinate. The value can also be an R expression.
#' Default is the name of the second variable.
#' @param title Title for the main part of the plot. Default is no title.
#' @param addLine A function that specifies the ordinate as a function of the
#' abscissa for a line to be added to the plot. The function should have
#' arguments cf and xl, where cf is a set of coefficients to be used to
#' calculate the ordinate and xl is the corresponding abscissa. Here is an
#' example: 
#'      cf <- coef(lm(D[,2] ~ D[,1]))
#'      aL <- function(cf, xl) {return(cf[1]+cf[2]*xl)}
#'      contourPlot(..., addLine=aL)
#'@param cf The set of coefficients to be used with function addLine() if provided. Default
#'is c(0,1).
#' @examples 
#' contourPlot(RAFdata[,c('RTH1', 'RTH2')])
#' contourPlot(RAFdata[,c('RTH1', 'RTH2')], Ndelta=c(20,20), logCount=FALSE, 
#' title='Comparison of Two Temperature Measurements', 
#' addLine=function(cf,xl){return(xl)})

contourPlot <- function(D, Ndelta=c(50, 50), logCounts=TRUE, cols=NA, 
                        xlim=NA, ylim=NA, xlab=NA, ylab=NA, title=NA, addLine=NA, cf=c(0,1)) {
  if (!is.data.frame(D) || ncol(D) < 2) {
    print ("error in contourPlot call, first argument must be a data,frame of at least two columns")
    return()
  }
  nm <- names(D)
  if (!is.expression(xlab)) {
    if (is.na(xlab[1])) {xlab <- nm[1]}
  } 
  if (!is.expression(ylab)) {
    if(is.na(ylab[1])) {ylab <- nm[2]}
  } 
  if (is.na(cols[1])) {
  cols <- c('gray', 'skyblue', 'forestgreen', 'darkorange', 'black')
  }
  D <- D[!is.na(D[,1]) & !is.na(D[,2]),]
  if (is.na(xlim[1])) {
    xlow <- min(D[,1]); xhigh <- max(D[,1])
  } else {
    xlow <- xlim[1]; xhigh <- xlim[2]
  }
  if (is.na(ylim[1])) {
    ylow <- min(D[,2]); yhigh <- max(D[,2])
  } else {
    ylow <- ylim[1]; yhigh <- ylim[2]
  }
  ix <- iy <- rep(0, nrow(D))
  deltax <- (xhigh-xlow)/Ndelta[1]
  deltay <- (yhigh-ylow)/Ndelta[2]
  xl <- seq(xlow, xhigh, by=deltax); yl <- seq(ylow, yhigh, by=deltay)
  ## indices are lower limits of the bins; add 1/2 bin to position
  ## no values are binned into the last bin; values smaller than the first bin limit go in ix=0
  for (i in 1:nrow(D)) { 
    ix[i] <- which(xl > D[i,1])[1] - 1
    iy[i] <- which(yl > D[i,2])[1] - 1
  }
  xlm <- xl + deltax/2
  ylm <- yl + deltay/2
  xlm <- xlm[-length(xlm)]
  ylm <- ylm[-length(ylm)]
  A <- rep(0, length(xlm) * length(ylm))
  dim(A) <- c(length(xlm), length(ylm))
  for (i in 1:nrow(D)) {
    if (!is.na(ix[i]) & !is.na(iy[i]) & (ix[i] > 0) & (iy[i] > 0)) {
      A[ix[i], iy[i]] <- A[ix[i], iy[i]] + 1 
    }
  }
  ## This was used for checking plot locations:
  # ixx <- which(xl > 10)[1] - 1
  # iyy <- which(yl > -0.5)[1] - 1
  # A[ixx,iyy] <- A[ixx,iyy] + 300
  # A[ixx-1,iyy-1] <- A[ixx=1,iyy-1] + 300
  lbls <- rep('', length(cols)+1)
  if (logCounts) {
    A[A <= 0] <- 0.1
    A <- log10(A)
    ## get the range in A, and scale
    Amax <- max(A, na.rm=TRUE)
    lv <- rep(1, length(cols)+1)
    lvMax <- ceiling(Amax)
    for (i in (length(cols)+1):1) {
      lv[i] <- 10^lvMax
      lbls[i] <- sprintf('%.0f', lv[i])
      lvMax <- ifelse (lvMax %% 1, floor(lvMax-0.1), lvMax+log10(.3))
    }
    while(lv[1] < 1) {
      lv <- lv[-1]
      lbls <- lbls[-1]
    }
    lvl <- log10(lv)
  } else {
    Amax <- max(A, na.rm=TRUE)
    ## find reasonable upper limit:
    Amax <- signif(Amax*1.1, 1)
    lvl <- Amax * seq(0, 1, by=1/(length(cols))) 
    lbls <- sprintf('%.0f', lvl)
  }
  mpar <- par()
  par(mgp=c(2.8,1,0), mar=c(5,4,3,0)+0.1, oma=c(0,0.5,0,0.5))
  # filled.contour(xlim, ylim, A,
  #   levels=lv, col=cols)
  ## find y as a function of x:
  if (!is.function(addLine)) {
    filled.contour(xlm, ylm, A, levels=lvl, col=cols, key.title=title(main='#/bin', cex.main=0.8),
                   plot.title=title(main=title, xlab=xlab, ylab=ylab), 
                   plot.axes={axis(1, tck=0.02); axis(2, tck=0.02); axis(3, labels=NA, tck=0.02);
                     axis(4, labels=NA, tck=0.02)}, 
                   key.axes=axis(4, at=lvl, labels=lbls, tck=0.01, cex.axis=0.8))
  } else {
    FN <- match.fun(addLine)
    filled.contour(xlm, ylm, A, levels=lvl, col=cols, key.title=title(main='#/bin', cex.main=0.8),
                   plot.title=title(main=title, xlab=xlab, ylab=ylab),
                   plot.axes={axis(1, tck=0.02); axis(2, tck=0.02); axis(3, labels=NA, tck=0.02);
                     axis(4, labels=NA, tck=0.02);
                     lines(xl, FN(cf, xl), col='red', lwd=2, lty=2)},
                   key.axes=axis(4, at=lvl, labels=lbls, tck=0.01, cex.axis=0.8))
  }
  op <- par (mgp=mpar$mgp, mar=mpar$mar, oma=mpar$oma)  ## restore to margin settings when entering function
}

## Example:
# library(Ranadu)
# Project <- 'WECAN'
# Flight <- 5
# D <- getNetCDF(sprintf('%s%s/%srf%02d.nc',DataDirectory(), Project, Project, Flight),
#                standardVariables(c('ATH1', 'ATH2', 'ATF1', 'TASX')))
# D$DT <- with(D, ATF1-ATH1) 
# D <- D[D$TASX > 80, ]
# nm <- c('ATH1', 'DT')
# cf <- coef(lm(D[, nm[2]] ~ D[, nm[1]]))   ## + I(D[, nm[1]]^2)))
# # print(cf)
# aL <- function(cf,xl) {
#   yln <- cf[1]+cf[2]*xl ##+ cf[3]*xl^2
#   return(yln)
# }
# contourPlot(D[, c('ATH1', 'DT')], addLine=aL)



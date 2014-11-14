#' @title plotWAC
#' @description Convenience routine for plots
#' @details Sets some plot defaults and calls plot and axis
#' @aliases plotWAC
#' @author William Cooper
#' @export plotWAC
#' @param x Usually, Time from a data.frame; a vector of abscissa values. Optionally,
#' a data.frame containing multiple variables to plot and superceding parameter y.
#' @param y A vector of ordinate values for points to plot. If the first parameter
#' is a data.frame, this argument is ignored.
#' @param col Color to pass to plot (default: blue)
#' @param xlab Label for the abscissa, to pass to plot (default: "TIME [UTC]")
#' @param ylab Label for the ordinate, to pass to plot (default: "")
#' @param lwd Line width to pass to plot (default: 2)
#' @param type Line type to pass to plot (default: "l")
#' @param legend.position For multiple-line calls with a data.frame, this parameter
#' specifies the position for the legend. The default is to produce no legend, but
#' this can still be added after the plotWAC call.
#' @param ... Additional arguments to pass to plot(), but don't include col, xlab, ylab, lwd, type, xaxt or yaxt
#' @examples 
#' \dontrun{plotWAC (Time, TASX, ylab="TAS")}
#' \dontrun{plotWAC (Time, PSXC, lty=2)}
plotWAC <- function (x, y=NA, col="blue", xlab="TIME [UTC]", 
                     ylab="", lwd=2, type='l', legend.position=NA, ...) {
  if (is.data.frame (x)) {
    if (ylab == "") {
      ylab <- names(x)[2]
    }
    plot (x[ ,1], x[ ,2], xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, lwd=lwd, 
          type=type, col=col, xaxs="r", yaxs="r", ...)
    if (length(x) > 2) {
      colrs <- c(col, 'red', 'darkgreen', 'cyan', 'darkorange')
      for (j in 3:min(6, length(x))) {
        lines(x[ ,1], x[ ,j], lwd=lwd, col=colrs[j-1], ...)
      }
      if (!is.na(legend.position)) {
        legend (legend.position, legend=names (x)[2:length(x)], 
                text.col=colrs[1:(length(x)-1)], lwd=lwd, 
                col=colrs[1:(length(x)-1)])
      }
    }
    if (!is.expression(xlab)) {
      if (xlab == "TIME [UTC]") {
        if (length(x[, 1]) < 120) {          # needs revision for high-rate data
          axis.POSIXct(1, x[, 1], format='%H:%M:%S', tck=0.02)
        } else {
          axis.POSIXct(1,x[, 1], format='%H:%M', tck=0.02)
        }
        axis.POSIXct(3,x[, 1], labels=NA, tck=0.02)
      } else {
        axis(1,tck=0.02)
        axis(3,labels=NA,tck=0.02)
      }
    } else {
      axis(1,tck=0.02)
      axis(3,labels=NA,tck=0.02)
    }
    axis(2,tck=0.02)
    axis(4,labels=NA,tck=0.02)
  } else {
    plot(x, y, xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, lwd=lwd, 
         type=type, col=col, xaxs="r", yaxs="r", ...)
  
    if (!is.expression(xlab)) {
      if (xlab == "TIME [UTC]") {
        if (length(x) < 120) {          # needs revision for high-rate data
          axis.POSIXct(1, x, format='%H:%M:%S', tck=0.02)
        } else {
          axis.POSIXct(1,x, format='%H:%M', tck=0.02)
        }
        axis.POSIXct(3,x, labels=NA, tck=0.02)
      } else {
        axis(1,tck=0.02)
        axis(3,labels=NA,tck=0.02)
      }
    } else {
      axis(1,tck=0.02)
      axis(3,labels=NA,tck=0.02)
    }
    axis(2,tck=0.02)
    axis(4,labels=NA,tck=0.02)
  }
}

#' @title lineWAC
#' @description Convenience routine for adding lines to plots
#' @details Sets some plot defaults and calls points; assumes a plot with axes has already been generated to which to add this line.
#' @aliases lineWAC
#' @author William Cooper
#' @export lineWAC
#' @param x Usually, Time from a data.frame; a vector of abscissa values. Default: Data$Time
#' @param y A vector of ordinate values for points to plot. 
#' @param col Color to pass to plot (default: darkgreen)
#' @param lwd Line width to pass to plot (default: 2)
#' @param type Line type to pass to plot (default: "l")
#' @param ... Additional arguments to pass to point()
#' @examples 
#' \dontrun{lineWAC (Time, TASX, col='darkgreen')}
#' \dontrun{lineWAC (Time, PSXC, lty=2)}
lineWAC <- function (x, y, col="blue", lwd=2, type='l', ...) {
  points(x, y, lwd=lwd, type=type, col=col, ...)
  return ()
}


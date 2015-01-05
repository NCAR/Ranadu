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
#' @param col Color to pass to plot (default: blue). Can be multiple values
#' to set colors for multiple variables in data.frame mode. There are defaults
#' ('blue', 'darkgreen', 'red', 'skyblue', 'darkorange') but values provided with
#' this parameter precede that sequence of colors.
#' @param xlab Label for the abscissa, to pass to plot (default: "TIME [UTC]")
#' @param ylab Label for the ordinate, to pass to plot (default: "" or, for data.frame
#' mode, first variable name)
#' @param lwd Line width to pass to plot (default: 1); can be a vector
#' @param type Plot type to pass to plot (default: "l")
#' @param lty Line type to pass to plot (default: 1). Can be a vector for multiple
#' variables provided in data.frame mode.
#' @param logxy Set to 'y' for log axes. This is provided (vs log='y') because it
#' makes possible translation of axis labels to 10^i format.
#' @param legend.position For multiple-line calls with a data.frame, this parameter
#' specifies the position for the legend. The default is 'bottomright'. The legend
#' can also be suppressed by setting this parameter to NA. Then a legend can still 
#' be added after the plotWAC call.
#' @param ... Additional arguments to pass to plot(), but don't include col, xlab, ylab, lwd, type, xaxt or yaxt
#' @examples 
#' \dontrun{plotWAC (Time, TASX, ylab="TAS")}
#' \dontrun{plotWAC (Time, PSXC, lty=2)}
#' \dontrun{plotWAC (Data[, c("Time", "PSXC")])}
plotWAC <- function (x, y=NA, col="blue", xlab="TIME [UTC]", 
                     ylab="", lwd=2, type="l", lty=1, logxy='',
                     legend.position="bottomright", ...) {
  if (is.data.frame (x)) {
    if (ylab == "") {
      ylab <- names(x)[2]
    }
    plot (x[ ,1], x[ ,2], xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, lwd=lwd, 
          lty=lty, type=type, col=col, xaxs="r", yaxs="r", log=logxy, ...)
    if ('y' %in% logxy) {
      aty <- axTicks(2)
      labs <- sapply(log10(aty),function(i)
      as.expression(bquote(10^ .(i)))
      )
    }
    if (length(x) > 2) {
      colrs <- c(col, 'darkgreen', 'red', 'skyblue', 'darkorange')
      lwd <- c(lwd, rep(1,5))
      lty <- c(lty, rep(1,5))
      for (j in 3:min(6, length(x))) {
        lines(x[ ,1], x[ ,j], lwd=lwd[j-1], lty=lty[j-1], col=colrs[j-1], ...)
      }
      if (!is.na(legend.position)) {
        legend (legend.position, legend=names (x)[2:length(x)], 
                text.col=colrs[1:(length(x)-1)], lwd=lwd, lty=lty, cex=0.50, 
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
    if ('y' %in% logxy) {
      axis(2,at=aty,labels=labs)
    } else {
      axis(2,tck=0.02)
    }
    axis(4,labels=NA,tck=0.02)
  } else {
    plot(x, y, xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, lwd=lwd, 
         type=type, col=col, xaxs="r", yaxs="r", log=logxy, ...)
  
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
    if ('y' %in% logxy) {
      axis(2,at=aty,labels=labs)
    } else {
      axis(2,tck=0.02)
    }
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
}


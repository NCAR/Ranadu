#' @title plotWAC
#' @description Convenience routine for plots
#' @details Sets some plot defaults and calls plot and axis
#' @aliases plotWAC
#' @author William Cooper
#' @export plotWAC
#' @param x Usually, Time from a data.frame; a vector of abscissa values. Default: Data$Time
#' @param y A vector of ordinate values for points to plot. 
#' @param col Color to pass to plot (default: blue)
#' @param xlab Label for the abscissa, to pass to plot (default: "TIME [UTC]")
#' @param ylab Label for the ordinate, to pass to plot (default: "")
#' @param lwd Line width to pass to plot (default: 2)
#' @param type Line type to pass to plot (default: "l")
#' @param ... Additional arguments to pass to plot(), but don't include col, xlab, ylab, lwd, type, xaxt or yaxt
#' @examples 
#' \dontrun{plotWAC (Time, TASX, ylab="TAS")}
#' \dontrun{plotWAC (Time, PSXC, lty=2)}
plotWAC <- function (x, y, col="blue", xlab="TIME [UTC]", 
                     ylab="", lwd=2, type='l', ...) {
  plot(x, y, xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, lwd=lwd, 
       type=type, col=col, xaxs="r", yaxs="r", ...)
  if (!is.expression(xlab)) {
    if (xlab == "TIME [UTC]") {
      axis.POSIXct(1,x, format='%H:%M', tck=0.02)
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
  return ()
}

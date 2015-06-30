#' @title ggplotWAC
#' @description Convenience routine for plots
#' @details Sets some plot defaults and calls ggplot and theme_WAC()
#' @aliases ggplotWAC
#' @author William Cooper
#' @import ggplot2 ggthemes grid 
#' @export ggplotWAC
#' @param .data A data.frame containing vectors to plot. The first will be the
#' abscissa and the remainder ordinate vectors to plot vs the abscissa.
#' @param col Color to pass to plot (default: blue). Can be multiple values
#' to set colors for multiple variables in data.frame mode. There are defaults
#' ('blue', 'darkgreen', 'red', 'skyblue', 'darkorange') but values provided with
#' this parameter precede that sequence of colors.
#' @param xlab Label for the abscissa, to pass to plot (default: "TIME [UTC]")
#' @param ylab Label for the ordinate, to pass to plot (default: second
#' variable name)
#' @param lwd Line width to pass to plot (default: 1); can be a vector
#' @param lty Line type to pass to plot (default: 1). Can be a vector.
#' variables provided in data.frame mode.
#' @param logxy Set to 'y' for log axes. This is provided (vs log='y') because it
#' makes possible translation of axis labels to 10^i format.
#' @param legend.position This parameter specifies the position for the 
#' legend. The default is 'bottomright'. The legend
#' can also be suppressed by setting this parameter to NA. Then a legend can still 
#' be added after the ggplotWAC call.
#' @param ... Additional arguments to pass to plot(), but don't include col, xlab, ylab, lwd, type, xaxt or yaxt
#' @examples 
#' \dontrun{ggplotWAC (data.frame ("Time"=Time, "TASX"=TASX), ylab="TAS")}
#' \dontrun{plotWAC (Data[, c(Time, PSXCi)], lwd=2)}
#' \dontrun{plotWAC (Data[, c("Time", "PSXC")])}
ggplotWAC <- function (.data, col="blue", xlab="TIME [UTC]", 
                       ylab="", lwd=1.2, lty=1, logxy='',
                       legend.position="bottomright", ...) {
  if (!is.data.frame (.data)) {
    print ("Error, first argument to ggplotWAC must be a data.frame")
  } else {
    if (!is.expression(ylab) && (ylab == "")) {
      ylab <- names(.data)[2]
    }
    ## protect against all-missing variables
    for (j in 2:min(6, length(.data))) {
      if (!any (!is.na (.data[, j]))) {
        .data[1, j] <- -32767.
        .data[2, j] <- 32767.
      }
    }
    yrange <- c(min(.data[ ,2], na.rm=TRUE), max(.data[ ,2], na.rm=TRUE))
    if (ncol(.data) > 2) {
      for (j in 3:min(6, ncol(.data))) {
        if (any (!is.na(.data[ ,j]))) {
          yl <- min(.data[ ,j], na.rm=TRUE)
          yh <- max(.data[ ,j], na.rm=TRUE)
          if (yl < yrange[1]) {yrange[1] <- yl}
          if (yh > yrange[2]) {yrange[2] <- yh}
        }
      }
    }
    colrs <- c(col, 'darkgreen', 'red', 'skyblue', 'darkorange')
    clr <- names(.data)[2:ncol(.data)]
    lwd <- c(lwd, rep(1,5))
    lty <- c(lty, rep(1,5))
    g <- ggplot (data=.data, aes(x=eval (parse (text=names(.data)[1]))), na.rm=TRUE)
    if (names(.data)[1] == "Time") {
      g <- g + xlab ("Time [UTC]")
    } else {
      g <- g + xlab (names(.data)[1])
    } 
    if (ylab == '') {
      ylab <- names (.data)[2]
    }
    g <- g + ylab(ylab)
    nv <- names (.data)
    for (j in 2:min(ncol(.data), 6)) {
      a <- sprintf ("aes (y=%s, colour='%s', size='%s', lty='%s')", nv[j], nv[j], nv[j], nv[j])
      g <- g + geom_line (eval (parse (text=a)))
    }
    g <- g + scale_colour_manual("", 
                                 labels = clr,
                                 values = colrs)
    g <- g + scale_size_manual ("", labels=clr, values = lwd)
    g <- g + scale_linetype_manual ("", labels=clr, values = lty)
    g <- g + theme_WAC()
    # suppressWarnings (print (g))

## left from plotWAC: implement someday?
#       if (!is.expression(xlab)) {
#         # get data.rate
#         data.rate <- 1
#         itg <- x[!is.na(x[,1]), 1]  # protect against missing values at start
#         if ((itg[2]-itg[1]) <= 0.04) {data.rate <- 25}
#         if ((itg[2]-itg[1]) <= 0.02) {data.rate <- 50}
#         
#         # print (sprintf (" data.rate is %d", data.rate))
#         if (xlab == "TIME [UTC]") {
#           if (length(x[, 1]) < 180*data.rate+2) {          # needs revision for high-rate data
#             axis.POSIXct(1, x[, 1], format='%H:%M:%S', tck=0.02)
#           } else {
#             axis.POSIXct(1,x[, 1], format='%H:%M', tck=0.02)
#           }
#           axis.POSIXct(3,x[, 1], labels=NA, tck=0.02)
#         } else {
#           axis(1,tck=0.02)
#           axis(3,labels=NA,tck=0.02)
#         }
#       } else {
#         axis(1,tck=0.02)
#         axis(3,labels=NA,tck=0.02)
#       }
#       if ('y' %in% logxy) {
#         axis(2,at=aty,labels=labs)
#       } else {
#         axis(2,tck=0.02)
#       }
#       axis(4,labels=NA,tck=0.02)
#     } 
  }
  return (g)
}


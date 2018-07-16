#' @title plotWAC
#' @description Convenience routine for plots
#' @details Sets some plot defaults and calls plot and axis. For a data.frame
#' argument, sets plot limits to cover range of all variables in the data.frame.
#' @aliases plotRAF
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
#' @param pch Character number to use for scatterplots. Ignored if plot type is 'l'.
#' @param cex Character size to use for scatterplots. Ignored if plot type is 'l'.
#' @param legend.position For multiple-line calls with a data.frame, this parameter
#' specifies the position for the legend. The default is 'bottomright'. The legend
#' can also be suppressed by setting this parameter to NA. Then a legend can still 
#' be added after the plotWAC call.
#' @param ... Additional arguments to pass to plot(), but don't include col, xlab, ylab, lwd, type, xaxt or yaxt
#' @examples 
#' plotWAC (RAFdata[, c("Time", "ATX", "DPXC")], legend.position="right")
#' plotWAC (subset (RAFdata,, c(Time, TASX, GGVNS)), legend.position='topright')
#' \dontrun{plotWAC (Time, TASX, ylab="TAS")}
#' \dontrun{plotWAC (Time, PSXC, lty=2)}
#' \dontrun{plotWAC (Data[, c("Time", "PSXC")])}
#' \dontrun{plotWAC (subset (Data,,c(Time,ATX,DPXC)))}
#' \dontrun{plotWAC (subset (Data,,c(ATX,DPXC)))}

plotWAC <- function (x, y=NA, col="blue", xlab="TIME [UTC]", 
                     ylab="", lwd=2, type="l", lty=1, logxy='', pch=20, cex=1, 
                     legend.position="bottomright", ...) {
  par(cex.axis=2, cex.lab=2) # This line bumps up font sizes for axes
  if (is.data.frame (x)) {
    if (!is.expression(ylab) && (ylab == "")) {
      ylab <- names(x)[2]
    }
    ## protect against all-missing variables
    for (j in 2:min(7, length(x))) {
      if (!any (!is.na (x[, j]))) {
        x[1, j] <- -32767.
        x[2, j] <- 32767.
      }
    }
    yrange <- c(min(x[ ,2], na.rm=TRUE), max(x[ ,2], na.rm=TRUE))
    if (length(x) > 2) {
      for (j in 3:min(7, length(x))) {
        if (any (!is.na(x[ ,j]))) {
          yl <- min(x[ ,j], na.rm=TRUE)
          yh <- max(x[ ,j], na.rm=TRUE)
          if (yl < yrange[1]) {yrange[1] <- yl}
          if (yh > yrange[2]) {yrange[2] <- yh}
        }
      }
    }
    ## correct for offset if abscissa is Time, because value is centered in interval
    if (grepl ('TIME', xlab) || grepl ('Time', xlab)) {
      data.rate <- 1
      itg <- x[!is.na(x), 1]  # protect against missing values at start
      if ((itg[2]-itg[1]) <= 0.045) {data.rate <- 25}
      if ((itg[2]-itg[1]) <= 0.025) {data.rate <- 50}
      x[, 1] <- x[, 1] + 0.5 / data.rate
    }
    if (!("ylim" %in% names(list(...))) && (yrange[1] != yrange[2])) {
      plot (x[ ,1], x[ ,2], xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, 
            lwd=lwd, lty=lty, type=type, col=col[1], xaxs="r", yaxs="r", 
            log=logxy, ylim=c(yrange[1], yrange[2]), pch=pch[1], cex=cex[1], ...)
    } else {
      plot (x[ ,1], x[ ,2], xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, 
            lwd=lwd, lty=lty, type=type, col=col[1], xaxs="r", yaxs="r", 
            log=logxy, pch=pch[1], cex=cex[1], ...)
    }
    if (grepl ('x', logxy)) {
      atx <- axTicks(1)
      labs <- sapply(
        log10(atx),function(i)
          as.expression(bquote(10^ .(i)))
      )
    }
    if (grepl ('y', logxy)) {
      aty <- axTicks(2)
      labs <- sapply(
        log10(aty),function(i)
          as.expression(bquote(10^ .(i)))
      )
    }
   # Color palette must be defined for the legend labeling to work
     colrs<-c(col, 
             rgb(027,158,119,maxColorValue=255),
             rgb(117,112,179,maxColorValue=255),
             rgb(231,041,138,maxColorValue=255),
             rgb(102,166,030,maxColorValue=255),
             rgb(230,171,002,maxColorValue=255),
             rgb(217,095,002,maxColorValue=255),'gray40' )

    if (length(x) > 2) {
      colrs <- c(col, 'darkgreen', 'red', 'skyblue', 'darkorange', 'gray40')
      lwd <- c(lwd, rep(1,5))
      lty <- c(lty, rep(1,5))
      for (j in 3:min(7, length(x))) {
        if (type == 'l') {
          lines(x[ ,1], x[ ,j], lwd=lwd[j-1], lty=lty[j-1], col=colrs[j-1], ...)
        } else {
          points (x[, 1], x[, j], col=colrs[j-1], pch=pch[j-1], cex=cex[j-1], ...)
        }
      }
    }
    if (!is.na(legend.position)) {
        legend (legend.position, legend=names (x)[2:length(x)], 
                text.col=colrs[1:(length(x)-1)], lwd=lwd, lty=lty, cex=0.80, 
                col=colrs[1:(length(x)-1)])
    }
    if (!is.expression(xlab)) {
      # get data.rate
      data.rate <- 1
      itg <- x[!is.na(x[,1]), 1]  # protect against missing values at start
      if ((itg[2]-itg[1]) <= 0.04) {data.rate <- 25}
      if ((itg[2]-itg[1]) <= 0.02) {data.rate <- 50}
      
      # print (sprintf (" data.rate is %d", data.rate))
      if (xlab == "TIME [UTC]") {
        if (length(x[, 1]) < 180*data.rate+2) {          # needs revision for high-rate data
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
    ## correct for offset if abscissa is Time, because value is centered in interval
    if (grepl ('TIME', xlab) || grepl ('Time', xlab)) {
      data.rate <- 1
      itg <- x[!is.na(x)]  # protect against missing values at start
      if ((itg[2]-itg[1]) <= 0.045) {data.rate <- 25}
      if ((itg[2]-itg[1]) <= 0.025) {data.rate <- 50}
      x <- x + 0.5 / data.rate
    }
    plot(x, y, xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, lwd=lwd, 
         type=type, col=col, xaxs="r", yaxs="r", log=logxy, ...)
    
    if (!is.expression(xlab)) {
      if (xlab == "TIME [UTC]") {
        # get data.rate
        data.rate <- 1
        itg <- x[!is.na(x)]  # protect against missing values at start
        if ((itg[2]-itg[1]) <= 0.04) {data.rate <- 25}
        if ((itg[2]-itg[1]) <= 0.02) {data.rate <- 50}
        # print (sprintf (" data.rate is %d", data.rate))
        if (length(x) < 180*data.rate+2) {         
          axis.POSIXct(1, x, format='%H:%M:%S', tck=0.02)
        } else {
          axis.POSIXct(1, x, format='%H:%M', tck=0.02)
        }
        axis.POSIXct(3, x, labels=NA, tck=0.02)
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
#' @importFrom graphics points plot text lines axTicks legend axis.POSIXct axis
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
  ## correct for offset if abscissa is Time, because value is centered in interval
  if (grepl ('TIME', xlab) || grepl ('Time', xlab)) {
    data.rate <- 1
    itg <- x[!is.na(x)]  # protect against missing values at start
    if ((itg[2]-itg[1]) <= 0.045) {data.rate <- 25}
    if ((itg[2]-itg[1]) <= 0.025) {data.rate <- 50}
    x <- x + 0.5 / data.rate
  }
  points(x, y, lwd=lwd, type=type, col=col, ...)
}


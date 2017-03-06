
#' @title ggplotWAC
#' @description Convenience routine for plots
#' @details Sets some plot defaults and calls ggplot and theme_WAC()
#' @aliases ggplotWAC
#' @author William Cooper
#' @import ggplot2 ggthemes reshape2
#' @importFrom grid viewport
#' @export ggplotWAC
#' @param .data A data.frame containing vectors to plot. The first will be the
#' abscissa and the remainder ordinate vectors to plot vs the abscissa.
#' @param col Color to pass to plot (default: blue). Can be multiple values
#' to set colors for multiple variables in data.frame mode. There are defaults
#' ('blue', 'forestgreen', 'red', 'skyblue', 'darkorange') but values provided with
#' this parameter precede that sequence of colors.
#' @param xlab Label for the abscissa, to pass to plot (default: "TIME [UTC]")
#' @param ylab Label for the ordinate, to pass to plot (default: second
#' variable name)
#' @param lwd Line width to pass to plot (default: 1); can be a vector
#' @param lty Line type to pass to plot (default: 1). Can be a vector.
#' variables provided in data.frame mode.
#' @param logxy Set to 'y' for log axes. This is provided (vs log='y') because it
#' makes possible translation of axis labels to 10^i format.
#' @param position An optional 2-element numeric vector specifying the
#' panel number (1 at the bottom) and the number of panels. Default is NA,
#' in which case the plot definition is returned; if specified, the plot
#' is instead plotted in an appropriate viewport. If further changes are
#' desired before plotting, this parameter should be left NA and the
#' appropriate viewport will then have to be defined and the plot inserted
#' via, e.g., print(g, vp=VP). To use this, include library(grid) and call
#' grid.newpage() before defining the first panel.
#' @param lmargin Specification for the left margin. Default is NA, in
#' which case the left margin in theme_WAC(), currently 1.3, will be used.
#' This is provided to help adjust multiple-panel plots when the size of
#' the axis labels varies, so that the body of the plots can remain aligned.
#' @param ylim A two-element numeric vector specifying the limits to use
#' for the ordinate. The default is NA, in which case the limits will be
#' determined from the range of values in the plotted variables.
#' @param legend.position This parameter specifies the position for the 
#' legend. The default is c(0.5, 0.92), inside but at the top of the plot.
#' The legend can also be suppressed by setting this parameter to NA. Then 
#' a legend can still be added after the ggplotWAC call.
#' @param panels The integer number of panels (facets) to be displayed. Default is 1,
#' in which case no faceting will be used. To display a set of panels aligned
#' vertically, e.g., to show multiple time-series plots, use this in conjunction
#' with the next three parameters, and call with a data.frame that contains
#' Time and N variables ordered in sets so that the first N/panels 
#' variables will be plotted in the top panel, etc. For faceted plots, the
#' resulting plot definition is returned for possible further modification
#' prior to printing, and viewports are not used internally by ggplotWAC.
#' @param labelL For N variables (excluding the first, which is often Time),
#' a character vector of length N/panels specifying 
#' the labels for the individual lines in each panel. These labels will appear
#' in the legend. The default is NA, in which case the legend will be suppressed.
#' @param labelP A character vector of length "panels" specifying the names 
#' of each panel. These labels will appear at the right side of each panel. The
#' default is NA, in which case generic "panel1", "panel2", etc., names will
#' be used.
#' @param theme.version The theme version to pass to theme_WAC; default is 0.
#' @param ... Additional arguments to pass to plot(), but don't include col, xlab, ylab, lwd, type, xaxt or yaxt
#' @examples 
#' ggplotWAC(RAFdata[, c("Time", "ATX", "DPXC")])
#' \dontrun{ggplotWAC (data.frame ("Time"=Time, "TASX"=TASX), ylab="TAS")}

ggplotWAC <- function (.data, col="blue", xlab="TIME [UTC]", 
                       ylab="", lwd=1, lty=1, logxy='',
                       position=NA, lmargin=NA, ylim=NA,
                       legend.position=c(0.5, 0.92), 
                       panels=1,
                       labelL=NA, labelP=NA, theme.version=0, ...) {
  if (!is.data.frame (.data)) {
    print ("Error, first argument to ggplotWAC must be a data.frame")
  } else {
    if (!is.expression(ylab) && (ylab == "")) {
      ylab <- names(.data)[2]
    }
    ## protect against all-missing variables
    for (j in 2:min(6, ncol(.data))) {
      if (!any (!is.na (.data[, j]))) {
        .data[1, j] <- -32767.
        .data[2, j] <- 32767.
      }
    }
    if (!is.na(position[1])) {
      h <- 0.9/position[2]
      yp <- 0.05+(position[1]-0.5)*h
      if (position[1] == 1) {
        hh <- h+0.1
      } else {
        hh <- h
        yp <- yp + 0.05
      }
      vp <- viewport(width=1, height=hh, x=0.5, y=yp)
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
    nv <- names (.data); np <- length(nv) - 1
    if (panels > 1) {
      np <- np / panels
    }
    if (!is.na(ylim[1])) {yrange <- ylim}
    if (length(col) == 1 && np > 1) {
      colrs <- c(col, 'forestgreen', 'red', 'skyblue', 'darkorange')
      colrs <- colrs[-c((np+1):length(colrs))]
    } else {
      colrs <- col
    }
    clr <- nv[-1] 
    lwd <- c(lwd, rep(1,np)); lwd <- lwd[-c((np+1):length(lwd))]
    lty <- c(lty, rep(1,np)); lty <- lty[-c((np+1):length(lty))]
    if (panels == 1) {
      names(colrs) <- clr
      names(clr) <- clr
      names(lwd) <- clr
      names(lty) <- clr
    }
    ## now handle the faceting case:
    if (panels > 1) {
      DL <- nrow (.data); DC <- ncol(.data) - 1
      lines_per_panel <- DC / panels
      if (is.na(labelP[1])) {
        labelP <- 'panel1'
        for (k in 2:panels) {
          labelP <- c(labelP, sprintf('panel%d',k))
        }
      }
      VarGroup <- rep (gl (lines_per_panel, DL, labels=labelL), panels)
      PanelGroup <- gl (panels, lines_per_panel*DL, labels=labelP)
      dd <- data.frame(reshape2::melt(.data, 1), VarGroup, PanelGroup)
      colrs <- rep(colrs, panels)
      lwd <- rep(lwd, panels)
      lty <- rep(lty, panels)
      lvl <- levels(dd$VarGroup)
      g <- with(dd, ggplot (dd, aes(Time, value, colour=VarGroup, linetype=VarGroup)))
      g <- g + geom_line(aes(size=VarGroup))
      g <- g + scale_size_manual ('', labels=lvl, breaks=lvl, values = lwd)
      g <- g + scale_linetype_manual ('', labels=lvl, breaks=lvl, values = lty)
      g <- g + scale_colour_manual('', labels = lvl, breaks=lvl, values = colrs)
      g <- g + facet_grid (PanelGroup ~ ., scales='free_y', drop=TRUE)
    } else {
      g <- ggplot (data=.data, aes(x=eval (parse (text=names(.data)[1]))), na.rm=TRUE)
      g <- g + ylim (yrange)
    }
    if (names(.data)[1] == "Time") {
      g <- g + xlab ("Time [UTC]")
    } else {
      g <- g + xlab (names(.data)[1])
    } 
    if (!is.expression(ylab) && ylab == '') {
      ylab <- names (.data)[2]
    }
    g <- g + ylab(ylab)
    if (panels == 1) {
      for (j in 1:min(np, 5)) {
        a <- sprintf ("aes (y=%s, colour='%s', size='%s', linetype='%s')", 
                      clr[j], clr[j], clr[j], clr[j])
        g <- g + geom_line (eval (parse (text=a)))
      }
      g <- g + scale_size_manual ("", labels=clr, breaks=clr, values = lwd)
      g <- g + scale_linetype_manual ("", labels=clr, breaks=clr, values = lty)
      g <- g + scale_colour_manual("", labels = clr, breaks=clr, values = colrs)
      # g <- g + guides(colour = guide_legend(reverse=TRUE),
      #                 linetype=guide_legend(reverse=TRUE),
      #                 size=guide_legend(reverse=TRUE))
      # print (c(clr, colrs, lwd, lty))
      # print (names(colrs))
    }
    g <- g + theme_WAC(theme.version)
    if (panels > 1) {
      g <- g + theme(axis.text.x = element_text (size=11.5, margin=margin(15,0,0,0)))
      g <- g + theme(axis.title.x = element_text (size=12))
    }
    if (is.na(legend.position[1])) {
      g <- g + theme (legend.position='none')
    } else {
      g <- g + theme(legend.position=legend.position)
    }
    if (!is.na(position[1])) {
      if (position[1] != 1) {
        g <- g + theme(axis.title.x=element_blank(), axis.text.x=element_blank())
        g <- g + theme(axis.title.y=element_text(size=12), 
                       axis.text.y=element_text(size=12))
      } else {
        g <- g + theme(axis.title.y=element_text(size=12), 
                       axis.text.y=element_text(size=12),
                       axis.text.x=element_text(size=12),
                       axis.title.x=element_text(size=12))
      }
    }
    if (!is.na(lmargin)) {
      ## these should match values in theme_WAC()
      g <- g + theme (plot.margin=unit(c(0.3,0.3,1.1,lmargin),"lines"))
    }
    ## preserve .data in the parent environment for plotting
    .data <<- .data
    if (!is.na(position[1])) {
      print (g, vp=vp)
    } else {
      return (g)
    }
    
    ## suppressMessages(ggsave (.plotfile, g))
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
}


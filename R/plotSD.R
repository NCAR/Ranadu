## plot size distribution from CDP, FSSP, 2DC (1DC), UHSAS, etc.
#' @title plotSD
#' @description Plot a size distribution from CDP, FSSP, 2DC (1DC), UHSAS, etc.  
#' @details The size distribution can be plotted with linear or log abscissa and linear or log ordinate. If the ordinate is log,
#' the plotted variable is the size distribution weighted by the particle size, as appropriate for a logarithmic density function.
#' Several size distributions can be combined on one plot. The size distribution will be averaged over all rows in the data.frame, 
#' so the time range should be limited before calling. Because subsetting removes attributes, save the 'CellSizes' attribute
#' before subsetting. Here is example code:
#' D <- getNetCDF('/Data/CSET/CSETrf06.nc', c('CCDP_', 'C1DC_'), 173000, 173500)
#' T <- plotSD(D, CellLimits=NA, logAxis='xy', xlim=c(1,1000))
#' title(bquote('CDP concentration' == .(format(T[1], digits=3)) ~ cm^"-3" ~ ' mean diameter' == .(format(T[2], digits=3)) ~ mu*"m"))
#' T <- plotSD(D[, c("Time", "C1DC_LWOO")], CellLimits=NA, ucon=1.e-3, col='forestgreen', ADD=TRUE)
#' legend('topright', legend=c('CDP', '2DC'), lwd=c(2,2), col=c('blue', 'forestgreen'))
#' @aliases plotSD
#' @author William Cooper
#' @export plotSD
#' @importFrom stats sd 
#' @param data A data.frame containing "Time" and one size-distribution variable to be plotted, as the second column after "Time". 
#' Example: CCDP_LWOI. To add other variables, call this function again with the other size-distribution variables and .ADD=TRUE.
#' It may be necessary to supply xlim spanning all sizes with the first call because this is not reset. An example of a suitable
#' data.frame is generated from "D <- getNetCDF('/Data/CSET/CSETrf06.nc', c('CCDP_', 'C1DC_'), 173000, 173500)". Then make individual
#' calls for each size distribution: "plotSD(D[ , c('Time', 'CCDP_LWOI')], CellLimits=NA)" and "plotSD(D[ , c('Time', 'C1DC_LWOO')], 
#' CellLimits=NA, ucon=1.e-4, .Add=TRUE)". 
#' @param CellLimits A vector containing the bin limits for the data. This is usually obtained from "attr(Data$CCDP_LWOI, 'CellSizes').
#' It represents the lower limits of the bins, so it should have one more entry than the number of columns in data to represent
#' the upper limit of the last bin. The deault is a vector of bin limits applicable to the CDP. If the supplied value is NA, the vector 
#' will be loaded from the attribute. In this case, if the attribute is unavailable, the function will fail. Don't use the standard
#' plot parameter 'log' because, while logarithmic axes will be generated, the weighted distribution won't be constructed.
#' @param logAxis A string indicating which axes should be logarithmic; e.g., logAxis='xy' for both logarithmic.
#' @param ucon A numeric value specifying the unit conversion factor (e.g., 1.e-3 for /L to /cc). The default is 'NA', in which
#' case the conversion factor will be 1 for CDP or FSSP or UHSAS variables and 1.e-3 for 2DC or 1DC variables (normally specified in 
#' units of /L).
#' @param col Color for the plotted size distribution. Default: 'blue'.
#' @param FirstCell The first cell to include. Default is 1; use 3 or higher for the 2D probe. For multiple probes, this
#' can be a vector with an entry for each probe in order.
#' @param title A title for the plot. The default is the data and time range used for the data. You can suppress
#' this with "title = ''" and then add your own title, perhaps including the concentration, mean diameter, and
#' standard deviation in diameter as returned from the function.
#' @param lwd Line width(s); default is 2.
#' @param xlim Limits for the abscissa of the plot. Default is to take this from the range of data.
#' @param ylim Limits for the ordinate of the plot. Default is to take this from the range of data.
#' @param CDF Include a cumulative distribution function? (Default is FALSE.)
#' @param LWC Construct the plot to show the distribution of liquid water content in logarithmic intervals.
#' @param addBar Plot an indication of the mean diameter and standard deviation for all the probes included in
#' the plot. Default is FALSE.
#' @param ADD A logical flag that, if true, does not generate the plot frame and axes
#' but instead just adds a new line to an existing plot. This will not reset the
#' scales, so it may be necessary to set them with the first call. log-x and log-y will be reloaded from the
#' preceding call. A legend can be added after the last call with a statement like
#' "legend('topright', legend=c('CDP', '2DC'), lwd=c(2,2), col=c('blue', 'forestgreen'))
#' @param legend.position If set, place legend as specified (e.g., 'topright'). Defaults use 'topright' for normal size
#' distributions and 'topleft' for LWC plots.
#' @param ... Additional arguments passed to the plot routine to control 
#' graphics parameters etc. 
#' @return T -- The result is the plot, but the mean concentration, mean diameter and standard deviation in the
#' diameter are returned in a three-component vector.
#' This is for possible plot annotation in, e.g., a plot title. An example of a plot title is:
#' "title(bquote('concentration' == .(format(T[1], digits=3)) ~ cm^"-3" ~ ' mean diameter' == .(format(T[2], digits=3)) ~ mu*"m"))"


plotSD <- function (data, CellLimits=NA, logAxis='', ucon=rep(NA, 5), col='blue', FirstCell=rep(1,5), 
  title=NA, lwd=rep(2,5), xlim=NA, ylim=NA, CDF=FALSE, LWC=FALSE, addBar=FALSE, ADD=FALSE, legend.position=NA, ...) {
  VtoPlot <- names(data)[-1]
  ## define functions used to construct size distribution: --------------------------------
  logaxis <- function(side, ...) {
    aty <- axTicks(side)
    Laty <- log10(aty)
    LRange <- Laty[length(Laty)] - Laty[1]
    atyA <- 10^(Laty[1]:Laty[length(Laty)])
    axis(side=side, at=atyA, labels=NA, tck=0.03)
    at.minor <- 10^as.vector(log10(outer(1:9, 10^(min(Laty):max(Laty)))))
    axis(side=side, at=at.minor, labels=NA, tck=0.02)
    at.minor <- log10(outer(1:9, 10^(min(Laty):max(Laty))))
    lab <- sapply(aty, function(i) as.expression(bquote(10^ .(i))))
    return(lab)
  }
  uconSet <- function(ucon, VtoPlot) {  ## find the unit-conversion factor
    if (is.na(ucon)) {
      ucon <- 1
      if (grepl('1DC', VtoPlot) || grepl('2DC', VtoPlot)) {
        ucon <- 1.e-3
      }
    }
    return(ucon)
  }
  getP <- function(data, VtoPlot, CellLimits=NA, logAxis='', ucon=NA, FirstCell=1, LWC=FALSE) {
    if (is.na(CellLimits[1])) {
      if (!is.null (attr(data[ , VtoPlot], 'CellSizes'))) {
        CellLimits <- attr(data[ , VtoPlot], 'CellSizes')
      } else {
        CellLimits <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50)
      }
    }
    CellSizes <- CellLimits [1 : (length(CellLimits) - 1)] + diff (CellLimits) / 2
    # print(c('CellLimits', CellLimits))
    # print(c('CellSizes', CellSizes))
    ucn <- uconSet(ucon, VtoPlot)
    # print (sprintf('ucon=%e', ucn))
    P <- data[, VtoPlot] * ucn  
    # print (c('P1', P[150,]))
    P <- P[!is.na(P[, 10]), ]  ## if one column is NA, all are NA
    aveN <- sum(colSums(P, na.rm=TRUE), na.rm=TRUE) / nrow(P)
    dbar <- sum(P %*% CellSizes, na.rm=TRUE) / sum(P, na.rm=TRUE)
    dbar2 <- sum(P %*% CellSizes^2, na.rm=TRUE) / sum(P, na.rm=TRUE)
    LWfactor <- pi * 1.e-6 / 6
    aveLWC <- sum(P %*% CellSizes^3, na.rm=TRUE) * LWfactor / nrow(P)
    # print (sprintf('total LWC=%.3f', aveLWC))
    sd <- sqrt(dbar2 - dbar^2)
    P <- colSums(P, na.rm=TRUE) / nrow(P)  ## get the average value over the selected time period
    # print (c('P2', P))
    P <- P / diff(CellLimits)              ## translate to conc. per um
    # print (c('P3', P))
    if (grepl('x', logAxis) || grepl('X', logAxis)) {
      P <- P * CellSizes * log(10)        ## this gives the distribution per interval in log10(d)
    }
    # print (c('P4', P))
    ylow <- ifelse (grepl('y', logAxis), 1.e-6, 0)
    if (LWC) {
      ylow <- 1.e-8
    }
    P[P < ylow] <- ylow    ## avoid underflow errors
    
    if (!is.na(FirstCell) && FirstCell > 1) {
      CellLimits <- CellLimits[-1:-(FirstCell-1)]
      CellSizes <- CellSizes[-1:-(FirstCell-1)]
      P <- P[-1:-(FirstCell-1)]
    }
    if (LWC) {
      P[P > ylow] <- P[P > ylow] * CellSizes[P > ylow]^3 * LWfactor
      P[P < ylow] <- ylow    ## avoid underflow errors
    }
    return(list(CellLimits, P, aveN, dbar, sd, aveLWC))
  }
  
  ## end of functions; begin plot calculation ------------------------------------------------------
  ## Save and restore these to avoid unintended consequences
  pmar <- par()
  ## need room at right side for CDF axis. Allow room in any case for consistent-size plots.
  op <- par (mar=c(5,4,3,4)+0.1, oma=c(0,0,0,0))
  if (ADD) {  ## set log argument to match what was used previously. Need to know this to multiply by diameter.
    if (par ('xlog')) {logAxis <- 'x'}
    if (par ('ylog')) {logAxis <- paste0(logAxis, 'y')}
  }
  
  if (LWC) {
    if (grepl('x', logAxis)) {
      yl <- expression(paste("d",chi,"/dlogD [g m"^"-3","]"))
    } else {
      yl <- expression(paste("d",chi,"/dD [g m"^"-3",mu,"m"^"-1","]"))
    }
  } else {
    if (grepl('x', logAxis)) {
      yl <- expression(paste("dN/dlog"["10"], "D [cm"^"-3","]"))  ## for log axis
    } else {
      yl <- expression(paste("dN/dD [cm"^"-3",mu,"m"^"-1","]"))   ## axis title for linear plot
    }
  }
  
  V <- getP (data, VtoPlot[1], CellLimits=CellLimits, logAxis=logAxis, ucon=ucon[1], FirstCell=FirstCell[1], LWC=LWC)
  CLimits <- V[[1]]
  P <-    V[[2]]
  aveN <- V[[3]]
  dbar <- V[[4]]
  sd <-   V[[5]]
  aveLWC <- V[[6]]
  if (ADD) {
    lines (CLimits[-1], P, type='S', lwd=2, col=col, ...)
    lines (CLimits[1:2], rep(P[1], 2), lwd=2, col=col, ...)  ## draw first line w/o step
  } else {  ## this is a new plot definition
    xlab <- expression(paste("D [",mu,"m]"))
    ylab <- ''  ## replace later with mtext call, for spacing
    ## Before plotting, see if there are any other variables to be plotted.
    ## If so, adjust plot limits unless forced in function call.
    ## Is xlim supplied? If not, find xlim from cell limits:
    if (is.na(xlim[1])) {
      xlim <- c(CLimits[1], CLimits[length(CLimits)])
      ## are there other size distributions? If so, set appropriate limits
      if (ncol(data) > 2) {
        for (j in 3:ncol(data)) {
          V2 <- getP (data, VtoPlot[j-1], CellLimits=NA, logAxis=logAxis, ucon=ucon[j-1], FirstCell=FirstCell[j-1], LWC=LWC)
          CL <- V2[[1]]
          xlimt <- c(CL[1], CL[length(CL)])
          if (xlimt[1] < xlim[1]) {xlim[1] <- xlimt[1]}
          if (xlimt[2] > xlim[2]) {xlim[2] <- xlimt[2]}
        }
      }
      if (grepl('x', logAxis)) {
        xlim <- c(10^floor(log10(xlim[1])), 10^ceiling(log10(xlim[2])))
      }
    } 
    if (is.na(ylim[1])) {
      ylim <- c(min(P, na.rm=TRUE), max(P, na.rm=TRUE))      
      if (ncol(data) > 2) {
        for (j in 3:ncol(data)) {
          V2 <- getP (data, VtoPlot[j-1], CellLimits=NA, logAxis=logAxis, ucon=ucon[j-1], FirstCell=FirstCell[j-1], LWC=LWC)
          if (min(V2[[2]], na.rm=TRUE) < ylim[1]) {ylim[1] <- min(V2[[2]], na.rm=TRUE)}
          if (max(V2[[2]], na.rm=TRUE) > ylim[2]) {ylim[2] <- max(V2[[2]], na.rm=TRUE)}
        }
      }
    }
    if (grepl('y', logAxis)) {
      ylim <- c(10^floor(log10(ylim[1])), 10^ceiling(log10(ylim[2])))
      ylow <- ylim[1]
    } else {
      ylow <- 0
    }
    # print (c('P6', P))
    plot (CLimits[-1], P, type='S', xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, 
      col=col, lwd=lwd, log=logAxis, xlim=xlim, ylim=ylim, ...)
    lines (CLimits[1:2], rep(P[1], 2), lwd=lwd, col=col, ...)  
    
    # as.POSIXlt(data$Time[nrow(data)], tz='UTC')))
    ## plot 10^-format labels for log scale
    if (grepl ('y', logAxis)) {
      aty <- axTicks(2)
      laby <- sapply(log10(aty), function(i) as.expression(bquote(10^ .(round(i, digits=2)))))
    }
    colrs <- c(col[1], 'darkgreen', 'red', 'skyblue', 'darkorange', 'gray40')
    xa <- CLimits
    CellSizes <- CLimits [1 : (length(CLimits) - 1)] + diff (CLimits) / 2
    if (grepl ('x', logAxis)) {
      ya <- P * diff(CLimits) / (log(10) * CellSizes)
    } else {
      ya <- P * diff(CLimits)
    }
    # if (LWC) {ya <- ya * CellSizes^3}
    ya <- c(0, ya)
    if (ncol(data) > 2) {
      ## get a single vector spanning all probes for the CDF. Concert back to #/bin if necessary.
      for (j in 3:ncol(data)) {
        V2 <- getP (data, VtoPlot[j-1], CellLimits=NA, logAxis=logAxis, ucon=ucon[j-1], FirstCell=FirstCell[j-1], LWC=LWC)
        dbar <- (dbar * aveN + V2[[3]] * V2[[4]]) / (aveN + V2[[3]])
        aveN <- aveN + V2[[3]]
        aveLWC <- aveLWC + V2[[6]]
        xa <- c(xa, V2[[1]])
        CS <- V2[[1]][1 : (length(V2[[1]]) - 1)] + diff (V2[[1]]) / 2
        if (grepl ('x', logAxis)) {
          yb <- diff(V2[[1]]) * V2[[2]] / (log(10) * CS)
        } else {
          yb <- V2[[2]] * diff(V2[[1]])
        }
        # if (LWC) {yb <- yb * CS^3}
        ya <- c(ya, c(0, yb))
        if (is.na(col[j-1])) {col2 <- colrs[j-1]}
        lines (V2[[1]][-1], V2[[2]], type='S', lwd=lwd[j-1], col=col2)
        lines (V2[[1]][1:2], rep(V2[[2]][1], 2), lwd=lwd[j-1], col=col2)
      }
      ## order these
      o <- order(xa)
      ya <- ya[o]
      xa <- xa[o]
    } 
    # print (sprintf ('conc. %.2f dbar %.2f sd %.2f LWC %.3f', aveN, dbar, sd, aveLWC))
    cdf <- cumsum(ya)                      ## calculate the cumulative distribution function
    cdf <- cdf/cdf[length(cdf)]            ## normalize it to 1 at max
    cdf <- 1-cdf                           ## show exceedance d.f. because log scale makes this easier to interpret
    # if (LWC) {
    #   ## label the axis according to diameter
    #   atL <- c(1, 10, 100)
    #   axis (1, at=atL^3*pi/6, labels=c('1', '10', '100'), tck=0.03)
    #   axis (3, at=atL^3*pi/6, labels=NA, tck=0.03)
    #   LatL <- log10(atL)
    #   # axis(side=1, at=atyA, labels=NA, tck=0.03)
    #   at.minor <- 10^as.vector(log10(outer(1:9, 10^(min(LatL):max(LatL)))))
    #   axis(side=1, at=at.minor^3*pi/6, labels=NA, tck=0.02)
    #   axis(side=3, at=at.minor^3*pi/6, labels=NA, tck=0.02)
    #   # at.minor <- log10(outer(1:9, 10^(min(Laty):max(Laty))))
    # } else {
    axis(1,tck=0.02)
    axis(3,labels=NA,tck=0.02)
    if (grepl('x', logAxis)) {
      logaxis(1)
      logaxis(3) 
    }
    # }
    if (grepl('y', logAxis)) {
      # log10.axis(2, at=aty)
      aty <- axTicks(2)
      ## omit values not power of 10
      ix <- log10(aty) %% 1 < 0.001
      aty <- aty[ix]
      laby <- laby[ix]
      axis(2,at=aty,labels=laby, las=2, tck=0.03)
      logaxis(2)
      logaxis(4)
    } else {
      axis(2, tck=0.02)
      axis(4, labels=NA, tck=0.02)
    }
    
    mtext(yl, side=2, line=2.5)
    # title(bquote('concentration' == .(format(aveN, digits=3)) ~ cm^"-3" ~ " mean diameter" == .(format(dbar, digits=3)) ~ mu*"m"))
    
    ## add a cumulative distribution function: fraction larger than the plotted diameter
    ml <- par('usr')
    if (grepl('y', logAxis)) {
      clow <- ceiling(ml[3])
      chigh <- floor(ml[4])
    } else {
      clow <- ml[3]
      chigh <- ml[4]
    }
    cl <- chigh - clow + 1
    if (grepl ('y', logAxis)) {
      clow <- 10^clow
      chigh <- 10^chigh
    }
    if (CDF) {
      aty <- axTicks(4)
      cdf <- aty[length(aty)] * cdf
      cdf[cdf < ylow] <- ylow
      if (FirstCell[1] <= 1) {
        lines(xa, cdf, col='darkorange', lwd=1.6, lty=2)
      } else {
        lines(xa, c(cdf[-1:-(FirstCell[1]-1)]), col='darkorange', lwd=1.6, lty=2)
      }
      if (grepl ('y', logAxis)) {
        atc <- aty / aty[length(aty)]  ## normalize to 1 for CDF
        ## and adjust if this is not the maximum
        # atc <- atc * ch / atc[1]
        labc <- sapply(log10(atc), function(i) as.expression(bquote(10^ .(round(i, digits=2)))))
        ## omit values not power of 10
        ix <- log10(atc) %% 1 < 0.001
        axis (4, at=aty[ix], labels=labc[ix], tck=-0.02, las=2)
      } else {
        atc <- aty / aty[length(aty)]
        if (abs (round (atc[2], 1) - atc[2]) > .01) {
          labc <- sprintf('%.2f', atc)
        } else {
          labc <- sprintf('%.1f', atc)
        }
        axis(4, at=aty, labels=labc, col.ticks='brown', tck=0.02)
      }
      mtext('exceedance fraction', side=4, line=2.5)
    }
    ## add a line and dot representing +/- std dev and mean size
    if (addBar) {
      points(dbar, 0.5*chigh, pch=19, col='darkgreen')
      lines (c(dbar-sd, dbar+sd), rep(0.5*chigh, 2), lwd=1.6, col='darkgreen')
    }
    if (is.na(legend.position[1])) {
      lloc <- ifelse (LWC, 'topleft', 'topright')
    } else {
      lloc <- legend.position
    }
    if (CDF) {
      legend(lloc, inset=0.04, legend=c(sub('_.*', '', VtoPlot), 'exceedance'), col=c(colrs[1:length(VtoPlot)], 'darkorange'), 
        lwd=c(rep(2, length(VtoPlot)), 1.6), lty=c(rep(1, length(VtoPlot)), 2))    
    } else {
      legend(lloc, inset=0.04, legend=c(sub('_.*', '', VtoPlot)), col=c(colrs[1:length(VtoPlot)]), 
        lwd=c(rep(2, length(VtoPlot))), lty=c(rep(1, length(VtoPlot))))
    }
    if (is.na(title[1])) {
      if (LWC) {
        title (sprintf ('%s -- %s  LWC=%.2f', as.POSIXlt(data$Time[1], tz='UTC'), strftime(data$Time[nrow(data)],
          format="%H:%M:%S", tz='UTC'), aveLWC))        
      } else {
        title (sprintf ('%s -- %s', as.POSIXlt(data$Time[1], tz='UTC'), strftime(data$Time[nrow(data)],
          format="%H:%M:%S", tz='UTC')))
      }
    } else {
      title (title)
    }
    op <- par (mar=pmar$mar, oma=pmar$oma)  ## restore to margin settings when entering function
    return(c(aveN, dbar, sd, aveLWC))
  }
}
  
  # library(Ranadu)
  # D <- getNetCDF('/Data/CSET/CSETrf06.nc', c('CCDP_', 'C1DC_', 'CUHSAS_'), 173000, 173500)
  # T <- plotSD(D, CellLimits=NA, logAxis='xy', LWC=TRUE, CDF=TRUE)
  # title(bquote('CDP concentration' == .(format(T[1], digits=3)) ~ cm^"-3" ~ ' mean diameter' == .(format(T[2], digits=3)) ~ mu*"m"))
  # T <- plotSD(D[, c("Time", "C1DC_LWOO")], CellLimits=NA, ucon=1.e-3, col='forestgreen', ADD=TRUE)
  # legend('topright', legend=c('CDP', '2DC'), lwd=c(2,2), col=c('blue', 'forestgreen'))
  

## plot histogram with cumulative distribution function or exceedance d.f.
#' @title histWAC
#' @description Plot a normal histogram but add a cumulative distribution function. 
#' @details The usual histogram with conventional arguments is used, but
#' the result then has an added cumulative distribution function with
#' scale on the right side. The 'density' plot is used and the cumulative
#' distribution function has limits from 0 to 1. In addition, an optional
#' argument "weughts" applies weighting to eqch of the items entering
#' the histogram
#' @aliases histWAC
#' @author William Cooper
#' @export histWAC
#' @param x The usual first argument to hist().
#' @param weights Numerical, length matching "x" or recyclable to match, 
#' to calculates a weighted histogram. Default NA.
#' @param Exceedance Logical (default FALSE). If TRUE, the exceedance is
#' plotted instead of the conventional cumulative distribution. If NA
#' the cumulative distribution is not plotted.
#' @param ADD Logical (default FALSE) specifying if the distribution should
#' be added to an existing one.
#' @param breaks Number of breaks for histogram. Default = 100.
#' @param freq Logical controlling plotting frequency or density. Default FALSE.
#' @param logAxis Character vector specifying if the axes should be log. 
#' Default: FALSE. The construction of tick marks and labels for the
#' abscissa is designed for special applications where the range is small,
#' so these may be inappropriate for some applications. (bug to be fixed)
#' @param plot Logical that, if FALSE, suppresses the plot. Default = TRUE.
#' @param ... Additional arguments passed to the plot() routine to control 
#' graphics parameters etc. 
#' @return T -- The result is the plot, but the usual value returned by
#' hist() is also returned invisibly. Assign it to a variable to use it.
#' @example Ranadu::histWAC(rnorm(2000,20,2))


histWAC <- function (x, weights = NA, Exceedance = FALSE, ADD = FALSE, 
                     breaks = 100, freq = FALSE, logAxis = '', plot=TRUE, ...) {
    ## define functions used to construct a logarithmic distribution: --------------------------------
    logaxis <- function(side, ...) {
        aty <- axTicks(side)
        Laty <- log10(aty)
        LRange <- Laty[length(Laty)] - Laty[1]
        atyA <- 10^Laty
        axis(side=side, at=atyA, labels=NA, tck=0.03)
        at.minor <- 10^as.vector(log10(outer(1:9, 10^(min(Laty):max(Laty)))))
        axis(side=side, at=at.minor, labels=NA, tck=0.02)
        at.minor <- log10(outer(1:9, 10^(min(Laty):max(Laty))))
        lab <- sapply(aty, function(i) as.expression(bquote(10^ .(i))))
        return(lab)
    }
    
    ## end of functions; begin plot calculation ------------------------------------------------------
    ## Save and restore these to avoid unintended consequences
    pmar <- par()
    ## need room at right side for CDF axis. Allow room in any case for consistent-size plots.
    op <- par (mar=c(5,4,3,4)+0.1, oma=c(0,0,0,0))
    if (ADD) {  ## set log argument to match what was used previously. Need to know this to multiply by diameter.
        if (par ('xlog')) {logAxis <- 'x'}
        if (par ('ylog')) {logAxis <- paste0(logAxis, 'y')}
        h <- hist(x, plot = FALSE, breaks=breaks, ...)
        ## Addition for weighted histogram:
        if (!is.na(weights[1])) {
            hw <- h
            hw$counts <- rep(0, length(hw$counts))
            bs <- binStats(data.frame(X=weights, Y=x), bins=length(h$breaks)-1,
                           xlow=h$breaks[1], xhigh=h$breaks[length(h$breaks)],
                           breaks=breaks, addBin = TRUE)
            ## Add the weights:
            for (ib in 1:nrow(bs)) {
                hw$counts[bs$BIN[ib]] <- hw$counts[bs$BIN[ib]] + bs$X[ib]
            }
            hw$density <- hw$counts / sum(hw$counts)
            h <- hw
        }
        if (plot) {
            if(freq) {
                lines (h$breaks[-1], h$counts, type='S', lwd=2, ...) 
                lines (h$breaks[1:2], c(0, h$counts[1]), lwd=2, ...)  
            } else {
                lines (h$breaks[-1], h$density, type='S', lwd=2, ...)
                lines (h$breaks[1:2], c(0, h$density[1]), lwd=2, ...)  ## draw first line w/o step
            }
        }
    } else {  ## this is a new plot definition
        if ("ylab" %in% names(list(...))) {
        } else {
            ylab <- ifelse(freq == FALSE, 'density', 'counts')  
        }
        
        # print (c('P6', P))
        # if (grepl('x', logAxis)) {x <- log10(x)}
        h <- hist(x, plot = FALSE, breaks = breaks)
        ## Addition for weighted histogram:
        if (!is.na(weights[1])) {
            hw <- h
            hw$counts <- rep(0, length(hw$counts))
            bs <- binStats(data.frame(X=weights, Y=x), bins=length(h$breaks)-1,
                           xlow=h$breaks[1], xhigh=h$breaks[length(h$breaks)],
                           breaks=breaks, addBin = TRUE)
            ## Add the weights:
            for (ib in 1:nrow(bs)) {
                hw$counts[bs$BIN[ib]] <- hw$counts[bs$BIN[ib]] + bs$X[ib]
            }
            hw$density <- hw$counts / (diff(hw$breaks) * sum(hw$counts))
            h <- hw
        }
        if (grepl('y', logAxis)) {
            ## for log-y plot, protect against density == 0 or counts == 0:
            if (freq) {
                lmin <- min(h$counts[h$counts > 0], na.rm=TRUE) / 10
                h$counts[h$counts <= 0] <- lmin
            } else {
                lmin <- min(h$density[h$density > 0], na.rm=TRUE) / 10
                h$density[h$density <= 0] <- lmin
            }
        }
        if (plot) {
            xrg <- range(h$breaks)
            if (freq) {
                if ("ylab" %in% names(list(...))) {
                    plot(h$breaks[-1], h$counts, type='S', xaxt='n', yaxt='n', 
                         log=logAxis, xlim=xrg, ...)      
                } else {
                    plot(h$breaks[-1], h$counts, type='S', xaxt='n', yaxt='n', 
                         ylab=ylab, log=logAxis, xlim=xrg, ...)     
                }
                lines (h$breaks[1:2], c(0,h$counts[1]), type='S')
            } else {
                if ("ylab" %in% names(list(...))) {
                    plot(h$breaks[-1], h$density, type='S', xaxt='n', yaxt='n', 
                         log=logAxis, xlim=xrg, ...)
                } else {
                    plot(h$breaks[-1], h$density, type='S', xaxt='n', yaxt='n', 
                         ylab=ylab, log=logAxis, xlim=xrg,  ...)
                }
                lines (h$breaks[1:2], c(0,h$density[1]), type='S')
            }
        }  # end of "if plot"
    }
    if (plot) {
        ## plot 10^-format labels for log scale
        if (grepl ('y', logAxis)) {
            aty <- axTicks(2)
            laby <- sapply(log10(aty), function(i) as.expression(bquote(10^ .(round(i, digits=2)))))
        }
        if (grepl ('x', logAxis)) {
            # atx <- axTicks(1)
            # labx <- sapply(log10(atx), function(i) as.expression(bquote(10^ .(round(i, digits=2)))))
        }
        
        colrs <- c('blue', 'darkgreen', 'red', 'skyblue', 'darkorange', 'gray40')
        xa <- h$mids
        dx <- mean(diff(xa), na.rm=TRUE)
        if (grepl('x', logAxis)) {
            # xa <- 10^xa 
        }
        if (freq) {
            ya <- h$counts
        } else {
            ya <- h$density
        }
        ya <- c(0, ya)
        cdf <- cumsum(ya)                      ## calculate the cumulative distribution function
        cdf <- cdf/cdf[length(cdf)]            ## normalize it to 1 at max
        if (!plot) {Exceedance <- NA}
        ## show exceedance d.f. because log scale makes this easier to interpret
        if (!is.na(Exceedance) && Exceedance) {cdf <- 1-cdf}         
        if (grepl('x', logAxis)) {
            atx <- axTicks(1)
            axis(1,at=atx,labels=atx, las=1, tck=0.03)
            ## omit values not power of 10
            ix <- log10(atx) %% 1 < 0.001
            # atx <- atx[ix]
            # labx <- 10^labx[ix]
            # axis(1,at=atx,labels=labx, las=2, tck=0.03)
            logaxis(1)
            logaxis(3) 
        } else {
            axis(1,tck=0.02)
            axis(3,labels=NA,tck=0.02)
        }
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
        
        # mtext(yl, side=2, line=2.5)
        # title(bquote('concentration' == .(format(aveN, digits=3)) ~ cm^"-3" ~ " mean diameter" == .(format(dbar, digits=3)) ~ mu*"m"))
        
        ## add a cumulative distribution function: fraction smaller than the plotted diameter
        if (!is.na(Exceedance)) {
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
            if (TRUE) {
                aty <- axTicks(4)
                cdf <- aty[length(aty)] * cdf
                # dx <- mean(diff(xa), na.rm=TRUE)
                # xa <- xa - dx/2
                
                # xa <- c(xa, xa[length(xa)]+dx)
                xa <- c(xa[1]*0.9, xa)
                # print (xa)
                # print(cdf)
                lines(xa, cdf, col='darkorange', lwd=1.6, lty=2)
                
                
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
                if (!is.na(Exceedance) && Exceedance) {
                    mtext('exceedance fraction', side=4, line=2.5)
                } else {
                    mtext('cumulative distribution', side=4, line=2.5)
                }
            }
        }
    }
    ## add a line and dot representing +/- std dev and mean size
    
    # if (is.na(legend.position[1])) {
    #   lloc <- ifelse (LWC, 'topleft', 'topright')
    # } else {
    #   lloc <- legend.position
    # }
    # if (CDF) {
    #   legend(lloc, inset=0.04, legend=c(sub('_.*', '', VtoPlot), 'exceedance'), col=c(colrs[1:length(VtoPlot)], 'darkorange'), 
    #     lwd=c(rep(2, length(VtoPlot)), 1.6), lty=c(rep(1, length(VtoPlot)), 2))    
    # } else {
    #   legend(lloc, inset=0.04, legend=c(sub('_.*', '', VtoPlot)), col=c(colrs[1:length(VtoPlot)]), 
    #     lwd=c(rep(2, length(VtoPlot))), lty=c(rep(1, length(VtoPlot))))
    # }
    # if (is.na(title[1])) {
    #   if (LWC) {
    #     title (sprintf ('%s -- %s  LWC=%.2f', as.POSIXlt(data$Time[1], tz='UTC'), strftime(data$Time[nrow(data)],
    #       format="%H:%M:%S", tz='UTC'), aveLWC))        
    #   } else {
    #     title (sprintf ('%s -- %s', as.POSIXlt(data$Time[1], tz='UTC'), strftime(data$Time[nrow(data)],
    #       format="%H:%M:%S", tz='UTC')))
    #   }
    # } else {
    #   title (title)
    # }
    op <- par (mar=pmar$mar, oma=pmar$oma)  ## restore to margin settings when entering function
    return(invisible(h))
}


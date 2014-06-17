#' @title plotTrack
#' @description Plot a flight track in lat-lon coordinates. 
#' @details The flight track is shown with appropriate 
#' geographic boundaries, and the lat-lon scales are adjusted
#' to give rectilinear coordinates at the center of the plot.
#' The center coordinates and size of the plot can be specified,
#' or if omitted will be set to cover the range of data in the
#' supplied coordinates. The range of indices to plot can 
#' be supplied in the argument Range, or in the range of indices
#' supplied with the arguments (lon, lat, Time). By default,
#' labels are placed along the track every 15 min; this can be
#' changed by the .Spacing argument.
#' @aliases plotTrack
#' @author William Cooper
#' @export plotTrack
#' @param lon A numeric vector of longitude coordinates (degrees)
#' @param lat A numeric vector of latitude coordinates (degrees) 
#' @param Time A POSIX-format vector of times corresponding to lat/lon
#' @param r A sequence of indices to use when plotting lat, lon, Time (default is 0 which causes the entire range of indices to be plotted.) 
#' @param xc An optional central longitude for the plot. If omitted, the range in lon[r] is used to determine the midpoint.
#' @param yc An optional central latitude for the plot. If omitted, the range in lat[r] is used to determine the midpoint.
#' @param sz An optional size in units of degrees for the plot. If omitted, the size is determined from the range of measurements.
#' @param Spacing The spacing between time labels placed on the graph, in minutes. The default is 15 min.
#' @param WindFlags A scale factor for wind flags placed along the track, in units of percentage of the plot size. The default is 0 which suppresses the flags. A common value is 5.
#' @param ... Additional arguments passed to the plot routine to control graphics parameters etc. 
#' @return NULL -- The result is the plot.
#' @examples 
#' \dontrun{plotTrack (LONC, LATC, Time, setRange (Time, 25000, 43000))}
plotTrack <- function (lon, lat, Time, r=0, xc=NULL,yc=NULL, 
                       sz=NULL, Spacing=15, WindFlags=0, ...) {
  if (length(r) < 2) {
    r <- 1:length(Time)
  }
  if (!is.null(xc)) {
    if (!is.null(sz)) {
      xl <- c(xc-sz/2., xc+sz/2.)
    } else {
      xl <- NULL
    }
  } else {
    xlow <- min (lon[r], na.rm=TRUE)
    xhigh = max (lon[r], na.rm=TRUE)
    xl <- c(xlow, xhigh)
  }
  if (!is.null(yc)) {
    if (!is.null(sz)) {
      yl <- c(yc-sz/2., yc+sz/2.)
    } else {
      yl <- NULL
    }
  } else {
    ylow <- min (lat[r], na.rm=TRUE)
    yhigh <- max (lat[r], na.rm=TRUE)
    yl <- c(ylow, yhigh)
  }
  sz <- max (xl[2] - xl[1], yl[2] - yl[1])
  
  ap <- 1. / cos (median (lat[r], na.rm=TRUE)*pi/180.)
  plot (lon[r], lat[r], type='n', xlim=xl, ylim=yl, asp=ap,
        xlab=expression(paste("Longitude [",degree,
                              "]")), ylab=expression(paste("Latitude [",degree,"]")))
  if ((min(lon[r], na.rm=TRUE) < -130) 
      | (max(lon[r], na.rm=TRUE) > -70.)
      | (min(lat[r], na.rm=TRUE) < 30.) 
      | (max(lat[r], na.rm=TRUE) > 50.)) {
        map("world", add=TRUE, fill=FALSE, col="black", lty=2)
      } else {
        map("state", add=TRUE, fill=FALSE, col="black", lty=2)
      }
  points (lon[r], lat[r], type='l', col='blue', 
          xlab='Latitude [deg.]', ylab='Longitude [deg.]', ...)
  ltm <- as.POSIXlt(Time)
  inx <- 1:length(ltm)
  ltime <- Time[(ltm$min%%Spacing == 0) & (ltm$sec == 0)]
  
  for (l in 1:length(ltime)) {
    ix <- inx[Time == ltime[l]]
    if ((ix >= r[1]) & (ix <= r[length(r)])) {
      lbl <- sprintf ("%02d%02d", ltm$hour[ix], ltm$min[ix])
      text (lon[ix], lat[ix], lbl, col='red', pos=4)
      points (lon[ix], lat[ix], pch=16, col='RED')
    }
  }
  # plot 50 wind flags along track:
  if (WindFlags > 0.) {
    skip <- length(Time[r]) / 50
    if (skip < 1) {skip <- 1}
    rw <- seq (r[1], r[length(r)], by=skip)
    for (l in rw) {
      wfx <- -1. * WSC[l] * sin (WDC[l] * pi/180.)
      wfy <- -1. * WSC[l] * cos (WDC[l] * pi / 180.)
      dlt <- 0.001 * WindFlags * wfy * sz
      dlg <- 0.001 * WindFlags * wfx * sz * ap
      lines (c(lon[l], lon[l]+dlg), c(lat[l], lat[l]+dlt), 
              lty=1, col='green')
    }
  }
}



#' @title ggplotWAC
#' @description Convenience routine for plots
#' @details Sets some plot defaults and calls ggplot, using
#' theme_WAC for the plot conventions. This can be followed with
#' calls to lineWAC and legendWAC to add lines and a legend.
#' The routine returns a descriptor for the plot that can be
#' used to repeat the plot with additions. Because the descriptor
#' that is returned is evaluated in the environment of the calling
#' program, the calling arguments must be available in that
#' environment.
#' @aliases ggplotWAC
#' @author William Cooper
#' @import ggplot2
#' @import ggthemes grid reshape2
#' @export ggplotWAC
#' @param data A dataframe containing the variables to plot
#' @param xp Usually, Time from a data.frame; a vector of abscissa values. Default: Data$Time
#' @param yp A vector or set of vectors containing ordinate values for points to plot. 
#' @param col Color or colors to pass to plot (default: blue)
#' @param xlab Label for the abscissa, to pass to plot (default: "TIME [UTC]")
#' @param ylab Label for the ordinate, to pass to plot (default: "")
#' @param lwd Line width to pass to plot (default: 1)
#' @param type Line type to pass to plot (default: "l")
#' @param title A title for the plot
#' @param ... Additional arguments to pass to plot(), but don't include col, xlab, ylab, lwd, type, xaxt or yaxt
#' @return A ggplot descriptor.
#' @examples 
#' \dontrun{plotWAC (Time, TASX, ylab="TAS")}
#' \dontrun{plotWAC (Time, PSXC, lty=2)}
ggplotWAC <- function (data=Data, xp="Time", yp="ATX", col="blue", xlab="TIME [UTC]", 
                     ylab="", lwd=2, type='l', title="", ...) {
  theme_WAC <- theme_gdocs() + theme(
    plot.title = element_text(hjust=0.5),
    axis.text = element_text(size = 16),
    panel.grid.major = element_line(color = "lightblue", linetype=5),
    panel.background = element_rect(fill = "gray95"),
    axis.title=element_text(face="plain",size=18,colour="blue"),
    line=element_line(size=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(-0.35,"cm"),
    axis.ticks.margin = unit (0.6,"cm"),
    legend.position=c(0.5,0.96),
    plot.margin=unit(c(1.5,1,0.5,0.5),"lines"),
    plot.title=element_text(vjust=1.3),
    legend.background=element_rect(fill="white"),
    legend.direction="horizontal",
    panel.border=element_rect(colour="black",size=0.7),
    axis.title.y=element_text(angle=90))
  
  g <- ggplot (data, aes(x=xp))
  xl <- "Time [UTC]"
  clr <- yp
  #col <- c("darkgreen","red","blue")
  if (length(yp) > 1) {
    for (j in 1:length(yp)) {
      a <- eval(sprintf("aes (y=data[,yp[%d]], colour='%s')",j,yp[j]))
      g <-  g + geom_line (eval(parse(text=a)))
    }  
    g <- g + scale_colour_manual("", 
                                 breaks = clr,
                                 values = col)
    g <- g + xlab(xlab)+ylab(ylab) +
      ggtitle(title)+theme_WAC
  } else {
    a <- eval(sprintf("aes (y=data[,yp, colour='%s')",yp))
    g <-  g + geom_line (eval(parse(text=a)))
    g <- g + xlab(xlab)+ylab(ylab)+ggtitle(title)+theme_WAC
  }
  g
  return(g)
  
#   plot(x, y, xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, lwd=lwd, 
#        type=type, col=col, xaxs="r", yaxs="r", ...)
#   if (!is.expression(xlab)) {
#     if (xlab == "TIME [UTC]") {
#       axis.POSIXct(1,x, format='%H:%M', tck=0.02)
#       axis.POSIXct(3,x, labels=NA, tck=0.02)
#     } else {
#       axis(1,tck=0.02)
#       axis(3,labels=NA,tck=0.02)
#     }
#   } else {
#     axis(1,tck=0.02)
#     axis(3,labels=NA,tck=0.02)
#   }
#   axis(2,tck=0.02)
#   axis(4,labels=NA,tck=0.02)
  
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
# lineWAC <- function (x, y, col="blue", lwd=2, type='l', ...) {
#   point(x, y, lwd=lwd, type=type, col=col, ...)
#   return ()
# }
# 
# 
# Flight <- "rf26"
# Project <- "DEEPWAVE"
# DataRoot <- "/home/Data/"
# 
# fname = sprintf("%s%s/%s%s.nc", DataRoot, Project, Project, Flight)
# VarNames <- standardVariables()
# VarNames <- c(VarNames, "ATHR1", "ATHR2", "ATRL", "AT_A", "AT_A2")
# VarNames <- c(VarNames, "EW_DPL", "DP_DPL", "PALTF", "EW_DPR",
#               "DP_DPR", "EW_VXL", "DP_VXL", "CAVP_CR2", 
#               "MIRRTMP_CR2", "DP_CR2")
# VarNames <- c(VarNames, "PSFC", "PS_A", "PS_GP", "QCFC", 
#               "QCRC", "QC_A", "QC_GP")
# VarNames <- c(VarNames, "GGQUAL", "GGVEW", "GGVNS", "VEW", 
#               "VNS", "AKRD", "SSLIP")
# VarNames <- c(VarNames, "CONCU_RWO", "CONCU100_RWO", "CONCU100_RWO", 
#               "CONCU500_RWO", "CNTS")
# VarNames <- c(VarNames, "CLAT_LAMS", "CLON_LAMS", "CPITCH_LAMS",
#               "CROLL_LAMS", "CTHDG_LAMS", "CVEW_LAMS", "CVNS_LAMS",
#               "CVSPD_LAMS", "PITCH", "ROLL", "THDG")
# Data <- getNetCDF (fname, VarNames)
# yp=c("WIC","PITCH", "AKRD")
# xp <- Data$Time
# gx <- ggplotWAC(data=Data,xp=xp,yp=yp,
#                 title="Vertical Wind Components", 
#                 col=c("darkgreen","red", "blue"))
# gx + coord_cartesian(ylim=c(-3.,6.))
# theme_WAC <- theme_gdocs() + theme(
#   plot.title = element_text(hjust=0.5),
#   axis.text = element_text(size = 16),
#   panel.grid.major = element_line(color = "lightblue", linetype=5),
#   panel.background = element_rect(fill = "gray95"),
#   axis.title=element_text(face="plain",size=18,colour="blue"),
#   line=element_line(size=1),
#   axis.ticks = element_line(size=1),
#   axis.ticks.length = unit(-0.35,"cm"),
#   axis.ticks.margin = unit (0.6,"cm"),
#   legend.position=c(0.5,0.96),
#   plot.margin=unit(c(1.5,1,0.5,0.5),"lines"),
#   plot.title=element_text(vjust=1.3),
#   legend.background=element_rect(fill="white"),
#   legend.direction="horizontal",
#   panel.border=element_rect(colour="black",size=0.7),
#   axis.title.y=element_text(angle=90))
# g <- ggplot (Data, aes(x=Time))
# yv <- c("PITCH","WIC","PITCH")
# xl <- "Time [UTC]"
# yl <- "WIC [m/s]"
# clr <- yv
# col <- c("darkgreen","red","blue")
# for (j in 1:3) {
#   a <- eval(sprintf("aes (y=Data[,yv[%d]], colour='%s')",j,yv[j]))
#   g <-  g + geom_line (eval(parse(text=a)))
# }
# # a <- aes (y=Data[,yv[1]], colour=yv[1])
# # #g <- g + geom_line (aes (y = Data[,yv[1]], colour=yv[1]))
# # g <- g + geom_line (a)
# # g <- g + geom_line (aes (y = Data[,yv[2]], colour=yv[2]))
# # g <- g + geom_line (aes (y = Data[,yv[3]], colour=yv[3]))
# if (length(yv) > 1) {
#   g <- g + scale_colour_manual("", 
#                                breaks = clr,
#                                values = col)
#   g <- g + xlab(xl)+ylab(yl) + ylim(-3.,5.) +
#     ggtitle("Vertical Wind Components")+theme_WAC
# } else {
#   g <- g + xlab(xl)+ylab(yl)+ggtitle("Vertical Wind Components")+theme_WAC
# }
# g

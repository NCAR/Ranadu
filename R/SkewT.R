#' @title SkewTSounding
#' @description Plots a specified segment of data from a netCDF file on a skew-T background
#' @details Superimposes a sounding plot on a skew-T diagram. The sounding data can either be
#' plotted as a continuous path or averaged in intervals before plotting. The diagram
#' is generated separately via a "Skew-TDiagram" program, with the plot saved in
#' "SkewT.RData" as plot definition "g". This is used by default, but another
#' plot definition can be specified as the third parameter.
#' @aliases skewTSounding
#' @author William Cooper
#' @export SkewTSounding
#' @import fields
#' @import ggplot2
#' @param Pressure A vector of numeric values representing the pressures [hPa] where
#' temperature and dewpoint values will be provided in the next two parameters. 
#' All three vectors must be of the same length. The default is NA, in which case
#' no data will be plotted but a skew-T background will still be provided. Optionally,
#' this parameter can be a data.frame containing all three variables (Pressure, 
#' Temperature, DewPoint), in which case the second and third parameters can
#' be omitted. The data.frame must have variables with the exact names "Pressure",
#' "Temperature", and "DewPoint", or the alternatives "PSXC", "ATX", "DPXC"; absence 
#' of any causes the function to return NA.
#' @param Temperature A vector of values to plot for the temperature variable [deg.C].
#' The default is NA, in which case no temperature sounding will be plotted.
#' @param DewPoint A vector of values to plot for the dew-point variable [deg.C].
#' The default is NA, in which case no dew-point sounding will be plotted.
#' @param BackgroundSpecs The Rdata-format file containing the ggplot definition for
#' the Skew-T background. Default: "skewTDiagram.Rdata". This file must reside where
#' Ranadu is installed so that it can be found at an
#' address like paste(path.package("Ranadu", "skewTDiagram.Rdata", sep='/')). Two
#' data files are part of the Ranadu package, one suited to typical GV flights and
#' the for the C-130. The latter is "skewTDiagramC130.Rdata".
#' @param AverageInterval The interval in pressure (in hPa) over which to average available
#' measurements before plotting. The default value is 0, and for that value or NA
#' all values are plotted in a continuous "path".
#' @param ADD A logical variable indicating if the data for the sounding should 
#' be returned in a data.frame suitable for addition to a previously generated
#' plot specification (if TRUE) or if an entire new plot should be generated 
#' (if FALSE, the default).
#' @return ggSpecs A ggplot-format definition of the full plot, with background
#' and sounding values included in the plot. Additions can be made to this 
#' specification to add components to the plot (e.g, wind barbs or a hodograph).
#' @examples 
#' print (SkewTSounding (data.frame (Pressure=RAFdata$PSXC, Temperature=RAFdata$ATX, 
#' DewPoint=RAFdata$DPXC)))
#' \dontrun{
#' Directory <- DataDirectory ()
#' Flight <- "rf16"       	
#' Project = "DEEPWAVE"
#' fname = sprintf("%s%s/%s%s.nc", Directory,Project,Project,Flight)
#' Data <- getNetCDF (fname, standardVariables())
#' r <- setRange (Data$Time, 123100, 125500)
#' DS <- Data[r, c("PSXC", "ATX", "DPXC")]
#' colnames(DS) <- c("Pressure", "Temperature", "DewPoint")
#' print (SkewTSounding(DS))
#' }
SkewTSounding <- function (Pressure=NA, Temperature=NA, DewPoint=NA, 
                           BackgroundSpecs="skewTDiagram.Rdata",
                           AverageInterval=0, ADD=FALSE) {
  ## the next lines are just to satisfy devtools::check() that there is a
  ## defined source for these variables; they are overwritten by 'load'
  skewTDiagram <- NA
  pBot <- 1000
  pTop <- 100
  tBot <- -40
  tTop <- 40
  # this loads skewTDiagram and tBot, tTop, pBot, pTop.
  Bvar <- load (paste(path.package ("Ranadu"), BackgroundSpecs, sep='/'))

  g <- skewTDiagram     # just to save some length in later "s <- g + ..." lines
  ## print (ggplot_build(g$panel$ranges[[1]]$x.range))
  ## A function for translation between the P-T coordinates and the skew-T plot coordinates:
  ##    (note, expects tTop etc in calling environment, so not explicitly passed.)
  XYplot <- function (.T, .p) { 
    return (data.frame(
      X=(.T-tBot) / (tTop-tBot) - log10(.p/pBot) / log10(pBot/pTop), 
      Y=log10(.p)))
  }
  OK <- FALSE
  if (is.data.frame(Pressure) ) {
    SKN <- names(Pressure)
    if ("Pressure" %in% SKN) {
      P <- Pressure$Pressure
      if ("Temperature" %in% SKN) {
        AT <- Pressure$Temperature
        if ("DewPoint" %in% SKN) {
          DP <- Pressure$DewPoint
          OK <- TRUE
        }
      }
    } else {
      if ("PSXC" %in% SKN) {
        P <- Pressure$PSXC
        if ("ATX" %in% SKN) {
          AT <- Pressure$ATX
          if ("DPXC" %in% SKN) {
            DP <- Pressure$DPXC
            OK <- TRUE
          }
        }
      } 
    }
    if (!OK) {
      print (sprintf (" SkewT call failed, required names (Pressure, Temperature, DewPoint) not in data.frame, first argument"))
      return(NA)
    }
    
    Pressure <- P; Temperature <- AT; DewPoint <- DP
  } else {
    if ((length(Pressure) != length(Temperature)) || (length(Pressure) != length (DewPoint))) {
      print (sprintf ("SkewT error, variables (P/T/DP) have different length"))
      return (NA)
    }
  }
  
  ## optionally average in specified intervals
  if (AverageInterval > 0) {
    NBins <- as.integer((pBot-pTop)/AverageInterval)
    AVT  <- fields::stats.bin (Pressure, Temperature, NBins)
    AVDP <- fields::stats.bin (Pressure, DewPoint,    NBins)
    Pressure <- AVT$centers
    Temperature <- AVT$stats["mean", ]
    DewPoint <- AVDP$stats["mean", ]
  }

DSKT <- data.frame ("P"=Pressure, "AT"=Temperature, "DP"=DewPoint)
  
  ## convert to plot coordinates:
  DSKT$AT  <- XYplot (Temperature, Pressure)$X
  DSKT$DP <- XYplot (DewPoint, Pressure)$X
  ## clip DP trace at left axis
  DSKT$DP[DSKT$DP < 0] <- 0
  DSKT$P  <- XYplot (Temperature, Pressure)$Y
  if (ADD) {return(DSKT)}
  # requireNamespace ("ggplot2", quietly=TRUE)
  ## this is just to fool devtools into accepting that there is a
  ## source for AT, DP, P:
  AT <- DSKT$AT
  DP <- DSKT$DP
  P <- DSKT$P
  g <- g + geom_path (data=DSKT, aes (x=AT, y=P, color="T"),  lwd=1.0)
  g <- g + geom_path (data=DSKT, aes (x=DP, y=P, color="DP"), lwd=1.0,
                      alpha=0.8)
  g <- g + scale_fill_discrete (breaks=c("DP", "T"))
  g <- g + theme (legend.position=c(0.2,0.85), legend.background=element_rect(fill="ivory"))
  g <- g + labs (color='Measurement')
  g <- g + scale_colour_manual (name = "Measurement", breaks=c("T", "DP"), 
                                values = c("darkgreen", "darkblue"))
  # print (g)
  return (g)
}


## This correction should be subtracted from PITCH to get PITCHC;
## it is the error in pitch, so you get the true value by subtraction
## This calculates corrections for an entire flight in one call.
## D must be a dataframe containing at least VNS, VEW, GGVNS, GGVEW,
## LATC, GGALT, THDG, PITCH, ROLL, and Time
#' @title CorrectPitch
#' @description Calculate a correction to pitch based on the Schuler oscillation.
#' @details Uses measurements of the ground-speed error (as determined by comparison
#' to GPS-measured ground-speed components) to estimate the error in pitch. This
#' works best if long segments of flight are included in the input data.frame so
#' there will be sufficient time to determine the Schuler oscillation well. For
#' high-rate files, calculations are based on 1-Hz data and interpolated because
#' the pitch correction is smoothed to be slowly varying over periods of several
#' minutes.
#' @aliases CorrectPitch
#' @author William Cooper
#' @export CorrectPitch
#' @param D a data.frame containing at least these variables: 
#' VNS, VEW, GGVNS, GGVEW, LATC, GGALT, THDG, PITCH, ROLL
#' @import zoo signal
#' @return c(PitchError, RollError) -- the estimated errors in the 
#' pitch and roll angles [deg], which should be subtracted from 
#' PITCH and ROLL to get the corrected values.
#' @examples 
#' \dontrun{PITCHC <- PITCH - CorrectPitch(D)[, 1]}
CorrectPitch <- function (D) {
  Cradeg <- pi/180
  ## get the data rate
  data.rate <- 1
  if ((D$Time[2]-D$Time[1]) <= 0.04) {data.rate <- 25}
  if ((D$Time[2]-D$Time[1]) <= 0.02) {data.rate <- 50}
  LD <- nrow(D)
  ## for HR, extract a 1-Hz data.frame and work with that, then interpolate/smooth
  if (data.rate > 1) {
    D <- D[(as.numeric(D$Time) %% 1) < 0.01,]
  }
  MaxGap <- 1000
  .vns <- zoo::na.approx (as.vector(D$VNS), maxgap=MaxGap, na.rm = FALSE)
  .vew <- zoo::na.approx (as.vector(D$VEW), maxgap=MaxGap, na.rm = FALSE)
  .ggvns <- zoo::na.approx (as.vector(D$GGVNS), maxgap=MaxGap, na.rm = FALSE)
  .ggvew <- zoo::na.approx (as.vector(D$GGVEW), maxgap=MaxGap, na.rm = FALSE)
  .vns[is.na(.vns)] <- 0
  .vew[is.na(.vew)] <- 0
  .ggvns[is.na(.ggvns)] <- 0
  .ggvew[is.na(.ggvew)] <- 0
  # 1013 points (must be odd) to span about 1/5 Schuler osc. -- about 16.8 min
  NAV <- 1013
  vndot <- signal::sgolayfilt (.vns-.ggvns, 3, NAV, m=1)  # m=1 for first deriv.
  vedot <- signal::sgolayfilt (.vew-.ggvew, 3, NAV, m=1)
  deltaPitchL <- -vndot/Ranadu::Gravity (D$LATC, D$GGALT)
  deltaRollL  <- vedot/Ranadu::Gravity (D$LATC, D$GGALT)
  .hdg <- D$THDG*Cradeg
  deltaPitch <- (sin(.hdg)*deltaRollL + cos(.hdg)*deltaPitchL)/Cradeg
  deltaRoll <- (cos(.hdg)*deltaRollL - sin(.hdg)*deltaPitchL)/Cradeg
  if (data.rate > 1) {
    PC <- vector ("numeric", LD)
    RC <- vector ("numeric", LD)
    L <- length(deltaPitch)
    for (i in 1:(L-1)) {
      for (j in 0:(data.rate-1)) {
        PC[(i-1)*data.rate+j+1] <- deltaPitch[i]+
                                   (j/data.rate)*(deltaPitch[i+1]-deltaPitch[i])
        RC[(i-1)*data.rate+j+1] <- deltaRoll[i]+
                                   (j/data.rate)*(deltaRoll[i+1]-deltaRoll[i])
      }
    }
#     for (j in 0:(data.rate-1)) {
#       PC[L*data.rate+j+1] <- deltaPitch2[L]
#     }    
    C <- c(PC, RC)
  } else {
    C <- c(deltaPitch, deltaRoll)
  }
  dim(C) <- c(LD, 2)
  return (C)
}

## This function is no longer needed by the function CorrectPitch(). 
## It is kept here for reference and for other uses.
## It transforms a vector in the body frame to one in the local frame
## (or the reverse if .reverse=TRUE).
XformLB <- function (bvector, .roll, .pitch, .heading, .reverse=FALSE) {
  Cradeg <- pi/180
  sr <- sin(.roll*Cradeg); cr <- cos(.roll*Cradeg)
  sp <- sin(.pitch*Cradeg); cp <- cos(.pitch*Cradeg)
  sh <- sin(.heading*Cradeg); ch <- cos(.heading*Cradeg)
  if (.reverse) {   # note that entries are in column order
    M <- c(sh*cr+ch*sp*sr, ch*cr-sh*sp*sr, -cp*sr,
           ch*cp, -sh*cp, sp,
           -sh*sr+ch*sp*cr, -ch*sr-sh*sp*cr, -cp*cr)
  } else {
    
    M <- c(sh*cp, ch*cr+sh*sp*sr, ch*sr-sh*sp*cr,
           ch*cp, -sh*cr+ch*sp*sr, -sh*sr-ch*sp*cr,
           -sp, cp*sr, cp*cr)
  }
  dim (M) <- c(3,3)
  return (M %*% bvector)
  
}


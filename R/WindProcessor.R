#' @title WindProcessor
#' @description Calculates new wind variables WDN, WSN, WIN (GV only)
#' @details Constructs new wind variables using a data.frame that contains the required variables 
#' (see 'param' below) as input and adds to that data.frame the new wind variables as 
#' listed in the description above. If it is desirable to apply the Shuler-based correction
#' to pitch, this should be done before calling this function and the variable PITCHC
#' resulting from that correction should be included in the data.frame. (If PITCHC is
#' present, it will be used instead of PITCH. If TASN is present it will be used in 
#' place of TASX. If ATTACK is not present then AKRD, if present, will be used. If
#' SSLIP is not present then SSRD, if present, will be used.)
#' @author Al Cooper
#' @export WindProcessor
#' @aliases windProcessor
#' @import zoo
#' @param data A data.frame containing these variables (and possibly others): 
#' TASX, ATTACK, SSLIP, GGVEW, GGVNS, GGVSPD, VEW, VNS, THDG, ROLL, and PITCH.
#' Note that for some data archives GGVSPD may not be present, so it may be
#' useful to define a new data.frame variable (e.g., Data$GGVSPD <- Data$VSPD_A).
#' Also, (ATTACK and AKRD) and (SSLIP and SSRD) are usually the same, so similar 
#' substitution may be needed in those cases. The routine searches for these
#' candidates and selects the first found: {PITCHC, PITCH}, {TASN, TASX},
#' {GGVSPD, GGVSPD_NVTL, VSPD_A}, {ATTACK, AKRD}, {SSLIP, SSRD}. If any
#' required variables are not found, the function returns the original
#' data.frame unchanged and prints an error message.
#' @param AC Aircraft identifier, either "GV" or "C130"; default "GV"
#' @param CompF Set TRUE to implement the complementary filter that merges
#' ground-speed vectors from the INS and GPS to obtain corrected values
#' of the wind vector.
#' @return The original data.frame is returned with variables WDN, WSN, WIN added. These
#' variables are the new horizontal wind direction and speed and the vertical wind.
#' @examples
#' newData <- WindProcessor (RAFdata)

WindProcessor <- function (data, AC='GV', CompF=TRUE) {
  Cradeg <- pi/180
  Names <- names (data)
  if ("PITCHC" %in% Names) {
    PITCH <- data$PITCHC
  } else {
    if(!("PITCH" %in% Names)) {
      print ("*** ERROR in Wind Processor: Variable PITCH is not present in data.frame.")
      return (data)
    }
    PITCH <- data$PITCH
  }
  if ("TASN" %in% Names) {
    TASX <- data$TASN
  } else {
    if(!("TASX" %in% Names)) {
      print ("*** ERROR in Wind Processor: Variable TASX is not present in data.frame.")
      return (data)
    }
    TASX <- data$TASX
  }

  if ("GGVSPDB" %in% Names) {
    GGVSPD <- data$GGVSPDB
  } else if ('GGVSPD' %in% Names) {
    GGVSPD <- data$GGVSPD
  } else if ("GGVSPD_NVTL" %in% Names) {
    GGVSPD <- data$GGVSPD_NVTL
  } else if ('VSPD_A' %in% Names) {
    GGVSPD <- data$VSPD_A
  } else {
    print ("*** ERROR in Wind Processor: Variable GGVSPD is not present in data.frame.")
    return (data)
  }
  if (!("ATTACK" %in% Names)) {
    if ("AKRD" %in% Names) {
      ATTACK <- data$AKRD
    } else {
      print ("*** ERROR in Wind Processor: Variable ATTACK is not present in data.frame.")
      return(data)
    }
  } else {
    ATTACK <- data$ATTACK
  }
  if (!("SSLIP" %in% Names)) {
    if ("SSRD" %in% Names) {
      SSLIP <- data$SSRD
    } else {
      print ("*** ERROR in Wind Processor: Variable SSLIP is not present in data.frame.")
      return(data)
    }
  } else {
    SSLIP <- data$SSLIP
  }
  RequiredNames <- c("GGVEW", "GGVNS", "VEW", "VNS", "THDG", "ROLL")
  for (N in RequiredNames) {
    if (!(N %in% Names)) {
      print (sprintf("*** ERROR in WindProcessor: Variable %s is not present in data.frame", N))
      return (data)
    } else {
      assign (N, eval(parse(text=sprintf('data$%s', N))))
    }
  }
  d <- data.frame ("U" = TASX)
  SSLIP <- SSLIP * Cradeg; ATTACK <- ATTACK * Cradeg
  PITCH <- PITCH * Cradeg; ROLL <- ROLL * Cradeg; THDG <- THDG * Cradeg
  Rate <- 1
  tg <- data$Time[!is.na(data$Time)]  # protect against missing values at start
  if ((tg[2]-tg[1]) <= 0.045) {Rate <- 25}
  if ((tg[2]-tg[1]) <= 0.025) {Rate <- 50}
  ## correct for aircraft rotation rate
  # LR <- 4.42; LG <- -4.30  ## these are normal GV values
  # LR <- 5.18; LG <- -9.88 (Feb 2015 and later) or -4.93 (pre-Feb 2015)
  Platform <- attr(data, 'Platform')
  if ((length(Platform) > 0) && (Platform == 'N130AR')) {
    AC <- 'C130'
  }
  if (AC == 'C130') {
    LR <- 5.18
    LG <- -9.88
    FlightDate <- attr(data, 'FlightDate')
    if ((length(FlightDate) > 0) && (as.integer(sub('.*/', '', attr(data, 'FlightDate'))) < 2015)) {
	    LG <- -4.93
    }
  }
  else {LR <- 4.42; LG <- -4.30}
  Pdot <- c(0, diff (PITCH)) * Rate  # diff does step-wise differentiation
  Hdot <- c(0, diff (THDG))          # see Rate multiplication few lines down
  Hdot[is.na(Hdot)] <- 0
  Hdot[Hdot > pi] <- Hdot[Hdot > pi] - 2*pi
  Hdot[Hdot < -pi] <- Hdot[Hdot < -pi] + 2*pi
  Hdot <- Hdot * Rate
  d$V <- TASX * tan (SSLIP) - Hdot * LR
  d$W <- TASX * tan (ATTACK) - Pdot * LR
  rw <- as.matrix(d)
  cosphi <- cos (ROLL)
  sinphi <- sin (ROLL)
  costheta <- cos (PITCH)
  sintheta <- sin (PITCH)
  cospsi <- cos (THDG)
  sinpsi <- sin (THDG)
  DL <- length(TASX)
  One <- rep (1, DL)
  Z <- rep (0, DL)
  T1 <- aperm(array (c(One,Z,Z,Z,cosphi,-sinphi,Z,sinphi,cosphi), 
                     dim=c(DL,3,3)))
  T2 <- aperm(array (c (costheta,Z,sintheta,Z,One,Z,-sintheta,Z,costheta), 
                     dim=c(DL,3,3)))
  T3 <- aperm(array (c (cospsi,-sinpsi,Z,sinpsi,cospsi,Z,Z,Z,One), 
                     dim=c(DL,3,3)))
  WDN <- vector ("numeric", DL)
  WSN <- vector ("numeric", DL)
  WIN <- vector ("numeric", DL)
  # VNS <- zoo::na.approx (as.vector(VNS), maxgap=1000, na.rm = FALSE)
  # VEW <- zoo::na.approx (as.vector(VEW), maxgap=1000, na.rm = FALSE)
  # GGVNS <- zoo::na.approx (as.vector(GGVNS), maxgap=1000, na.rm = FALSE)
  # GGVEW <- zoo::na.approx (as.vector(GGVEW), maxgap=1000, na.rm = FALSE)
  
  ## corrections for GPS-to-INS distance (GV)
  GGVNS <- GGVNS + LG * Hdot * sinpsi
  GGVEW <- GGVEW - LG * Hdot * cospsi
  
  CVEW <- GGVEW - VEW
  CVNS <- GGVNS - VNS
  CVEW <- zoo::na.approx (as.vector(CVEW), maxgap=1000, na.rm = FALSE)
  CVNS <- zoo::na.approx (as.vector(CVNS), maxgap=1000, na.rm = FALSE)
  GGVSPD <- zoo::na.approx (as.vector(GGVSPD), maxgap=1000, na.rm = FALSE)
  VNS <- zoo::na.approx (as.vector(VNS), maxgap=1000, na.rm = FALSE)
  VEW <- zoo::na.approx (as.vector(VEW), maxgap=1000, na.rm = FALSE)
  GGVSPD <- GGVSPD - LG * Pdot  ## positive contribution for Pdot positive
  VNS[is.na(VNS)] <- 0
  VEW[is.na(VEW)] <- 0
  GGVNS[is.na(GGVNS)] <- 0
  GGVEW[is.na(GGVEW)] <- 0
  CVNS[is.na(CVNS)] <- 0
  CVEW[is.na(CVEW)] <- 0
  
  # CVEW <- ComplementaryFilter (VEW, GGVEW, 150)
  # CVNS <- ComplementaryFilter (VNS, GGVNS, 150)
  
  tau <- 150
  if (CompF) {
    CVEW <- VEW + signal::filter (signal::butter(3, 2/tau), CVEW)
    CVNS <- VNS + signal::filter (signal::butter(3, 2/tau), CVNS)
  } else {
    CVEW <- VEW
    CVNS <- VNS
  }  
  Hlast <- 0.
 
  for (i in 1:DL) {
    Y1 <- T1[,,i] %*% matrix (rw[i,], 3, 1)
    Y2 <- T2[,,i] %*% Y1
    Y3 <- T3[,,i] %*% Y2
    WG <- matrix (c(-CVNS[i], -CVEW[i], GGVSPD[i]), 
                  3, 1)
    Y4 <- Y3 + WG
    WDN[i] <- atan2 (Y4[2], Y4[1]) / Cradeg
    if ((!is.na(WDN[i])) & (WDN[i] < 0.)) {
      WDN[i] <- WDN[i] + 360.
    }
    WSN[i] <- sqrt (Y4[1]**2 + Y4[2]**2)
    WIN[i] <- Y4[3]
  }
  # detach (data)
  data$WDN <- WDN
  data$WSN <- WSN
  data$WIN <- WIN
  return (data)
}

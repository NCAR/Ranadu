#' @title WindProcessor
#' @description Calculates new wind variables WDN, WSN, WIN
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
#' @import signal
#' @param data A data.frame containing these variables: 
#' TASX, ATTACK, SSLIP, GGVEW, GGVNS, GGVSPD, VEW, VNS, THDG, ROLL, and PITCH.
#' @return The original data.frame is returned with variables WDN, WSN, WIN added. These
#' variables are the horizontal wind direction and speed and the vertical wind.
WindProcessor <- function (data=Data) {
  Cradeg <- pi/180
  Names <- names (data)
  attach (data)
  if ("PITCHC" %in% Names) {
    PITCH <- PITCHC
  } else {
    if(!("PITCH" %in% Names)) {
      print ("*** ERROR in Wind Processor: Variable PITCH is not present in data.frame.")
      return (data)
    }
  }
  if ("TASN" %in% Names) {
    TASX <- TASN
  } else {
    if(!("TASX" %in% Names)) {
      print ("*** ERROR in Wind Processor: Variable TASX is not present in data.frame.")
      return (data)
    }
  }
  if ("GGVSPDB" %in% Names) {
    GGVSPD <- GGVSPDB
  } else if ("GGVSPD_NVTL" %in% Names) {
    GGVSPD <- GGVSPD_NVTL
  } else {
    if(!("GGVSPD" %in% Names)) {
      print ("*** ERROR in Wind Processor: Variable GGVSPD is not present in data.frame.")
      return (data)
    }
  }
  if (!("ATTACK" %in% Names)) {
    if ("AKRD" %in% Names) {
      ATTACK <- AKRD
    } else {
      print ("*** ERROR in Wind Processor: Variable ATTACK is not present in data.frame.")
      return(data)
    }
  }
  if (!("SSLIP" %in% Names)) {
    if ("SSRD" %in% Names) {
      SSLIP <- SSRD
    } else {
      print ("*** ERROR in Wind Processor: Variable SSLIP is not present in data.frame.")
      return(data)
    }
  }
  RequiredNames <- c("GGVEW", "GGVNS", "VEW", "VNS", "THDG", "ROLL")
  for (N in RequiredNames) {
    if (!(N %in% Names)) {
      print (sprintf("*** ERROR in WindProcessor: Variable %s is not present in data.frame", N))
      return (data)
    }
  }
  d <- data.frame ("U" = TASX)
  SSLIP <- SSLIP * Cradeg; ATTACK <- ATTACK * Cradeg
  PITCH <- PITCH * Cradeg; ROLL <- ROLL * Cradeg; THDG <- THDG * Cradeg
  d$V <- TASX * tan (SSLIP)
  d$W <- TASX * tan (ATTACK)
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
  VNS <- zoo::na.approx (as.vector(VNS), maxgap=1000, na.rm = FALSE)
  VEW <- zoo::na.approx (as.vector(VEW), maxgap=1000, na.rm = FALSE)
  GGVNS <- zoo::na.approx (as.vector(GGVNS), maxgap=1000, na.rm = FALSE)
  GGVEW <- zoo::na.approx (as.vector(GGVEW), maxgap=1000, na.rm = FALSE)
  VNS[is.na(VNS)] <- 0
  VEW[is.na(VEW)] <- 0
  GGVNS[is.na(GGVNS)] <- 0
  GGVEW[is.na(GGVEW)] <- 0
  
  CVEW <- ComplementaryFilter (VEW, GGVEW, 150)
  CVNS <- ComplementaryFilter (VNS, GGVNS, 150)
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
  detach (data)
  data$WDN <- WDN
  data$WSN <- WSN
  data$WIN <- WIN
  return (data)
}

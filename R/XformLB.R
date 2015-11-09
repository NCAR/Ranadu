#' @title XformLB
#' @description Transform a vector from b-frame to l-frame.
#' @details Apply rotations specified by ROLL, PITCH, THDG in the supplied
#' data.frame to transform a vector (BX, BY, BZ) in the body frame of the
#' aircraft (where x is forward, y starboard, z down) to the local or ENU 
#' frame with x-east, y-north, z-up. 
#' @aliases XformLB
#' @author William Cooper
#' @export XformLB
#' @param data A data.frame containing at least ROLL, PITCH, THDG (true
#' heading) in units of degrees, possibly in many rows representing time
#' series, and a body-frame vector with components BX, BY, BZ.
#' @import zoo
#' @return The same data.frame with added variables LX, LY, LZ representing
#' the l-frame coordinates of the vector.
#' @examples 
#' newDataFrame <- XformLB (data.frame("ROLL"=1:50, "PITCH"=(3+(1:50)/50), "THDG"=91:140,
#'                                    "BX"=21:70, "BY"=31:80, "BZ"=41:90))

XformLB <- function (data=Data) { # Data must contain ROLL, PITCH, HEADING, BX, BY, BZ
  Cradeg <- pi/180
  if ("PITCHC" %in% names(data)) {
    PITCH <- data$PITCHC
  } else {
    PITCH <- data$PITCH
  }
  ## note minus signs in the next line
  PITCH <- -PITCH * Cradeg; ROLL <- -data$ROLL * Cradeg
  THDG <- data$THDG * Cradeg
  
  cosphi <- cos (ROLL)
  sinphi <- sin (ROLL)
  costheta <- cos (PITCH)
  sintheta <- sin (PITCH)
  cospsi <- cos (THDG)
  sinpsi <- sin (THDG)
  d <- data.frame("X" = data$BX)
  d$Y <- data$BY
  d$Z <- data$BZ    ## for body accels, need to add G to BZ before calling 
  ## it was removed from sensed accel.
  A <- as.matrix(d)
  DL <- nrow(data)
  One <- rep (1, DL)
  ZZ <- rep (0, DL)
  T1 <- aperm(array (c(One,ZZ,ZZ,ZZ,cosphi,-sinphi,ZZ,sinphi,cosphi), 
                     dim=c(DL,3,3)))
  T2 <- aperm(array (c (costheta,ZZ,sintheta,ZZ,One,ZZ,-sintheta,ZZ,costheta), 
                     dim=c(DL,3,3)))
  T3 <- aperm(array (c (cospsi,-sinpsi,ZZ,sinpsi,cospsi,ZZ,ZZ,ZZ,One), 
                     dim=c(DL,3,3)))
  AX <- vector ("numeric", DL)
  AY <- vector ("numeric", DL)
  AZ <- vector ("numeric", DL)
  X <- zoo::na.approx (as.vector(d$X), maxgap=1000, na.rm = FALSE)
  Y <- zoo::na.approx (as.vector(d$Y), maxgap=1000, na.rm = FALSE)
  Z <- zoo::na.approx (as.vector(d$Z), maxgap=1000, na.rm = FALSE)
  X[is.na(X)] <- 0
  Y[is.na(Y)] <- 0
  Z[is.na(X)] <- 0
  
  for (i in 1:DL) {
    Y1 <- T1[,,i] %*% matrix (A[i,], 3, 1)
    Y2 <- T2[,,i] %*% Y1
    Y3 <- T3[,,i] %*% Y2
    AX[i] <- Y3[1]; AY[i] <- Y3[2]; AZ[i] <- Y3[3]
  }
  data$LX <- AY
  data$LY <- AX
  data$LZ <- AZ
  return (data)
}

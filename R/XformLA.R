#' @title XformLA
#' @description Transform a vector from a-frame to l-frame.
#' @details Apply rotations specified by ROLL, PITCH, THDG in the supplied
#' data.frame to transform a vector (BX, BY, BZ) in the body frame of the
#' aircraft (where x is forward, y starboard, z down) to the local or ENU 
#' frame with x-east, y-north, z-up. 
#' @aliases XformLA
#' @author William Cooper
#' @export XformLA
#' @param data A data.frame containing at least ROLL, PITCH, THDG (true
#' heading) in units of degrees, possibly in many rows representing time
#' series
#' @param Avector A body-frame matrix with three components (x,y,z) in
#' the aircraft reference frame where x is forward, y is starboard, and
#' z is downward. For transforming accelerations, body-normal-acceleration
#' is normally measured in the -z direction in the aircraft frame and has
#' the acceleration of gravity G subtracted, so G should be added before
#' transforming and subtracted afterward. The matrix should have 3 columns
#' representing the components and a number of rows corresponding to the
#' number of observations.
#' @param .inverse Logical, transform from l-frame to a-frame if TRUE
#' @import zoo
#' @return The vector components transformed to l-frame or ENU coordinates, local
#' Earth-relative with x east, y north, and z upward. Same structure as Avector.
#' @examples 
#' newDataFrame <- XformLA (data.frame("ROLL"=1:50, "PITCH"=(3+(1:50)/50), "THDG"=91:140),
#'                                    Avector=matrix(c(21:70, 31:80, 41:90), ncol=3))

XformLA <- function (data, Avector, .inverse=FALSE) { 
  # data must contain ROLL, PITCH or PITCHC, HEADING
  Cradeg <- pi/180
  if ("PITCHC" %in% names(data)) {
    PITCH <- data$PITCHC
  } else {
    PITCH <- data$PITCH
  }
  PITCH <- PITCH * Cradeg; ROLL <- data$ROLL * Cradeg
  THDG <- data$THDG * Cradeg
  
  cosphi <- cos (ROLL)
  sinphi <- sin (ROLL)
  costheta <- cos (PITCH)
  sintheta <- sin (PITCH)
  cospsi <- cos (THDG)
  sinpsi <- sin (THDG)
#   d <- data.frame("X" = data$BX)
#   d$Y <- data$BY
#   d$Z <- data$BZ    ## for body accels, need to add G to BZ before calling 
#   ## it was removed from sensed accel.
#   A <- as.matrix(d)
  DL <- nrow(data)
#   One <- rep (1, DL)
#   ZZ <- rep (0, DL)
#   T1 <- aperm(array (c(One,ZZ,ZZ,ZZ,cosphi,-sinphi,ZZ,sinphi,cosphi), 
#                      dim=c(DL,3,3)))
#   T2 <- aperm(array (c (costheta,ZZ,sintheta,ZZ,One,ZZ,-sintheta,ZZ,costheta), 
#                      dim=c(DL,3,3)))
#   T3 <- aperm(array (c (cospsi,-sinpsi,ZZ,sinpsi,cospsi,ZZ,ZZ,ZZ,One), 
#                      dim=c(DL,3,3)))
#   AX <- vector ("numeric", DL)
#   AY <- vector ("numeric", DL)
#   AZ <- vector ("numeric", DL)
# #   X <- zoo::na.approx (as.vector(d$X), maxgap=1000, na.rm = FALSE)
# #   Y <- zoo::na.approx (as.vector(d$Y), maxgap=1000, na.rm = FALSE)
# #   Z <- zoo::na.approx (as.vector(d$Z), maxgap=1000, na.rm = FALSE)
# #   X[is.na(X)] <- 0
# #   Y[is.na(Y)] <- 0
# #   Z[is.na(X)] <- 0
#   
#   for (i in 1:DL) {
#     Y1 <- T1[,,i] %*% matrix (A[i,], 3, 1)
#     Y2 <- T2[,,i] %*% Y1
#     Y3 <- T3[,,i] %*% Y2
#     AX[i] <- Y3[1]; AY[i] <- Y3[2]; AZ[i] <- Y3[3]
#   }
#   data$LX <- AY
#   data$LY <- AX
#   data$LZ <- AZ
  ## now try this another way:
  Rbl <- c(sinpsi*costheta, cospsi*cosphi+sinpsi*sintheta*sinphi, cospsi*sinphi-sinpsi*sintheta*cosphi,
           cospsi*costheta, -sinpsi*cosphi+cospsi*sintheta*sinphi, -sinpsi*sinphi-cospsi*sintheta*cosphi,
           -sintheta, costheta*sinphi, -costheta*cosphi)
  RblM <- aperm( array (Rbl, dim=c(DL,3,3)))
  AA <- matrix(nrow=DL, ncol=3)
  if (.inverse) {
    for (i in 1:DL) {
      AA[i,] <- t(RblM[,,i]) %*% Avector[i,]
    }
  } else {
    for (i in 1:DL) {
      AA[i,] <- RblM[,,i] %*% Avector[i,]
    }
  }
  ## much slower:
#   AA <- aperm(mapply(FUN='%*%',
#                     lapply(X=apply(RblM, 3, data.frame), FUN=as.matrix, nrow=3, ncol=3),
#                     as.data.frame (aperm(A))))
#  data$LX2 <- AA[,1]; data$LY2 <- AA[,2]; data$LZ2 <- AA[,3]
  
  return (AA)
}

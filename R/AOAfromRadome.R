#' @title GV_AOAfromRadome
#' @description Calculate angle of attack from radome pressures
#' @details based on calibration, applies model for AOA given ADIFR, QCF, and Mach Number
#' @aliases GV_AOAfromRadome
#' @author William Cooper
#' @export GV_AOAfromRadome
#' @param ADIFR The pressure difference between top and bottom radome ports
#' @param QCF  The dynamic pressure (use consistent units for ADIFR and QCF.) 
#' By convention, this calibration related to the uncorrected value QCF, not
#' the PCorFunction()-corrected value QCFC. 
#' @param MACH  The Mach number
#' @param CF Three-coefficient vector representing the standard-formula calibration
#' coefficients of the radome. Default is c(4.605, 18.44, 6.75). Cf. the document
#' on ProcessingAlgorithms.pdf. These are the default values for the NSF/NCAR GV.
#' @return The angle of attack in degrees (possibly a vector)
#' @examples 
#' AOA <- GV_AOAfromRadome (RAFdata$ADIFR, RAFdata$QCXC, 
#'        MachNumber (RAFdata$PSXC, RAFdata$QCXC))
GV_AOAfromRadome <- function (ADIFR, QCF, MACH=0.8, CF=c(4.605, 18.44, 6.75)) {
  return (as.vector (CF[1] + (ADIFR/QCF)*(CF[2] + CF[3] * MACH)))
} 

#' @title GV_YawFromRadome
#' @description Calculate sideslip angle from radome pressures
#' @details based on calibration, applies model for sideslip given BDIFR and QCXC.
#' This is the standard calibration in use for the NSF/NCAR GV.
#' @aliases GV_YawFromRadome
#' @author William Cooper
#' @export GV_YawFromRadome
#' @param BDIFR The pressure difference between starboard and port radome pressures
#' @param QCXC  The dynamic pressure (use consistent units for BDIFR and QCXC) 
#' @return The sideslip angle in degrees (possibly a vector)
#' @examples 
#' SS <- GV_YawFromRadome (RAFdata$BDIFR, RAFdata$QCXC)
GV_YawFromRadome <- function (BDIFR, QCXC) {
  b0 = -0.0025
  b1 = 1. / 0.04727
  return (as.vector (b1 * ((BDIFR / QCXC) + b0)))
} 

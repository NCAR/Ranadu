#' @title GV_AOAfromRadome
#' @description Calculate angle-of-attack from radome pressures
#' @details based on calibration, applies model for AOA given ADIFR, QCXC, and Mach Number
#' @aliases GV_AOAfromRadome
#' @author William Cooper
#' @export GV_AOAfromRadome
#' @param ADIFR The pressure difference between top and bottom radome ports
#' @param QCXC  The dynamic pressure (use consistent units for ADIFR and QCXC) 
#' @param MACH  The Mach number
#' @return The angle of attack in degrees (possibly a vector)
#' @examples 
#' \dontrun{a <- GV_AOAfromRadome (ADIFR, QCXC, MACH)}
GV_AOAfromRadome <- function (ADIFR, QCXC, MACH) {
  c0 <- 4.604
  c1 <- 18.67
  c2 <- 6.49
  akrd <- c0 + (ADIFR/QCXC)*(c1 + c2 * MACH)
  return (akrd)
}

#' @title GV_YawFromRadome
#' @description Calculate sideslip angle from radome pressures
#' @details based on calibration, applies model for sideslip given BDIFR and QCXC
#' @aliases GV_YawFromRadome
#' @author William Cooper
#' @export GV_YawFromRadome
#' @param BDIFR The pressure difference between starboard and port radome pressures
#' @param QCXC  The dynamic pressure (use consistent units for BDIFR and QCXC) 
#' @return The sideslip angle in degrees (possibly a vector)
#' @examples 
#' \dontrun{a <- GV_YawFromRadome (BDIFR, QCXC)}
GV_YawFromRadome <- function (BDIFR, QCXC) {
  b0 = -0.0025
  b1 = 1./0.04727
  ssrd <- b1*((BDIFR/QCXC)-b0)
  return (ssrd)
}
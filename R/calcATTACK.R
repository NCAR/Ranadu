#' @title calcAttack
#' @description Calculate angle-of-attack
#' @details Assumes vertical wind = 0, finds AOA from pitch, VSPD and TASX. Used to
#' provide a reference value for calibrating gust probes.
#' @aliases calcAttack
#' @author William Cooper
#' @export calcAttack
#' @param PITCH The pitch angle in degrees
#' @param VSPD  The rate of climb of the aircraft in m/s 
#' @param TASX  The true airspeed of the aircraft in m/s
#' @examples 
#' AOAREF <- calcAttack (RAFdata$PITCH, RAFdata$GGVSPD, RAFdata$TASX)
calcAttack <- function (PITCH, VSPD, TASX) {
  aoa <- PITCH - (180./pi) * asin (VSPD/TASX)
  return (aoa)
}

#' @title TrueAirspeed
#' @description Returns the true airspeed 
#' @details Calculates the true airspeed from measurements of Mach number, air temperature, and optionally humidity
#' @aliases trueAirspeed
#' @author William Cooper
#' @export TrueAirspeed
#' @param MACH A numeric vector representing the Mach number, possibly from MachNumber()
#' @param AT A numeric vector representing the air temperature in deg.C
#' @param EoverP An optional numeric vector representing the ratio of water vapor pressure to total pressure
#' @return A numeric vector giving the true airspeed in m/s
#' @examples 
#' TAS <- TrueAirspeed (MachNumber (RAFdata$PSXC, RAFdata$QCXC), RAFdata$ATX, RAFdata$EWX/RAFdata$PSXC)

TrueAirspeed <- function (MACH, AT, EoverP=0.) {
# TAS from Mach Number and temperature:
  CP <- SpecificHeats (EoverP)
  gamma <- CP[,1]/CP[,2]
  return (as.vector (MACH * (gamma*CP[,3] * (AT + StandardConstant ("Tzero")))^0.5))
}


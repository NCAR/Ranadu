TZERO <- StandardConstant("Tzero")
#' @title TrueAirspeed
#' @description Returns the true airspeed 
#' @details Calculates the true airspeed from measurements of Mach number, air temperature, and optionally humidity
#' @aliases TrueAirspeed trueairspeed
#' @author William Cooper
#' @export TrueAirspeed
#' @param MACH A numeric representing the Mach number, possible from MachNumber()
#' @param AT A numeric representing the air temperature in deg.C
#' @param EoverP An optional numeric representing the ratio of water vapor pressure to total pressure
#' @return The true airspeed in m/s
#' @examples 
#' TAS <- TrueAirspeed (0.3, 10.)
#' TAS <- TrueAirspeed (0.5, 0., 6.11/800.)
#' TAS <- TrueAirspeed (MachNumber (RAFdata$PSXC, RAFdata$QCXC), RAFdata$ATX)
TrueAirspeed <- function (MACH, AT, EoverP=0.) {
# TAS from Mach Number and temperature:
  CP <- SpecificHeats (EoverP)
  gamma <- CP[,1]/CP[,2]
  TAS <- MACH * (gamma*CP[,3]*(AT+TZERO))**0.5
  return (TAS)
}


#' @title AirTemperature
#' @description Calculates the ambient air temperature
#' @details Finds the air temperature from measurements of recovery temperature and ambient and dynamic pressure, with optional correction for humidity
#' @author William Cooper
#' @export AirTemperature
#' @param RT A numeric representing the recovery temperature in deg. C
#' @param P  A numeric representing the ambient pressure in hPa
#' @param Q  A numeric representing the dynamic pressure in hPa
#' @param E  An optional numeric representing the water vapor pressure in hPa
#' @return The ambient air temperature in deg.C
#' @examples 
#' AT <- AirTemperature (10., 700., 50.)
AirTemperature <- function (RT, P, Q, E=0.) {
# Find air temperature from recovery temperature, MACH, and
# humidity: (RT in deg C, P, Q, E all in the same units,
# E used for humidity correction; omit for dry-air)
  x <- E/P
  CP <- SpecificHeats(x)
  Ra <- CP[,3]  
  cv <- CP[,2]  
  MACH <- MachNumber (P, Q, E)
  AT <- (RT+TZERO)/
    (1.+RecoveryFactor(MACH)*MACH**2 *Ra/(2.*cv))-TZERO
  return (AT)
}


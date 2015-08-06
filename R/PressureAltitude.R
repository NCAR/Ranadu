#' @title PressureAltitude
#' @description Pressure altitude in the ISA standard atmosphere
#' @details Calculates the altitude in the ISA standard atmosphere corresponding to the specified pressure
#' @aliases PressureAltitude
#' @author William Cooper
#' @export PressureAltitude
#' @param P A numeric representing pressure in hPa 
#' @return The pressure altitude in meters
#' @examples 
#' PALT <- PressureAltitude (500.)
PressureAltitude <- function (P) {
# Pressure altitude formula: (P in hPa)
  PALT <- 11000.+(8314.32*216.65)/(9.80665*28.9644)*log(226.3206/P)
  PALT[P > 226.3206] <- (288.15/0.0065)*(1-(P[P > 226.3206]/1013.25)**0.1902632)
#   if (P > 226.3206) {
#     PALT = (288.15/0.0065)*(1-(P/1013.25)**0.1902632)
#   } else {
#     PALT = 11000.+(8314.32*216.65)/(9.80665*28.9644)*log(226.3206/P)
#   }
  return (PALT)
}


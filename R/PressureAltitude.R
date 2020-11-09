#' @title PressureAltitude
#' @description Pressure altitude in the ISA standard atmosphere
#' @details Calculates the altitude in the ISA standard atmosphere corresponding to the specified pressure
#' @aliases pressureAltitude
#' @author William Cooper
#' @export PressureAltitude
#' @param P A numeric vector representing pressure in hPa 
#' @return A numeric vector giving the pressure altitude in meters
#' @examples 
#' PALT <- PressureAltitude (500.)
#' PALT2 <- PressureAltitude (RAFdata$PSXC)
PressureAltitude <- function (P) {
# Pressure altitude formula: (P in hPa)
	Ru <- 8314.32
	gzero <- 9.80665
	MWD <- 28.9644
  exp1 <- Ru * 0.0065 / (MWD * gzero)
  exp2 <- -Ru * 0.001 / (MWD * gzero)
  exp3 <- -Ru * 0.0028 / (MWD * gzero)
  p1 <- 1013.25 * (1 - 0.0065 * 11000 / 288.15)^(1/exp1)
  p2 <- p1 * exp(-gzero * MWD / (216.65 * Ru) * 9000)
  p3 <- p2 * (1 + 0.001 * 12000 / 216.65)^(1/exp2)
  p4 <- p3 * (1 + 0.0028 * 15000 / 228.65)^(1/exp3)
  PALT <- 11000.+(Ru * 216.65) / (gzero * MWD) * log(p1 / P)
  PALT[P > p1] <- (288.15 / 0.0065) * (1 -(P[P > p1] / 1013.25)^exp1)
  PALT[P < p2] <- 20000 - (216.65 / 0.001) * (1 - (P[P < p2] / p2)^exp2)
  PALT[P < p3] <- 32000 - (228.65 / 0.0028) * (1 - (P[P < p3] / p3)^exp3)
  PALT[P < p4] <- 47000 + (Ru * 270.65) / (gzero * MWD) * log(p4 / P[P < p4])
#   if (P > 226.3206) {
#     PALT = (288.15/0.0065)*(1-(P/1013.25)**0.1902632)
#   } else {
#     PALT = 11000.+(8314.32*216.65)/(9.80665*28.9644)*log(226.3206/P)
#   }
  return (as.vector(PALT))
}


#' @title MixingRatio
#' @description Calculate the mixing ratio of water vapor
#' @details The dimensionless mixing ratio of water vapor is calculated from the ratio of water vapor pressure to total pressure.
#' @aliases MixingRatio 
#' @author William Cooper
#' @export MixingRatio
#' @param EoverP A numeric representing the ratio of water vapor pressure to total pressure.
#' @return A numeric representing the mixing ratio in dimensionless units (*NOT* g/kg)
#' @examples 
#' MR <- MixingRatio (3./800.)
MixingRatio <- function (EoverP) {
# Mixing ratio (E/P); returns the
# dimensionless mixing ratio, *NOT* g/kg
  r <- (18.0153/28.9637) * (EoverP/(1.-EoverP))
  return (r)
}


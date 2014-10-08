#' @title SpecificHeats
#' @description Specific heats and gas constant for moist or dry air
#' @details Calculates the specific heat at constant pressure, specific heat at constant volume, and gas constant for air having ratio of water vapor pressure to total pressure of the argument EoverP
#' @aliases SpecificHeats SpecificHeat specificheats specificheat
#' @author William Cooper
#' @export SpecificHeats
#' @param EoverP A numeric giving the ratio of water vapor pressure to total pressure
#' @return an array of dimension (n,3) containing n rows of (Cp, Cv, R), where n is the number of elements in EoverP
#' @examples 
#' CP <- SpecificHeats()    # gives dry-air valuies, single row
#' CP <- SpecificHeats(6.11/800.)
#' \dontrun{CP <- (EWX/PSXC) # returns array if EWX and PSXC are vectors}
SpecificHeats <- function (EoverP=0.) {
# calculate humidity-dependent gas constant and specific
# heats, or dry-air values if EoverP is 0:
  Rd <- StandardConstant("Rd")
  Ra <- Rd/(1.+(StandardConstant("MWW")/StandardConstant("MWD")-1.)*EoverP)
  cp <- StandardConstant("cpd")*(Ra/Rd)*(1.+EoverP/5.)
  cv <- StandardConstant("cvd")*(Ra/Rd)*(1.+EoverP/7.)
  L <- length(EoverP)
  R <- c(cp,cv,Ra)
  dim(R) <- c(L,3)
  return (R)
}


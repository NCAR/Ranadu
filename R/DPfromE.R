require(nleqslv)
#' @title DPfromE 
#' @description Finds the dew point given the water vapor pressure
#' @details The Murphy-Koop equation for water vapor pressure is inverted to find dew point (in deg. C) from water vapor pressure (in hPa). The inversion uses the R routine nleqslv.
#' @aliases DPfromE
#' @author William Cooper
#' @import nleqslv
#' @export DPfromE
#' @param E A numeric representing the water vapor pressure in hPa 
#' @return A numeric representing the dewpoint in deg. C corresponding to the input vapor pressure.
#' @examples 
#' DP <- DPfromE (0.01)
DPfromE <- function (E) {
# Given water vapor pressure E, find the dewpoint that gives
# this same vapor pressure:
  assign("Ereference", E, envir = .GlobalEnv)  # global variable for MKerror
  #print (Ereference)
  A <- nleqslv (-10.,MKerror, method="Newton")
  return (A$x)
}

#' @title MKerror
#' @description Internal function used by DPfromE()
#' @details Internal function used for inverting the Murphy-Koop formula to find dew-point as a function of water vapor pressure
#' @aliases MKerror 
#' @author William Cooper
#' @param DP A numeric representing the dew point in deg. C
#' @return The error between the vapor pressure corresponding to the input dew point and the reference vapor pressure (Ereference) set before calling this function.
#' @examples 
#' \dontrun{err <- MKerror (10.)}
MKerror <- function (DP) {
  # Internal function for inverting the Murphy-Koop formula to 
  # find dewpoint as a function of water vapor pressure.
  # Set global Ereference before calling. Called by DPfromE;
  # probably no other uses
  return (MurphyKoop(DP)-Ereference)
}


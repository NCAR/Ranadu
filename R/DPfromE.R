#' @title DPfromE 
#' @description Finds the dew point given the water vapor pressure
#' @details The Murphy-Koop equation for water vapor pressure is inverted to find 
#' the dew point (in deg. C) from the water vapor pressure (in hPa). The inversion uses the 
#' R routine nleqslv. This gives the temperature where the equilibrium vapor pressure 
#' matches the supplied vapor pressure, but without consideration of the "enhancement factor"
#' that changes the equilibrium pressure in the presence of air or another gas.
#' @aliases DPfromE
#' @author William Cooper"
#' @export DPfromE
#' @import nleqslv
#' @param .E A numeric representing the water vapor pressure in hPa (vector OK)
#' @return A numeric vector representing the dewpoint in deg. C corresponding to the input vapor pressure.
#' @examples 
#' DP <- DPfromE (0.01)
#' DPa <- DPfromE (RAFdata$EWX)
DPfromE <- function (.E) {
# Given water vapor pressure E, find the dewpoint that gives
# this same vapor pressure:
  MKerror <- function (.DP, .Ereference) {
    # Internal function for inverting the Murphy-Koop formula to 
    # find dewpoint as a function of water vapor pressure. Called by DPfromE;
    # probably no other uses
    return (MurphyKoop(.DP)-.Ereference)
  }
  LE <- length (.E)
  RDP <- vector ("numeric", LE)
  for (i in 1:LE) {
    RDP[i] <- nleqslv::nleqslv (-10., fn=MKerror, jac=NULL, method="Newton", .E[i])$x
  }
  return (RDP)
}




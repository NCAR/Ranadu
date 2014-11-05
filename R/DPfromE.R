#' @title DPfromE 
#' @description Finds the dew point given the water vapor pressure
#' @details The Murphy-Koop equation for water vapor pressure is inverted to find dew point (in deg. C) from water vapor pressure (in hPa). The inversion uses the R routine nleqslv.
#' @aliases DPfromE
#' @author William Cooper"
#' @export DPfromE
#' @param E A numeric representing the water vapor pressure in hPa 
#' @return A numeric representing the dewpoint in deg. C corresponding to the input vapor pressure.
#' @examples 
#' DP <- DPfromE (0.01)
DPfromE <- function (.E) {
# Given water vapor pressure E, find the dewpoint that gives
# this same vapor pressure:
  Ereference <- .E
  MKerror <- function (.DP, .Ereference) {
    # Internal function for inverting the Murphy-Koop formula to 
    # find dewpoint as a function of water vapor pressure.
    # Set global Ereference before calling. Called by DPfromE;
    # probably no other uses
    return (MurphyKoop(.DP)-.Ereference)
  }
  A <- nleqslv::nleqslv (-10.,MKerror, method="Newton", Ereference)
  return (A$x)
}




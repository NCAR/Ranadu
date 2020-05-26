#' @title AdiabaticTandLWC
#' @description Calculate the temperature and liquid water content for adiabatic ascent. 
#' @details For reversible ascent, this function calculates the final 
#' temperature and liquid water content given an initial pressure, 
#' temperature, and liquid water content. The starting point will often 
#' be the cloud base, and .lwc will be zero, but the routine doesn't 
#' require this and if given a subsaturated vapor pressure will
#' calculate reversible ascent below cloud base and produce zero LWC
#' for final pressures below cloud base. If the optional vapor pressure
#' at the starting point is zero or omitted the equilibrium vapor pressure at the
#' starting-point temperature will be used.
#' @aliases AdiabaticLWC
#' @author William Cooper
#' @export AdiabaticTandLWC
#' @import nleqslv
#' @param .Pbase Starting-point pressure, hPa
#' @param .Tbase Starting-point temperature, degC
#' @param .Pobs Pressure at the observation level, hPa (can be length>1)
#' @param .Ebase Optional vapor pressure at starting point; the default
#' is zero and in that case the calculation uses the equilibrium vapor 
#' pressure at .Tbase
#' @param .lwc Liquid water content (g/m^3) at starting point (default 0)
#' @return A data.frame containing two columns, Tobs and ALWC, which are the
#' (possibly length>1) values of temperature [degC] and liquid water content
#' [g/m^3] corresponding to the pressures .pObs .
#' @examples 
#' AdiabaticTandLWC (900, 20, c(900,800,700,600,500,400))
#' \dontrun{plot(AdiabaticTandLWC (900.,20.,seq(900,200,by=-10))$ALWC, seq(900,200,by=-10), 
#'     xlab='LWC [g/m^3]', ylab = 'Pressure [hPa]', ylim=c(1000,100), type='l', lwd=2, col='blue')}

AdiabaticTandLWC <- function (.Pbase, .Tbase, .Pobs, .Ebase=0, .lwc=0) {
  ThetaQ <- WetEquivalentPotentialTemperature (.Pbase, .Tbase, .Ebase, .lwc)
  TZERO <- 273.15; CP <- SpecificHeats () # must use dry-air values
  eBase <- ifelse (.Ebase <= 0, MurphyKoop (.Tbase, .Pbase), .Ebase) 
  rTot <- MixingRatio (eBase / .Pbase) + (.lwc / 1000) /
    (100 * (.Pbase - eBase) / (CP[3] * (.Tbase+TZERO))) #this is rho_dry_air; 100 Pa / mb
  cpt <- CP[1] + rTot * 4.190e3     # latter is specific heat of liq water
  
  ###### function for nleqslv, solving a non-linear equation
  ##     (here, finding the temperature that gives the same ThetaQ at a new level)
  fcn2 <- function (T1, .Pob, rtot, ThetaQ, CP) {
    e <- MurphyKoop (T1, .Pob)
    rv <- MixingRatio (e/.Pob)
    if (rtot < rv) {
      e <- rtot * .Pob / (rtot + (StandardConstant ("MWW") / StandardConstant ("MWD")))
      lwc <- 0
    } else {
      lwc <- 1000 * (rtot - rv) * 100. * (.Pob - e) / (CP[3] * (T1 + TZERO))
    }
    return (WetEquivalentPotentialTemperature (.Pob, T1, e, lwc) - ThetaQ)
  }
  ###### end of function for nleqslv
  
  Tobs <- alwc <- .Pobs  # this reserves vectors of appropriate length; to be overwritten
  for (i in 1:length (.Pobs)) { # have to loop: nleqslv won't handle a vector of length > 1
    Tobs[i] <- nleqslv::nleqslv (.Tbase, fcn2, jac=NULL, .Pobs[i], rTot, ThetaQ, CP)$x
  }
  eObs <- MurphyKoop (Tobs, .Pobs)
  rObs <- MixingRatio (eObs / .Pobs)
  alwc <- 1000. * (rTot - rObs) * 
    (100 * (.Pobs - eObs) / (CP[3] * (Tobs + TZERO)))
  alwc [rObs >= rTot] <- 0
  return (data.frame (Tobs, "ALWC"=alwc))
}



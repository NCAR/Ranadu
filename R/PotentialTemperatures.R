TZERO <- StandardConstant("Tzero")
#' @title PotentialTemperature
#' @description Function to calculate the potential temperature
#' @details Potential temperature is calculated from the pressure and temperature, optionally with a humidity correction
#' @aliases PotentialTemperature
#' @author William Cooper
#' @export PotentialTemperature
#' @param P A numeric representing the pressure in hPa
#' @param AT A numeric representing the air temperature in deg. C
#' @param E A numeric representing the water vapor pressure in hPa
#' @return Numeric containing the potential temperature
#' @examples 
#' THETAP <- PotentialTemperature (500.,0.)
PotentialTemperature <- function (P, AT, E=0.) {
# Potential Temperature, conventionally for dry air which
# results if E argument omitted. P and E in hPa; AT degC.
# If E given, the adiabatic calculation to 1000 hPa is done
# using the thermodynamic properties of moist air.
  CP <- SpecificHeats (E/P)
  Theta <- (AT + TZERO) * (1000./P)**(CP[,3]/CP[,1])
  return (Theta)
}

#' @title EquivalentPotentialTemperature
#' @description The pseudo-adiabatic equivalent potential temperature
#' @details The pseudo-adiabatic equivalent potential temperature is calculated according to the formula of Davies-Jones (2009); see ProcessingAlgorithms.pdf
#' @aliases EquivalentPotentialTemperature
#' @author William Cooper
#' @export EquivalentPotentialTemperature
#' @param P A numeric representing ambient pressure in hPa 
#' @param AT A numeric representing air temperature in deg. C 
#' @param E A numeric representing water vapor pressure in hPa. Defaults to zero,
#' and for that case the calculation uses the equilibrium vapor pressure at AT.
#' @return A numeric representing the pseudo-adiabatic equivalent potential temperature in kelvin.
#' @examples 
#' THETAP <- EquivalentPotentialTemperature (700., 10., 9.) 
EquivalentPotentialTemperature <- function (P, AT, E=0) {
# Davies-Jones pseudo-adiabatic equivalent potential 
# temperature. Needs P, AT, E (hPa, degC, hPa).
  L0 <- 2.56313e6
  L1 <- 1754.
  K2 <- 1.137e6
  TK <- AT + TZERO
  if (E <= 0) {E <- MurphyKoop (AT)}
  r <- MixingRatio (E/P) 
  CP <- SpecificHeats(0.)     # need dry-air value, don't need vector
  TL = 2840./(3.5*log(TK)-log(E)-4.805)+55.
  TDL <- TK*(1000./(P-E))**0.2854*(TK/TL)**(0.28e-3*r)
  THETAP <- TDL * exp (r*(L0-L1*(TL-TZERO)+K2*r)/(CP[1]*TL))
  return (THETAP)
}

#' @title VirtualTemperature
#' @description Calculates the virtual temperature in deg. C
#' @details The virtual temperature is calculated from the air temperature and mixing ratio of water vapor
#' @aliases VirtualTemperature 
#' @author William Cooper
#' @export VirtualTemperature
#' @param AT A numeric representing the air temperature in deg. C
#' @param r A numeric representing the water vapor mixing ratio in dimensionless units (*NOT* g/kg) 
#' @return A numeric representing the virtual temperature in deg. C 
#' @examples 
#' Tvir <- VirtualTemperature (20., 0.005) 
VirtualTemperature <- function (AT, r) {
# Virtual Temperature, fn of AT (degC), r (mixing ratio
# in dimensionless units, kg/kg)
  Tvir <- (AT+TZERO)*((1.+(StandardConstant("MWD")/StandardConstant("MWW")*r)/(1.+r)))-TZERO
  return (Tvir)
}

#' @title VirtualPotentialTemperature
#' @description Calculates the virtual potential temperature in kelvin
#' @details The potential virtual temperature is calculated from the air temperature, pressure, and mixing ratio of water vapor
#' @aliases VirtualPotentialTemperature 
#' @author William Cooper
#' @export VirtualPotentialTemperature
#' @param Tvir A numeric representing the virtual temperature in deg. C
#' @param P A numeric representing the pressure in hPa
#' @param E A numeric representing the water vapor pressure in hPa. This is not used to calculate the virtual temperature but is used for the adiabatic calculation to 1000 hPa total pressure
#' @return A numeric representing the virtual potential temperature in kelvin
#' @examples 
#' THETAV <- VirtualPotentialTemperature (10., 850.)
#' THETAV <- VirtualPotentialTemperature (20., 900., 12.)
VirtualPotentialTemperature <- function (Tvir, P, E=0.) {
# Virtual Potential Temperature (K), fn of virtual temperature
# and pressure (hPa). Conventional definition uses dry-air
# constants and this is realized for E=0 or omitted, but the
# routine will also give an appropriate result for moist air
# also if E is supplied and is different from 0. 
  CP <- SpecificHeats (E/P)
  THETAV <- (Tvir+TZERO)*(1000./P)**(CP[,3]/CP[,1])
}

#' @title WetEquivalentPotentialTemperature
#' @description The wet-equivalent potential temperature (THETAQ)
#' @details Wet-equivalent potential temperature is calculated from the pressure, air temperature, water vapor pressure, and liquid water content; see Paluch 1978.
#' @aliases WetEquivalentPotentialTemperature
#' @author William Cooper
#' @export WetEquivalentPotentialTemperature
#' @param P A numeric representing pressure in hPa 
#' @param AT A numeric representing air temperature in deg. C 
#' @param E A numeric representing water vapor pressure in hPa. Default is 0, and if
#' E <= 0 then E is set to the equilibrium value for the calculation.
#' @param w A numeric representing liquid water content in g/m^3. Default is 0.
#' @return A numeric representing the wet-equivalent potential temperature in kelvin 
#' @examples 
#' THETAQ <- WetEquivalentPotentialTemperature (700., 0., 6.11, 1.0)
WetEquivalentPotentialTemperature <- function (P, AT, E=0, w=0) {
# wet-equivalent potential temperature (THETAQ) as a function
# of pressure (P, hPa), temperature (AT, degC), mixing
# ratio (r, dimensionless), and condensed water content (w) in
# g/m^3
  Tk <- AT + TZERO
  eeq <- MurphyKoop (AT, P)
  if (E <= 0) {
    E <- eeq
  }
  Lv <- 2.501e6 - 2370.*AT    # latent heat of vaporization, temp-dependent
  cw <- 4190.                  # J/(kg K), mean value 0-90C
  CP <- SpecificHeats ()     # need dry-air values
  r <- MixingRatio (E/P)
  rt <- r + (w/1000.) / (100 * (P-E) / (CP[3] * Tk)) # denom. is density of dry air
  cpt <- CP[1]+rt*cw
  F1 <- ifelse ((E < eeq), (E/eeq) ^ (-r*StandardConstant("Rw")/cpt), 1.)
  return (F1 * Tk * (1000 / (P - E)) ^ (CP[3] / cpt) * exp((Lv * r) / (cpt * Tk)))
}


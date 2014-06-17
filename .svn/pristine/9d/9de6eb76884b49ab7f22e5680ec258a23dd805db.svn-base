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
  Ra <- 287.083/(1.+(0.6220-1.)*EoverP)
  cp <- 1004.79*Ra/287.083*(1+0.8373*EoverP)
  cv <- 717.66*Ra/287.083*(1.+0.92926*(EoverP))
  L <- length(EoverP)
  R <- c(cp,cv,Ra)
  dim(R) <- c(L,3)
  return (R)
}

#' @title MachNumber
#' @description Calculates the Mach number 
#' @details returns the Mach number calculated from ambient and dynamic pressure and, optionally, corrected for water vapor pressure
#' @aliases MachNumber machnumber
#' @author William Cooper
#' @export MachNumber
#' @param P A numeric (optionally vector) giving the ambient pressure in hPa 
#' @param Q A numeric (optionally vector) giving the dynamic pressure in hPa 
#' @param E A numeric (optionally vector) giving the water vapor pressure in hPa 
#' @return A numeric (possible vector) giving the MACH Number
#' @examples 
#' MACH <- MachNumber(500., 50.)
#' MACH <- MachNumber(700., 60., 1.1)
MachNumber <- function (P, Q, E=0.) {
# function to calculate the Mach number, optionally with
# humidity correction. Call with pressure, dynamic pressure,
# and vapor pressure, all in the same units.
# Any units can be used as long as it is the same for all
# three arguments.
  CP <- SpecificHeats (E/P)
  MACH <- ((2.*(CP[,1]-CP[,3])/CP[,3])*(((P+Q)/P)**(CP[,3]/CP[,1])-1.))**0.5
  return (MACH)
}

#' @title RecoveryFactor
#' @description Mach-number-dependent recovery factor for heat sensor
#' @details For specified Mach number, returns the recovery factor according to the formula in ProcessingAlgorithms.
#' @aliases RecoveryFactor recoveryfactor
#' @author William Cooper
#' @export RecoveryFactor
#' @param MACH A numeric giving the Mach number, perhaps calculated by MachNumber()
#' @return The recovery factor applicable to a heated sensor
#' @examples rf <- RecoveryFactor (0.75)
RecoveryFactor <- function (MACH) {
# recovery factor for heated probes:
  rf2 <- 0.988+0.053*log10(MACH)+0.090*log10(MACH)**2
       +0.091*log10(MACH)**3
  return (rf2)
}

TZERO = 273.15
#' @title MurphyKoop
#' @description Returns the water vapor pressure
#' @details Calculates the equilibrium water vapor pressure according to the Murphy-Koop equation
#' @aliases MurphyKoop murphykoop
#' @author William Cooper
#' @export MurphyKoop
#' @param DP A numeric representing the dew point in deg. C
#' @param P An optional numeric representing total pressure in hPa, set zero to suppress 'enhancement factor' correction for total pressure
#' @return Water vapor pressure in equilibrium with a plane water surface at dew point DP
#' @examples 
#' e <- MurphyKoop (-12.)
#' e <- MurphyKoop (10., 800.)
MurphyKoop <- function (DP, P=0) {
# returns vapor pressure via Murphy-Koop equations.
# Supply DP=dewpoint (deg C) and optionally P=pressure (hPa),
# the latter for the enhancement-factor correction.
  b0 <- 9.550426
  b1 <- -5723.265
  b2 <- 3.53068
  b3 <- -0.00728332
  b7 <- 54.842763
  b8 <- -6763.22
  b9 <- -4.210
  b10 <- 0.000367
  b11 <- 0.0415
  TR2 <- 218.8
  b12 <- 53.878
  b13 <- -1331.22
  b14 <- -9.44523
  b15 <- 0.014025
  tk <- DP + TZERO
  ess <- exp(b7+b8/tk+b9*log(tk)+b10*tk+tanh(b11*(tk-TR2))
             *(b12+b13/tk+b14*log(tk)+b15*tk))
  ess <- ess/100.
  f=1.e-5 * P*(4.923-0.0325*tk+5.84e-5 *tk**2)+1.
  ess <- f * ess
  return (ess)
}

#' @title MurphyKoopIce
#' @description Returns the water vapor pressure
#' @details Calculates the water vapor pressure in equilibrium over a plane ice surface according to the Murphy-Koop equation
#' @aliases MurphyKoopIce murphykoopice
#' @author William Cooper
#' @export MurphyKoopIce
#' @param FP A numeric representing the frost point in deg. C
#' @param P An optional numeric representing total pressure in hPa, set zero to suppress 'enhancement factor' correction for total pressure
#' @return Water vapor pressure in equilibrium with a plane ice surface at dew point DP
#' @examples 
#' e <- MurphyKoopIce (-50.)
#' e <- MurphyKoopIce (-60., 200.)
MurphyKoopIce <- function (FP, P=0) {
  b0 <- 9.550426
  b1 <- -5723.265
  b2 <- 3.53068
  b3 <- -0.00728332
  b7 <- 54.842763
  b8 <- -6763.22
  b9 <- -4.210
  b10 <- 0.000367
  b11 <- 0.0415
  TR2 <- 218.8
  b12 <- 53.878
  b13 <- -1331.22
  b14 <- -9.44523
  b15 <- 0.014025
  tk <- FP + TZERO
  ess=6.111536*exp(b1*(TZERO-tk)/(TZERO*tk)
                   +b2*log(tk/TZERO)+b3*(tk-TZERO))
  f=1.e-5 * P*(4.923-0.0325*tk+5.84e-5 *tk**2)+1.
  ess <- f * ess
  return (ess)
}

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
  if (P > 226.3206) {
    PALT = (288.15/0.0065)*(1-(P/1013.25)**0.1902632)
  } else {
    PALT = 11000.+(8314.32*216.65)/(9.80665*28.9644)*log(226.3206/P)
  }
  return (PALT)
}

#' @title AirTemperature
#' @description Calculates the ambient air temperature
#' @details Finds the air temperature from measurements of recovery temperature and ambient and dynamic pressure, with optional correction for humidity
#' @aliases AirTemperature
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
  Ra <- CP[,3]  #287.083/(1.+(0.6220-1.)*E/P)
  cv <- CP[,2]  #717.66*(1.+0.92926*(E/P))/((1.+(0.6220-1.)*E/P))
  MACH <- MachNumber (P, Q, E)
  AT <- (RT+TZERO)/
    (1.+RecoveryFactor(MACH)*MACH**2 *Ra/(2.*cv))-TZERO
  return (AT)
}

#' @title TrueAirspeed
#' @description Returns the true airspeed 
#' @details Calculates the true airspeed from measurements of Mach number, air temperature, and optionally humidity
#' @aliases TrueAirspeed trueairspeed
#' @author William Cooper
#' @export TrueAirspeed
#' @param MACH A numeric representing the Mach number, possible from MachNumber()
#' @param AT A numeric representing the air temperature in deg.C
#' @param EoverP An optional numeric representing the ratio of water vapor pressure to total pressure
#' @return The true airspeed in m/s
#' @examples 
#' TAS <- TrueAirspeed (0.3, 10.)
#' TAS <- TrueAirspeed (0.5, 0., 6.11/800.)
TrueAirspeed <- function (MACH, AT, EoverP=0.) {
# TAS from Mach Number and temperature:
  CP <- SpecificHeats (EoverP)
  gamma <- CP[,1]/CP[,2]
  TAS <- MACH * (gamma*CP[,3]*(AT+TZERO))**0.5
  return (TAS)
}

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

#' @title EquivalentPotentialTemperature
#' @description The pseudo-adiabatic equivalent potential temperature
#' @details The pseudo-adiabatic equivalent potential temperature is calculated according to the formula of Davies-Jones (2009); see ProcessingAlgorithms.pdf
#' @aliases EquivalentPotentialTemperature
#' @author William Cooper
#' @export EquivalentPotentialTemperature
#' @param P A numeric representing ambient pressure in hPa 
#' @param AT A numeric representing air temperature in deg. C 
#' @param E A numeric representing water vapor pressure in hPa
#' @return A numeric representing the pseudo-adiabatic equivalent potential temperature
#' @examples 
#' THETAP <- EquivalentPotentialTemperature (700., 10., 9.) 
EquivalentPotentialTemperature <- function (P, AT, E) {
# Davies-Jones pseudo-adiabatic equivalent potential 
# temperature. Needs P, AT, E (hPa, degC, hPa).
  L0 <- 2.56313e6
  L1 <- 1754.
  K2 <- 1.137e6
  TK <- AT + TZERO
  r <- MixingRatio (E/P) 
  CP <- SpecificHeats(0.)     # need dry-air value, don't need vector
  TL = 2840./(3.5*log(TK)-log(E)-4.805)+55.
  TDL <- TK*(1000./(P-E))**0.2854*(TK/TL)**(0.28e-3*r)
  THETAP <- TDL * exp (r*(L0-L1*(TL-TZERO)+K2*r)/(CP[1]*TL))
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
  Tvir <- (AT+TZERO)*((1.+(29.9637/18.0153)*r)/(1.+r))-TZERO
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
#' @param E A numeric representing water vapor pressure in hPa
#' @param w A numeric representing liquid water content in g/m^3
#' @return A numeric representing the wet-equivalent potential temperature in kelvin 
#' @examples 
#' THETAQ <- WetEquivalentPotentialTemperature (700., 0., 6.11, 1.0)
WetEquivalentPotentialTemperature <- function (P, AT, E, w) {
# wet-equivalent potential temperature (THETAQ) as a function
# of pressure (P, hPa), temperature (AT, degC), mixing
# ratio (r, dimensionless), and condensed water content (w) in
# g/m^3
  Tk <- AT + TZERO
  Lv <- 2.501e6-2370.*AT
  cw <- 4190.                  # J/(kg K), mean value 0-90C
  CP <- SpecificHeats (0.)     # need dry-air values
  r <- MixingRatio (E/P)
  rho_air <- (P-E)/(CP[3]*(AT+TZERO))
  rt <- r + (w/1000.)/rho_air
  cpt <- CP[1]+rt*cw
  Rw = 461.5228
  eeq <- MurphyKoop (AT)
  F1 <- ifelse ((E < 0.9*eeq) & (w < 0.00001), (E/eeq)**(r*Rw/cpt), 1.)
  T1 <- Tk * (1000./(P-E))**(CP[3]/CP[1])
  THETAQ <- T1*F1*exp((Lv*r)/(cpt*Tk))
  return (THETAQ)            
}

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

#' @title plotWAC
#' @description Convenience routine for plots
#' @details Sets some plot defaults and calls plot and axis
#' @aliases plotWAC
#' @author William Cooper
#' @export plotWAC
#' @param x Usually, Time from a data.frame; a vector of abscissa values for points to plot
#' @param y A vector of ordinate values for points to plot. 
#' @param col Color to pass to plot (default: blue)
#' @param xlab Label for the abscissa, to pass to plot (default: "TIME [UTC]")
#' @param ylab Label for the ordinate, to pass to plot (default: "")
#' @param lwd Line width to pass to plot (default: 2)
#' @param type Line type to pass to plot (default: "l")
#' @examples 
#' \dontrun{plotWAC (Time, TASX)}
plotWAC <- function (x, y, col='blue', xlab="TIME [UTC]", ylab="", lwd=2, type='l') {
  plot(x, y, xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, 
     xaxs="r", yaxs="r", type=type, col=col, lwd=lwd)
  axis.POSIXct(1,x, format='%H:%M', tck=0.02)
  axis.POSIXct(3,x, labels=NA, tck=0.02)
  axis(2,tck=0.02)
  axis(4,labels=NA,tck=0.02)
  return ()
}

# ############### Main Routine Starts Here ##############
# # specify variables needed from netCDF file, and the file:
# # (don't include Time; this is treated differently)
# VarList <- c("CAVP_DPL", "CAVP_DPR", "TASX", "VSPD",
#              "PSXC", "QCXC", "ATTACK", "SSLIP")
# F <- 6
# fname = paste ("/home/Data/CONTRAST/CONTRASTrf", 
#                formatC(F,digits=0,width=2, format='d', 
#                        flag='0'), '.nc', sep='')
# fname = paste ("/run/media/cooperw/left_wing/HIPPO5/HIPPO-5rf",
#                formatC(F,digits=0,width=2, format='d', 
#                        flag='0'), '.nc', sep='')
# 
# 
# Data <- getNetCDF (fname, VarList, 191500, 224500)  # load variables to data.frame
# attach(Data)
# SE <- getStartEnd(Time)    #StartTime is SE[1]; EndTime SE[2]
# 
# # specify the index range corresponding to a specified time range
# T1 = 202005
# T2 = 221503
# T1 <- SE[1]
# T2 <- SE[2]
# r <- getIndex(Time,T1):getIndex(Time,T2)
# plot(Time[r], TASX[r], xaxt='n', yaxt='n', xlab="TIME [UTC}", 
#      ylab="TASX", xaxs="r", yaxs="r", type="l", lwd=2)
# axis.POSIXct(1,Time[r], format='%H:%M', tck=0.02)
# axis.POSIXct(3,Time[r], labels=NA, tck=0.02)
# axis(2,tck=0.02)
# #axis(3,labels=NA,tck=0.02)
# axis(4,labels=NA,tck=0.02)
# plotWAC(Time[r], TASX[r], ylab="TASX",type='l',lwd=3,col="orange")
# detach(Data)
 
# require(devtools)
# require(roxygen2)
# wd("~/RStudio/RANADU)
# document ()
# check ()
# build ()
# install ()

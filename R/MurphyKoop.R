TZERO = StandardConstant("Tzero")
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


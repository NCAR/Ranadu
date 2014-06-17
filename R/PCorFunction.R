#' @title Static Defect Correction
#' @description Corrections to dynamic pressure and ambient pressure
#' @details This correction was determined from fits to the measurements from
#' the Laser Air Motion Sensor, as described by Cooper et al., 2014.
#' @aliases PCorFunction
#' @author William Cooper
#' @export PCorFunction
#' @param p Measured (uncorrected) ambient pressure (e.g., PSF), hPa (no default)
#' @param q Measured (uncorrected) dynamic pressure (e.g., QCF), hPa (or any units that match those used for p) -- default 100 hPa. This is unused for the C-130 so it does not need to be supplied.
#' @param alpha Angle of attack (degrees; default +3)
#' @param Mach Mach number (default: 0.8)
#' @param AC Aircraft identifier, either "GV" of "C130" (default "GV")
#' @return The ratio of the pressure correction to the measured pressure. The correction to dynamic pressure is the negative of the returned value.
#' @examples 
#' \dontrun{dPoverP <- PCorFunction (p, q, alpha, Mach, "GV")}
PCorFunction <- function (p, q=100., alpha=3., Mach=0.8, AC="GV") {

  if (AC == "C130") { # this is for PSFD; PSFRD is different
    b0 <- -0.2237  # this doesn't look right, altho matched Alg. Doc.
    b1 <- 0.00110
    b2 <- 0.01349
    dp <- b0 + b1*alpha + b2*Mach
  } else {
    a0 <- -0.01317
    a1 <- 0.07256
    a2 <- -0.08611
    a3 <- 0.002253
    dp = a0 + a1*q/p +a2*Mach**3 +a3*alpha
  } 
  return (dp)
}
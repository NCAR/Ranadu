#' @title Static Defect Correction
#' @description Corrections to dynamic pressure and ambient pressure
#' @details This correction was determined from fits to the measurements from
#' the Laser Air Motion Sensor, as described by Cooper et al., 2014.
#' @aliases PCorFunction
#' @author William Cooper
#' @export PCorFunction
#' @param p Measured (uncorrected) ambient pressure (e.g., PSF), hPa (no default)
#' @param q Measured (uncorrected) dynamic pressure (e.g., QCF), hPa (or any units that match those used for p) -- default 100 hPa. This is unused for the C-130 so it does not need to be supplied.
#' @param akrd Ther angle of attack (degrees; no default)
#' @param AC Aircraft identifier, either "GV" of "C130" (default "GV")
#' @param Sensors Normally "STD" (the default) but "R" for right pressure pair, C130 
#' @return The ratio of the pressure correction to the measured pressure. The correction to dynamic pressure is the negative of the returned value.
#' @examples 
#' \dontrun{dPoverP <- PCorFunction (p, q, akrd, "GV")}
PCorFunction <- function (p, q=100., akrd=2.5, AC="GV", Sensors = "STD") {
  Mach <- MachNumber(p, q)   # calculated from raw measurements
  if ((AC == "C130") | (AC == "C-130")) { # this is for PSFD; PSFRD is different
    if (Sensors == "R") {
      b0 = -0.00754
      b1 <- 0.000497
      b2 <- 0.0368
    } else {
      b0 <- -0.00637  
      b1 <- 0.001366
      b2 <- 0.0149
    }  
    dp <- b0 + b1*akrd + b2*Mach
  } else {             # GV:
    #bp <- c(4.604, 18.67, 6.49)             # old AKRD coefficients for PREDICT
    #b <-  c(4.34685, 20.10448, 1.36492)     # AKRD coefficients for CONTRAST
    a <- c(-0.012255, 0.075372, -0.087508, 0.002148)       # PCOR coefficients
    dp <- a[1] + a[2] * (q/p) + a[3] * Mach**3 + a[4] * akrd
  } 
  return (dp)
}

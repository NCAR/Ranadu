#' @title Static Defect Correction
#' @description Corrections to dynamic pressure and ambient pressure
#' @details This correction was determined from fits to the measurements from
#' the Laser Air Motion Sensor, as described by Cooper et al., 2014. These do not
#' apply to older projects; in particular, TREX and before (GV). 
#' @aliases PCORfunction
#' @author William Cooper
#' @export PCorFunction
#' @param p Measured (uncorrected) ambient pressure (e.g., PSF), hPa (no default)
#' @param q Measured (uncorrected) dynamic pressure (e.g., QCF), hPa (or any units 
#' that match those used for p) -- default 100 hPa. 
#' @param akrd The angle of attack (degrees; 2.5 deg. default)
#' @param AC Aircraft identifier, either "GV" of "C130" (default "GV")
#' @param Sensors Normally "STD" (the default) but "R" for right pressure pair, C130 
#' @return The ratio of the pressure correction to the measured pressure. The 
#' correction to dynamic pressure is the negative of the returned value.
#' @examples # using PSXC and QCXC because PSF and QCF are not in RAFdata
#' PCOR <- RAFdata$PSXC * PCorFunction (RAFdata$PSXC, RAFdata$QCXC, RAFdata$ATTACK)
#' \dontrun{dPoverP <- PCorFunction (p, q, akrd, "GV")}
PCorFunction <- function (p, q=100., akrd=2.5, AC="GV", Sensors = "STD") {
  Mach <- MachNumber(p, q)   # calculated from raw measurements
  if ((AC == "C130") | (AC == "C-130")) { 
    if (Sensors == "R") {  ## for PSFRD
      # b0 = -0.00754
      # b1 <- 0.000497
      # b2 <- 0.0368
      b0 <- 0.007372
      b1 <- 0.12774
      b2 <- -6.8776e-4
      b3 <- -0.02994
      b4 <- 0.001630
    } else {               ## for PSFD
      # b0 <- -0.00637  
      # b1 <- 0.001366
      # b2 <- 0.0149
      b0 <- -4.389e-3
      b1 <- -2.966e-2
      b2 <- 6.831e-5
      b3 <- 2.672e-2
      b4<- 2.4466e-3
    }  
    # dp <- b0 + b1*akrd + b2*Mach
    dp <- b0 + (q/p) * (b1 + b4 * akrd^2) + b2 * akrd + b3 * Mach
  } else {             # GV:
    #bp <- c(4.604, 18.67, 6.49)             # old AKRD coefficients for PREDICT
    #b <-  c(4.34685, 20.10448, 1.36492)     # AKRD coefficients for CONTRAST
    a <- c(-0.012255, 0.075372, -0.087508, 0.002148)       # PCOR coefficients
    dp <- a[1] + a[2] * (q/p) + a[3] * Mach**3 + a[4] * akrd
  } 
  return (as.vector(dp))
}

#' @title RecoveryFactor
#' @description Mach-number-dependent recovery factor for a temperature sensor
#' @details For specified Mach number, returns the recovery factor according to the formula in ProcessingAlgorithms.
#' @aliases RecoveryFactor recoveryfactor
#' @author William Cooper
#' @export RecoveryFactor
#' @param MACH A numeric giving the Mach number, perhaps calculated by MachNumber()
#' @param probe A string giving a probe name. Default is 'HARCO'; 
#' other options are 'Rose" (heated 102 Rosemount) and 'UNHEATED' (Rosemount 102AL unheated)
#' @return The recovery factor applicable to a temperature sensor
#' @examples rf <- RecoveryFactor (0.75)
RecoveryFactor <- function (MACH, probe='HARCO') {
# recovery factor for heated probes:
  if (probe == 'ROSE') {
    rf2 <- 0.9816655 + 0.1605396*log10(MACH)+0.2603177*log10(MACH)^2
    +0.2512514*log10(MACH)^3 
  } else if (probe == "UNHEATED") {
    rf2 <- 0.9959+0.0283*log10(MACH)+0.0374*log10(MACH)**2
    +0.0762*log10(MACH)**3
  } else {  ## defaults to HARCO
    rf2 <- 0.988+0.053*log10(MACH)+0.090*log10(MACH)**2
       +0.091*log10(MACH)**3
  }
  return (rf2)
}


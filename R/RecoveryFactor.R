#' @title RecoveryFactor
#' @description Mach-number-dependent recovery factor for a temperature sensor
#' @details For specified Mach number, returns the recovery factor according to the formula in ProcessingAlgorithms.
#' @aliases RecoveryFactor recoveryfactor
#' @author William Cooper
#' @export RecoveryFactor
#' @param MACH A numeric giving the Mach number, perhaps calculated by MachNumber()
#' @param probe A string giving a probe name. Default is 'HEATED' and anything else is
#' treated as unheated.
#' @return The recovery factor applicable to a temperature sensor
#' @examples rf <- RecoveryFactor (0.75)
RecoveryFactor <- function (MACH, probe='HEATED') {
# recovery factor for heated probes:
  rf2 <- 0.988+0.053*log10(MACH)+0.090*log10(MACH)**2
       +0.091*log10(MACH)**3
  if (probe != "HEATED") {
    rf2 <- 0.9959+0.0283*log10(MACH)+0.0374*log10(MACH)**2
          +0.0762*log10(MACH)**3
  }
  return (rf2)
}


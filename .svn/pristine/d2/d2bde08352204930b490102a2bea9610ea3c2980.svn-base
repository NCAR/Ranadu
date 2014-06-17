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


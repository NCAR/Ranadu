#' @title RecoveryFactor
#' @description Mach-number-dependent recovery factor for a temperature sensor
#' @details For specified Mach number, returns the recovery factor according to the formula in ProcessingAlgorithms.
#' @aliases RecoveryFactor recoveryfactor
#' @author William Cooper
#' @export RecoveryFactor
#' @param .Mach A numeric giving the Mach number, perhaps calculated by MachNumber()
#' @param probe A string giving a probe name. Default is 'HARCO'; other options are
#' 'HARCOB' for B-element of HARCO, 'Rose" (heated 102 Rosemount) and 'UNHEATED' (Rosemount 102AL unheated)
#' @return The recovery factor applicable to a temperature sensor
#' @examples 
#' rf <- RecoveryFactor (0.75)
#' rf2 <- RecoveryFactor (MachNumber (RAFdata$PSXC, RAFdata$QCXC))
RecoveryFactor <- function (.Mach, probe='HARCO') {
# recovery factor for temperature probes:
  LM <- log10(.Mach)
  if (probe == 'ROSE') {
    # this is the alternate-geometry Rosemount probe, not the one we have
    # rf2 <- 0.9816655 + 0.1605396*LM+0.2603177*LM^2+0.2512514*LM^3
    # the one we have is nominally the HARCO formula below, but this works better:
    rf2 <- 0.958    ## both elements
  } else if (probe == "UNHEATED") {
    rf2 <- 0.9959+0.0283*LM+0.0374*LM^2+0.0762*LM^3
  } else if (probe == 'HARCOB') {
    rf2 <- 0.988+0.053*LM+0.090*LM^2+0.091*LM^3 - 0.015
  } else {  ## defaults to HARCO
    rf2 <- 0.988+0.053*LM+0.090*LM^2+0.091*LM^3
  }
  return (rf2)
}


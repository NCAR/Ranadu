#' @title wetbulb Temperature
#' @description Calculates the wet-bulb temperature given the pressure, 
#' temperature, and dewpoint.
#' @details An interative solution is used, as described here:
#' https://rpubs.com/cooperwilliama/797335 
#' If the known quantities are pressure, temperature, and vapor pressure
#' you can use DPfromE() to find the dewpoint. The
#' input parameters may be vectors, all of the same size, and in that
#' case the result will also be a vector. Be aware, however, that large
#' vectors (like whole flights even at 1 Hz) are likely to give an error
#' resulting from the attempt by nleqslv to allocate too much memory. 
#' @aliases wetbulbT,wetBulbT,wetbulb
#' @author William Cooper
#' @export wetbulbT
#' @param P A numeric representing ambient pressure in hPa 
#' @param AT A numeric representing air temperature in deg. C 
#' @param DP A numeric representing dewpoint temperature in deg. C. 
#' @import nleqslv
#' @return A numeric representing the wet-bulb temperature in deg. C
#' @example expect 13.58
#' twb <- wetbulbT(800, 20, 10) 
wetbulbT <- function(P, AT, DPT) {
    twb <- function(tw) { # This function depends on P. AT and DPT 
        # (i.e., temperature [degC], pressure [hPa], dewpoint [degC]
        # being in the calling environment.
        e <- MurphyKoop(DPT) # omitting dependence on p, as is conventional
        e2 <- MurphyKoop(tw)
        r <- MixingRatio(e / P)
        rs <- MixingRatio(e2 / P)
        Lv <- 2.501e6 - 2370 * tw 
        cp <-SpecificHeats(e2 / P)[1]
        return(tw - AT - Lv * (r - rs) / cp) # for adjusting to zero return
    }
    ## Try to avoid errors for missing values:
    P <- zoo::na.approx (as.vector(P), na.rm=FALSE, rule = 2)
    AT <- zoo::na.approx (as.vector(AT), na.rm=FALSE, rule = 2)
    DPT <- zoo::na.approx (as.vector(DPT), na.rm=FALSE, rule = 2)
    T1 <- 0.5 * (AT + DPT)  # The first guess for nleqslv
    X <- nleqslv(T1, twb) # X$x is the solution for tw
    return(X$x)
}

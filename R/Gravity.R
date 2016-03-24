#' @title Gravity
#' @description Provides the acceleration caused by gravity.
#' @details The formula used incorporates the dependence on latitude and on altitude above the WGS-84 geoid.
#' @aliases gravity
#' @author William Cooper
#' @export Gravity
#' @param latitude The latitude (or vector of latitudes) in units of degrees N.
#' @param altitude The altitude in meters above the WGS-84 geoid. GPS measurements are 
#' usually appropriate to use when supplying this altitude.
#' @return The acceleration of gravidy in units of m/s^2
#' @examples 
#' G <- Gravity (RAFdata$GGLAT, RAFdata$GGALT)
Gravity <- function (latitude, altitude=0.) {
  sl2 <- sin (latitude * StandardConstant("Cradeg"))**2
  g <- 9.780327 * ((1.+0.001931851*sl2) / 
                     (1.-0.006694380*sl2))-3.086e-6*altitude
  ## Somigliana formula 1980 GRS:
  # g <- 9.7803267714 * (1 + sl2 * (5.2790414e-3 + 2.32718e-5 * sl2)) 
  #      - (3.0876910891e-6 + 4.3977311e-9 * sl2) * altitude + 7.211e-13 * altitude^2
  return(g)
}

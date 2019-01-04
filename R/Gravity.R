#' @title Gravity
#' @description Provides the acceleration caused by gravity.
#' @details The formula used incorporates the dependence on latitude and on altitude above the WGS-84 geoid.
#' @aliases gravity
#' @author William Cooper
#' @export Gravity
#' @param latitude The latitude (or vector of latitudes) in units of degrees N.
#' @param altitude The altitude in meters above the WGS-84 geoid. GPS measurements before
#' correction to MSL are appropriate to use when supplying this altitude. For detailed accuracy,
#' correct a variable like GGALT by adding the height of the geoid above the WGS84 reference
#' ellipsoid. In RAF datasets, this may be present as the variable GGEOIDHT.
#' @return The acceleration of gravity in units of m/s^2
#' @examples 
#' G <- Gravity (RAFdata$GGLAT, RAFdata$GGALT)
Gravity <- function (latitude, altitude=0.) {
  sl2 <- sin (latitude * pi/180)^2
  ## The following comments document the relationship of the coefficients used
  ## to the fundamental defining constants of WGS84 (f, a, omega, GM) and
  ## the Taylor-expansion coefficients for the altitude dependence:
  # f <- 1/298.257223563
  # a <- 6378137.0
  # b <- 6356752.3142
  # GM <- 3986004.418e8
  # omega <- 7292115e-11
  # m <- omega^2*a^2*b/GM
  # k1 <- 2*(1+f+m)/a
  # k2 <- 4*f/a
  # k3 <- 3/a^2
  k1 <- 3.1570428706e-07
  k2 <- 2.1026896504e-09
  k3 <- 7.3745167729e-14
  SF <- ((1.+0.001931851*sl2) / sqrt(1.-0.006694380*sl2))  ## Somigliana factor
  ge <- 9.7803267714                                       ## g at the equator
  g <- ge * SF * (1 - (k1 - k2*sl2) * altitude + k3 * altitude^2)
  return(g)
}

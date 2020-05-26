
#' @title GeoPotHeight
#' @description Provides the geopotential height.
#' @details The formula used incorporates the dependence of gravity on latitude and on 
#' altitude above the WGS-84 geoid, as represented by the Somigliana factor and a
#' Taylor expansion of the dependence on height. See the code for "Gravity.R" for
#' more details regarding the coefficients used.
#' @aliases GeoPotHeight
#' @author William Cooper
#' @export GeoPotHeight
#' @param .latitude The latitude (or vector of latitudes) in units of degrees N.
#' @param .H The altitude in meters above the geoid representing mean sea level, or a vector
#' containing such quantities. The GPS-provided variable "GGALT" is appropriate.  
#' @param .geoid The height of the geoid above the WGS84 reference ellipse. This usually
#' introduces a small correction to the results obtained with .geoid equal to zero. The
#' variable GGEOIDHT in RAF-supplied data files is appropriate to use here.
#' @return A variable or vector of variables representing geopotential height [m].
GeoPotHeight <- function (.latitude, .H=0, .geoid=0) {  ## H is height above the geoid, MSL
  sl2 <- sin (.latitude * pi/180)^2
  k1 <- 3.1570428706e-07
  k2 <- 2.1026896504e-09
  k3 <- 7.3745167729e-14
  SF <- ((1.+0.001931851*sl2) / sqrt(1.-0.006694380*sl2))  ## Somigliana factor
  ge <- 9.7803267714                                       ## g at the equator:
  gzero <- StandardConstant('g_standard')                  ## reference value for geopot hgt
  gp <- ge * SF * (.H - 0.5 * (k1 - k2*sl2) * (.H^2+2*.H*.geoid) + k3 / 3 * ((.H+.geoid)^3 - .geoid^3)) 
  return (gp/gzero)
}

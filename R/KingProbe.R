# constants used in the KingProbe function:
King.TASL <- 0.
King.ReL <- 0.
King.tauNu <- 100.      # time constant for updating zero value
# three regimes: [c] TAS < 150; o.w. [a] Re<7244 or [b] Re>=7244
King.afb <- c(0.135, 0.638)
King.afa <- c(1.868, 0.343)
King.afc <- c(0.133, 0.382)
King.L <- 0.021     # element length, m
King.d <- 1.805e-3   # wire diameter, m
King.Ts <- 130.      # sensor temperature, deg. C
King.cb <- c(0.03366503, 1.34236135, -0.33479451, 0.0351934)
King.Rd <- StandardConstant("Rd")
King.cw <- 4190.     # specific heat of water, J/(kg K), mean value 0-90C
King.ConcTest <- 1.  # conc of droplets below which to consider zero


#' @title KingProbe
#' @description Calculates liquid water content from King Probe
#' @details Calculates an estimate of the dry-probe power and
#' subtracts this from the total power, then converts the resulting
#' residual power to a measure of the liquid water content. If applied
#' to successive measurements, the routine maintains an estimate of
#' the dry-air relationship between Nusselt number and Reynolds number
#' and uses that to adjust the zero reading when out of cloud (as
#' indicated by a measured droplet concentration falling below some
#' threshold).
#' @aliases KingProbe
#' @author William Cooper
#' @export KingProbe
#' @param Power The power in watts measured by the King probe (Watts)
#' @param TAS The true airspeed (m/s)
#' @param p Pressure (hPa)
#' @param T Temperature (deg. C)
#' @param N Droplet concentration (e.g., from a CDP)
#' @return Liquid water content (g/m^3)
# @examples
# lwc <- KingProbe (25., 180., 700., 10.,0.) 
KingProbe <- function (Power, TAS, p, T, N) {
  x <- log10 (p)
  Tb <- 10.**(King.cb[1]+x*(King.cb[2]+x*(King.cb[3]+x*King.cb[4])))  # boiling temperature deg.C
  Tm <- 0.5 * (T+King.Ts)     # mean temperature for air properties
  # NOTE: sign of 2nd term was wrong, PLWCC memo; code section OK:
  lhv <- (2.501-0.00237*Tb)*1.e6     # latent heat of vaporization, J/kg
  cond <- (2.38+0.0071*Tm)*1.e-2       # therm. cond., J/(m s K)
  visc <- (1.718+0.0049*Tm)*1.e-5    # viscosity, kg/(m s)
  dens <- 100. * p / (King.Rd * (Tm+273.15))  # air density mks
  Re <- dens * TAS * King.d / visc
  Nup <- Power / (pi * King.L * cond * (King.Ts-T))
  Valid <- (N < King.ConcTest) & (TAS > 50.) & (!is.na(Power))
#     Nup[!Valid] <- NA
#     NuRe0 <- rep (NA, length(Power))
#     NuRe1 <- rep (NA, length(Power))
#     NuRe0[TAS >= 150.] <- King.afc[1]
#     NuRe1[TAS >= 150.] <- King.afc[2]
#     NuRe0[(TAS > 150.) & (Re < 7244)] <- King.afa[1]
#     NuRe1[(TAS > 150.) & (Re < 7244)] <- King.afa[2]
#     NuRe0[(TAS > 150.) & (Re >= 7244)] <- King.afb[1]
#     NuRe1[(TAS > 150.) & (Re >= 7244)] <- King.afb[2]
  w <- vector ("numeric", length(Power))
  af <- matrix (nrow=length(Power), ncol=2)
#
# this section adjusts the Nu-Re coefficients to maintain zero out-of-cloud
  xl <- King.afc
  for (i in 1:length(Power)) {
    if (is.na(Power[i]) | is.na(TAS[i]) | is.na(T[i]) | is.na(N[i])) {
      af[i,] <- c(NA,NA)
    } else {
      xf <- KingUpdate(N[i], TAS[i], King.afa, King.afb, King.afc, Re[i], Nup[i])
      if (!is.na(xf[1])) {
        af[i,] <- xf
      } else {
        af[i,] <- xl
      }
      xl <- af[i,]
      King.TASL <<- TAS[i]
      King.ReL <<- Re[i]
    }
  }
  
  Nu <- af[,1] * Re**af[,2]
  Pdry <- pi * Nu * King.L * cond * (King.Ts-T)
#   x <- Pdry
#   x[TAS < 50.] <- NA
#   xm <- mean(x, na.rm=TRUE)
#   x <- Pdry
#   x[is.na(x)] <- xm
#   fc <- butter(4,0.05,type="low")
#   Pdry <- filtfilt(fc,x)
#   Pdry <- sgolayfilt(x, 3, 301)
  #plotWAC (Data$Time, Pdry)
  w <- 1000. * (Power-Pdry) / (King.L * King.d * TAS * (lhv + King.cw * (Tb-T)))
  return (w)
#   for (i in 1:length(Power)) {
#     if (is.na(Power[i]) | is.na(TAS[i]) | is.na(T[i]) | is.na(N[i])) {
#       w[i] <- NA
#     } else {
#       af <- KingUpdate(N[i], TAS[i], King.afa, King.afb, King.afc, Re[i], Nup[i])
#       King.TASL <<- TAS[i]
#       King.ReL <<- Re[i]
#       Nu <- af[1] * Re[i]**af[2]
#       Pdry <- pi * Nu * King.L * cond[i] * (King.Ts-T[i])
#       w[i] <- 1000. * (Power[i]-Pdry) / (King.L * King.d * TAS[i] * (lhv[i] + King.cw * (Tb[i]-T[i])))
#     }
#   }
#   return(w)
}    

KingUpdate <- function (N, TAS, afa, afb, afc, Re, Nup) {
  # this function provides an update to the Nu-Re coefficients and
  # returns the values to use if an adjustment is needed
  if ((N > King.ConcTest) | (TAS < 50.)) {
    return(c(NA,NA))
  }
  if (TAS < 150.) {
    if (King.TASL >= 150.) {
      if (King.ReL < 7244.) {
        afc[1] <- afa[1] * Re**(afa[2]-afc[2])
      } else {
        afc[1] <- afb[1] * Re**(afb[2]-afc[2])
      }
    }
    assign("King.afc", c(afc[1] + (Nup / (Re**afc[2]) - afc[1]) / King.tauNu, afc[2]),1)
    return (c(afc[1], afc[2]))
  } else if (Re < 7244.) {
    if (King.TASL < 150.) {
      afa[1] <- afc[1] * Re**(afc[2]-afa[2])
    } else if (King.ReL >= 7244.) {
      afa[1] <- afb[1] * Re**(afb[2]-afa[2])
    }
    assign("King.afa", c(afa[1] + (Nup / (Re**afa[2]) - afa[1]) / King.tauNu, afa[2]),1)
    return (c(afa[1], afa[2]))
  } else {
    if (King.TASL < 150.) {
      afb[1] <- afc[1] * Re**(afc[2]-afb[2])
    } else if (King.ReL < 7244.) {
      afb[1] <- afa[1] * Re**(afa[2]-afb[2])
    }
    assign("King.afb", c(afb[1] + (Nup / (Re**afb[2]) - afb[1]) / King.tauNu, afb[2]),1)
    return (c(afb[1], afb[2]))
  }
}

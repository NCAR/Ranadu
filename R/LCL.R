## find T and p at the lifted condensation level, given potential temperature and mixing ratio
# library(nleqslv)
# library(Ranadu)
#' @title LCL
#' @description Find the pressure and temperature at the lifted condensation level
#' @details Solves iteratively for the pressure at which, for the given 
#' potential temperature and mixing ratio, the equilibrium vapor pressure at that
#' pressure level would match that required to give the specified mixing ratio.
#' Note for comparison the Bolton formula: (2840 / 3.5*log(T+273.15)-log(e)-4.805)+55 
#' @aliases LiftedCondensationLevel
#' @author William Cooper
#' @export LCL
#' @import nleqslv
#' @param p The pressure [hPa] of the air parcel
#' @param t The temperature [degC] of the air parcel
#' @param r The water vapor mixing ratio of the air parcel [dimensionless, NOT g/kg]
#' @return A two-element vector giving the pressure [hPa]and temperature [degC] at the LCL
#' @examples 
#' X <- LCL (850, 10, 0.08)  ## expect (825.62, 7.659)

LCL <- function (p, t, r) {  ## given p, t, r at starting level, find p, t at LCL
  EoverP <- r / (0.622 + r)
  e <- p * EoverP
  SH <- SpecificHeats(EoverP)
  RbyCp <- SH[,3] / SH[,1]
  theta <- PotentialTemperature (p, t, e)  ## note that this is for moist, not dry, air
  LCLp <- function (p, th, r, RbyCp) {
    e <- r*p / (r + 0.622)
    t <- th / ((1000/p))^RbyCp 
    return (MurphyKoop (t-273.15) - e)
  }
  ## use Bolton value as starting value:
  TBolton <- (2840 / (3.5*log(theta)-log(1000*EoverP)-4.805))+55
  PBolton <- 1000 * (TBolton / theta)^(1/RbyCp)
  ## must loop; nleqslv doesn't handle vectors
  lt <- length (theta)
  pLCL <- vector (length=lt)
  tLCL <- vector (length=lt)
  for (i in 1:lt) {
    L <- nleqslv::nleqslv (PBolton[i], LCLp, jac=NULL, theta[i], r[i], RbyCp[i])
    pLCL[i] <- L$x
    tLCL[i] <- theta[i] / ((1000/(pLCL[i]))^RbyCp[i]) - 273.15
  }
  RV <- c(pLCL, tLCL)
  dim(RV) <- c(lt,2)
  return (RV)
}

# t <- c(20,25,30, 291.929-273.15)
# e <- MurphyKoop (t) * 0.5
# e[4] <- 1000*0.01134316/(0.622+0.01134316)
# p <- c(850,800,750, 1000)
# EoverP <- e/p
# r <- MixingRatio (EoverP)
# X <- LCL (p, t, r)
# print (X)
# ## compare to Bolton:
# TBolton <- (2840 / (3.5*log(theta)-log(e*1000/p)-4.805))+55
# TBolton <- (2840 / (3.5*log(t+273.15)-log(e)-4.805))+55
# print (c(TBolton-273.15, TBolton))


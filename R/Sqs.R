#' @title Sqs
#' @description Calculate the quasi-steady supersaturation and characteristic relaxation time.
#' @details See the equations in Politovich and Cooper (1988) and Cooper (1989)
#' [http://journals.ametsoc.org/doi/pdf/10.1175/1520-0469%281988%29045%3C1651%3AVOTSIC%3E2.0.CO%3B2]
#' [http://journals.ametsoc.org/doi/pdf/10.1175/1520-0469%281989%29046%3C1301%3AEOVDGH%3E2.0.CO%3B2]
#' Given the updraft, droplet size distribution, temperature and pressure, calculates the quasi-steady
#' supersaturation and the associated time constant. Uses a condensation coefficient of 0.04 and
#' an accommodation coefficient of 1. 
#' @aliases Sqs
#' @author William Cooper
#' @export Sqs
#' @param W The updraft [m/s]
#' @param Nd A data.frame containing the droplet size distribution with column names "n" and "r" 
#' for number concentration and radius. Units can be either SI or /cm^3 and micrometers. 
#' @param T The temperature in degrees Celsius.
#' @param p The pressure is hPa.
#' @return A two-component vector containing the quasi-steady supersaturation (in fractional units,
#' not in %) and the time constant for relaxation to the quasi-steady supersaturation.
#' @examples 
#' Sqs (W=2., Nd=data.frame(r=2:20, n=c(rep(0,5), rep(50,10), rep(0,4))), T=4.3, p=771)

Sqs <- function (W=1, Nd, T=5, p=700) {
  Nd$a2 <- rep(0, nrow(Nd))
  Mw <- StandardConstant('MWW')
  Md <- StandardConstant('MWD')
  CP <- SpecificHeats ()
  Cp <- CP[1]
  Rd <- CP[3]
  Cv <- CP[2]
  Ru <- StandardConstant('Ru')
  Rw <- StandardConstant('Rw')
  rhow <- 1.e3      # kg/m^3
  es <- MurphyKoop (T)
  alpha <- 1
  beta <- 0.04
  lhv <- (2.501 - 0.00237 * T) * 1.e6     # latent heat of vaporization, J/kg
  Tk <- T + 273.15
  a1 <- (9.80/Tk) * (lhv / (Cp*Rw*Tk) - 1/Rd)
  rho <- 100*p / (Rd*Tk)
  conductivity = 2.40e-2 * (Tk / 273.15)^0.91
  diffusivity = 0.225 * 1.e-4 * (Tk / 273.15)^1.7 * 1013.25 / p
  aa <- conductivity * sqrt(2*pi*Ru*Md*Tk) / (alpha * p * 100 * (3*Ru))
  ab <- sqrt(2*pi/(Rw*Tk)) * diffusivity / beta
  ftest <- function (Ndd) {
    fa <- Ndd$r*1.e-6 / (Ndd$r*1.e-6 + aa)
    fb <- Ndd$r*1.e-6 / (Ndd$r*1.e-6 + ab)
    a2 <- (4*pi/rho) * ((p*Rw/(Rd*es)) + lhv^2/(Rw*Cp*Tk^2)) /
      (Rw*Tk/(fb*diffusivity*es*100) + lhv^2/(fa*conductivity*Rw*Tk^2))
    return (a2)
  }
  Nd$a2 <- plyr::adply (Nd, .margins=1, .fun=ftest, .expand=FALSE)$V1
  # for (i in 1:nrow(Nd)) {
  #   fa <- Nd$r[i]*1.e-6 / (Nd$r[i]*1.e-6 + aa)
  #   fb <- Nd$r[i]*1.e-6 / (Nd$r[i]*1.e-6 + ab)
  #   a2 <- (4*pi/rho) * ((p*Rw/(Rd*es)) + lhv^2/(Rw*Cp*Tk^2)) /
  #                       (Rw*Tk/(fb*diffusivity*es*100) + lhv^2/(fa*conductivity*Rw*Tk^2))
  #   Nd$a2[i] <- a2
  # }
  a2I <- sum(Nd$a2 * Nd$n * Nd$r)  # units: n cm^-3; r: um so canceling factors of e6
  tau <- 1 / a2I
  Sqs <- a1*W/a2I
  return (c(Sqs, tau))
}

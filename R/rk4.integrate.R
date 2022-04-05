#' @title rk4.integrate 
#' @description Runge-Kutta integration with Cash-Karp adjustment of the time step.
#' @details This is a somewhat specialized function that has not been tested very
#' much and is included for some special uses like that for sensible-heat flux. It
#' is suggested that the R function rmutil::runge.kutta be used instead unless
#' problems with stability and accuracy arise with that function. Here the 
#' fourth-order Runge-Kutta method is used to integrate the derivative
#' provided as the first argument. If the estimated uncertainty does not meet the
#' tolerance specified, the step-size is made shorter and the integration is repeated
#' over the initial interval with smaller steps. Unlike the standard Cash-Karp method,
#' no provision is provided for increasing the time step because it is assumed that
#' results are needed for each time interval in the time series. The function calls
#' another called rk4.step() repeatedly to perform the actual integration. That function 
#' is only available internally and can't be called separate from rk4.integrate().
#' @aliases rk4.integrate
#' @author William Cooper
#' @export rk4.integrate
#' @param dydt A function that provides the derivative to be integrated. This function
#' needs to be valid for any index including fractional indices that represent the "time"
#' variable. It should have arguments "y" and "t" and should return the value of the
#' derivative of "y" at time "t". As used for example in ReviseT.R, the function may
#' reference values in the calling environment like the time-response parameters or
#' the original time series as for the recovery temperature.
#' @param ystart An initial value assigned to the integrated result. Default is zero.
#' @param tv An array of indices for which the integral is evaluated. Typically
#' "1:nrow(D)" where "D" is the data frame containing the variables that determine
#' the derivative. There is no default.
#' @param tol  The tolerance in the estimated error for a time step. If the estimated
#' error exceeds this limit the time step is broken into smaller steps to span the original
#' time interval. Default is 0.005, an absolute value. 
#' @return A vector representing the integration of the derivative at each time interval 
#' specified by "tv". The entire integration is done following one call when "tv" is
#' a vector, but a single step can be specified also to do the integration sequentially.
rk4.integrate<- function (dydt, ystart = 0, tv, tol = 0.005) {
  ## Try the full step and accept if error estimate is < tol;
  ## otherwise calculate the number of steps that will give the desired
  ## tolerance and divide the step into that number of smaller steps.
  L <- length(tv)
  yr <- rep(ystart, L)  ## will contain the result
  y <- ystart           ## the current value of yr
  dt <- 1               ## normal step size in t is one *bin*; dydt should adjust for Rate != 1
  for (it in 2:L) {
    dt <- tv[it] - tv[it - 1]  ## will accept fractional times; then dydt should report appropriately
    t <- tv[it - 1]
    RK4 <- rk4.step(y, t, dt, dydt)  ## This does the actual RK4 step, returning an error estimate
    # print (sprintf ('rk4 return for y=%.2f and t=%.1f is %.2f with error estimate %.3f',
    #                 y1, t, RK4[[1]], RK4[[2]]))
    if (abs(RK4[[2]]) <= tol) {      ## Should change this to be a relative error instead (later)
      yr[it] <- y <- RK4[[1]]
    } else {
      N <- as.integer (1.1 * (abs(RK4[[2]]) / tol) ^ 0.3 + 1)
      # print (sprintf ('error estimate is %.3f so using %d steps', RK4[[2]], N))
      
      for (n in 1:N) {
        A <- rk4.step(y, t, dt / N, dydt)    ## Replace the trial step with N smaller steps
        y <- A[[1]]
        if (abs(A[[2]]) > tol) {
          # print (sprintf ('Warning: error estimate still too large, t=%.2f, y=%.2f, err=%.2f', t, y, A[[2]]))
          # Try further reduced step size
          M <- as.integer (1.1 * (abs(A[[2]]) / tol) ^ 0.3 + 1)   ## still smaller steps 
          for (m in 1:M) {
            y <- rk4.step(y, t, dt / (N * M), dydt)[[1]]
            t <- t + dt / (N * M)
          }
        } else {
          t <- t + dt / N
        }
      }
      yr[it] <- y
    }
  }
  return(yr)
}
## These are the coefficients used by the Runge-Kutta Cash-Karp integration.They are
## stored in a data.frame that is assigned to a special environment that can be
## loaded when rk4.stepR() is called. That environment is defined when the Ranadu
## library is loaded.
RKD <- data.frame(
  a2 = 0.2,
  a3 = 0.3,
  a4 = 0.6,
  a5 = 1,
  a6 = 0.875,
  b21 = 0.2,
  b31 = 3 / 40,
  b32 = 9 / 40,
  b41 = 0.3,
  b42 = -0.9,
  b43 = 1.2,
  b51 = -11 / 54,
  b52 = 2.5,
  b53 = -70 / 27,
  b54 = 35 / 27,
  b61 = 1631 / 55296,
  b62 = 175 / 512,
  b63 = 575 / 13824,
  b64 = 44275 / 110592,
  b65 = 253 / 4096,
  c1 = 37 / 378,
  c3 = 250 / 621,
  c4 = 125 / 594,
  c6 = 512 / 1771
)
RKD <-
  cbind(
    RKD,
    data.frame(
      dc1 = RKD$c1 - 2825 / 27648,
      dc3 = RKD$c3 - 18575 / 48384,
      dc4 = RKD$c4 - 13525 / 55296,
      dc5 = -277 / 14336,
      dc6 = RKD$c6 - 0.25
    )
  )
## Save this in a special environment that rk5.stepR() can access:
if (!exists('RKCKEnv', envir = emptyenv())) {
  # define if absent
  assign('RKCKEnv', new.env(parent = emptyenv()), envir = globalenv())
}
ct <-  with(RKD, c(0, a2, a3, a4, a5, a6))
cf <- rep(0, 36); dim(cf) <- c(6, 6)
cf[2, 1] <- RKD$b21
cf[3, 1:2] <- with(RKD, c(b31, b32))
cf[4, 1:3] <- with(RKD, c(b41, b42, b43))
cf[5, 1:4] <- with(RKD, c(b51, b52, b53, b54))
cf[6, 1:5] <- with(RKD, c(b61, b62, b63, b64, b65))
cfn <- with(RKD, c(c1, 0, c3, c4, 0, c6))
cfe <- with(RKD, c(dc1, 0, dc3, dc4, dc5, dc6))
RKCKEnv$RKD <- RKD
RKCKEnv$cf  <- cf
RKCKEnv$ct  <- ct
RKCKEnv$cfn <- cfn
RKCKEnv$cfe <- cfe

rk4.step <- function (y1, t, dt, dydt) {
  # RKD <- RKCKEnv$RKD  ## load the integration constants
  cf <- RKCKEnv$cf  ## 6x6 array
  ct <- RKCKEnv$ct
  cfn <- RKCKEnv$cfn
  cfe <- RKCKEnv$cfe
  evald <- rep(0, 6)

  for (i in 1:6) {  ## six evaluations needed for RK4 and error estimate
    evald[i] <- dydt(y1 + dt * sum(cf[i, ] * evald), t + ct[i] * dt)
  }
  yn <- y1 + dt * sum(cfn * evald)
  err <- dt * sum(cfe * evald)
  return(list(yn, err))
}

#' @title LagrangeInterpolate
#' @description Interpolates tabulated values via Lagrange interpolation
#' @details The routine takes as input a data.frame with two variables, one
#' to be used for interpolation of values in the second. It returns the
#' value of the second variable corresponding to the provided value
#' of the first variable.
#' @author William Cooper
#' @export LagrangeInterpolate
#' @param .x Value of variable #1 for which interpolated result is to be returned
#' @param .n Number of points to use (order of polynomial will be .n-1).
#' The value provided must be in the range from 2 to 10; otherwise NA is returned.
#' @param .D  Data.frame containing the two variables to use for interpolation
#' @return The interpolated value of variable #2 corresponding to the 
#' value provided for variable #1 (.x)
#' @examples
#' LagrangeInterpolate (3.3, 3, data.frame (1:5, 1:5+runif(5,-0.1,0.1)))

LagrangeInterpolate <- function (.x, .n, .D) {
  if (.n < 2 || .n > 10) {return(NA)}
  .D <- .D[do.call(order, as.list(.D)), ] # require increasing order
  .D <- .D[!is.na(.D[, 1]), ]
  # find starting point in array
  L <- nrow (.D)
  y <- rep(NA, length(.x))
  # note: returning end points for calls outside limits
  y[.x < .D[1,1]] <- .D[1,2]
  y[.x > .D[L,1]] <- .D[L,2]
  for (k in 1:length(.x)) {
    if (is.na(y[k])) {
      y[k] <- 0
      m <- which (.x[k] <= .D[,1])[1]
      i1 <- m - ((.n+1) %/% 2)
      if (i1 < 1) {i1 <- 1}
      i1 <- min (i1, L - .n + 1)
      i2 <- i1 + .n -1
      for (i in i1:i2) {
        p <- 1
        for (j in i1:i2) {
          if (j == i) {next}
          p <- p * (.x[k]-.D[j,1]) / (.D[i,1] - .D[j,1])
        }
        y[k] <- y[k] + .D[i,2] * p
      }
    }  
  }
  return (y)
}


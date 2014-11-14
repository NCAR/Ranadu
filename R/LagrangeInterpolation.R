#' @title LagrangeInterpolate
#' @description Interpolates tabulated values via Lagrange interpolation
#' @details The routine takes as input a data.frame with two variables, one
#' to be used for interpolation of values in the second. It returns the
#' value of the second variable corresponding to the provided value
#' of the first variable.
#' @author William Cooper
#' @export LagrangeInterpolate
#' @param .x Value of variable-1 for which interpolated result is to be returned
#' @param .n Number of points to use (order of polynomial will be .n-1).
#' The value provided must be in the range from 2 to 10; otherwise NA is returned.
#' @param .D  Data.frame containing the two variables to use for interpolation
#' @return The interpolated value of variable-2 corresponding to the 
#' value provided for variable-1 (.x)
#' @examples 
#' \dontrun{}
LagrangeInterpolate <- function (.x, .n, .D) {
  if (.n < 2 || .n > 10) {return(NA)}
  .D <- .D[order(.D[,1]), ] # require increasing order
  .D <- .D[!is.na(.D[, 1]), ]
  # find starting point in array
  L <- length (.D[ , 1])
  # note: returning end points for calls outside limits
  if (.x < .D[1, 1]) {return (.D[1, 2])}
  if (.x > .D[L, 1]) {return(.D[L, 2])}
  i <- 1  # get first point greater than .x in .D:
  # if this is slow, improve this search
  while ((i < L) && (.x > .D[i,1])) {i <- i + 1}
  i1 <- i - ((.n+1) %/% 2)
  while (i1+.n-1 > L) {i1 <- i1 - 1}
  while (i1 < 1) {i1 <- i1 + 1}
  i2 <- i1 + .n -1
  y <- 0
  for (i in i1:i2) {
    p <- 1
    for (j in i1:i2) {
      if (j == i) {next}
      p <- p * (.x-.D[j,1]) / (.D[i,1] - .D[j,1])
    }
    y <- y + .D[i,2] * p
  }
  return (y)
}


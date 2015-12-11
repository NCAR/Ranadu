#' @title binStats
#' @description bin and calculate mean and st.dev. for each bin
#' @details Given two corresponding variables in a data frame, defines
#' bins in the second, bins the values of the first, and
#' calculates the mean and standard deviation of the first in each bin.
#' The purpose is to provide values for error bars, e.g., for geom_errorbar
#' where ymin=ybar-sigma and ymax=ybar+sigma.
#' @aliases binStats
#' @author William Cooper
#' @export binStats
#' @param .d A data-frame containing at least two columns, the first holding
#' values to partition and summarize and the second the variable used for
#' defining bins. Values from the first will be grouped according to the
#' bins in the second and then a mean and standard deviation will be
#' calculated for each group. Additional column variables will be ignored.
#' @param bins (default 20)
#' @param xlow The lower limit for bins. The default is the minimum of the
#' second variable.
#' @param xhigh The upper limit for bins. The default is the maximum of the
#' second variable.
#' @return A dataframe of dimension c(bins, 4) that contains, for each bin,
#' 'xc' = the center coordinate of the bin (2nd column), 'ybar' = the mean value for the bin
#' (or NA if there are no members of the group), 'sigma' = the standard
#' deviation for the bin, or NA for no members of the group, and 'nb' = 
#' the number of values in the bin.
#' @examples 
#' E <- binStats(RAFdata[, c("PSXC", "ATX")], bins=10)
binStats <- function (.d, bins=20, xlow=min(.d[,2], na.rm=TRUE), 
                      xhigh=max(.d[,2], na.rm=TRUE)) {
  ## ignore rows where the first column is NA
  v <- .d[!is.na(.d[,1]) & !is.na(.d[,2]), 1]   ## variable to be put in bins
  p <- .d[!is.na(.d[,1]) & !is.na(.d[,2]), 2]   ## bin-defining variable
  xc <- ybar <- sigma <- N <- vector ("numeric", bins)
  binLimits <- xlow + (0:bins)/bins * (xhigh-xlow)
  for (i in 1:bins) {
    xc[i] <- (binLimits[i] + binLimits[i+1]) / 2
    y <- v[p > binLimits[i] & p <= binLimits[i+1]]
    N[i] <- length (y)
    if (N[i] > 0) {
      ybar[i] <- mean (y, na.rm=TRUE)
      sigma[i] <- sd (y, na.rm=TRUE)
    } else {
      ybar[i] <- NA
      sigma[i] <- NA
    }
  }
    # in case it is useful to extend this to confidence intervals for
    # the bin mean, based on t-statistic:
    # for conf int of C, use (1+c)/2 for two-tailed CI. DOF=N-1
    # CI = 0.95
    # CIfactor <- qt ((CI+1)/2, N-1)
    # sem <- sigma / sqrt (N)  # standard error of the bin mean
    # ci <- sem * CIfactor     # and add this to the data.frame
  return (data.frame("xc"=xc, "ybar"=ybar, "sigma"=sigma, "nb"=N))
}
  

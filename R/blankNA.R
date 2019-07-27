#' @title blankNA
#' @description Given a RANADU-convention data.frame or tibble, this function
#' sets values of specified variables missing for rows where the .Restr variable
#' is TRUE. Variable attributes are preserved and the remainder of the data.frame 
#' is preserved, with data.frame attributes. 
#' @details The purpose of this function is to enable plots where the excluded
#' variables have gaps rather than connecting lines spanning the exclusion.
#' This is suitable for use in pipes like
#' D %>% blankNA(TASX > 100) %>% select(Time, ATX) %>% plotWAC()
#' Another syntax is to generate the restriction variable:
#' D %>% Rmutate(Restr, TASX > 230) %>% blankNA(Restr) %>% ...
#' @aliases blankNA, BlankNA
#' @author William Cooper
#' @export blankNA
#' @param .d A data.frame or tibble that follows RANADU conventions. It normally
#' should contain a POSIXCT-format variable named "Time" and any variables
#' needed for the calculation of the new variable.
#' @param .Restr A TRUE/FALSE vector indicating which values to set missing, or
#' a statement that will generate such a vector. The default is to make no changes.
#' @param .names A vector of variable names that should be set missing where
#' .Restr is TRUE. The default is to modify all variables except Time, including .Restr
#' @return A new data.frame or tibble with the modified variables.
#' @example DS <- blankNA(RAFdata, RAFdata$TASX > 225, 'ATX')
blankNA <- function (.d, .Restr=rep(TRUE, nrow(.d)), .names=names(.d)[-1]) {
  qRestr <- enquo (.Restr)
  mCase <- environmentName(attr(qRestr, '.Environment')) == environmentName(globalenv())
  print (parent.frame())
  if (mCase) {
    r <- .Restr
    print ('case A')
  } else {
    print ('case B')
  #   r <- .Restr
  #   print (summary (r))
  # } else {
    ## Get the restriction logical vector:
    r <- dplyr::select(.d, !! qRestr)
  }
  dt <- .d
  for (v in .names) {
    .d[r, v] <- NA
  }
  return (transferAttributes(dt, .d))
}

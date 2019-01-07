#' @title df2tibble
#' @description Transform RANADU data.frame to tibble and v.v.
#' @details Given a RANADU data.frame (e.g., produced by getNetCDF()), 
#' construct and return a tibble. The special handling required relates
#' to size-distribution data, stored in a two-dimensional vector as
#' a component of the data.frame. This is not acceptable in a tibble,
#' so this size distribution is converted to a list before producing
#' the tibble. The reverse transformation makes that list back into
#' a RANADU data.frame. In both cases attributes of the size-distribution
#' variables are preserved; this is important because an attribute
#' specifies the sizes corresponding to bins in the size distribution,
#' and this is used in RANADU functions that plot the size distribution. 
#' The converted variables include any for which the data.frame column
#' is a two-dimensional vector; this will be the case for variables like
#' CCDP, CUHSAS, etc. The reason for using this function is to enable the
#' many special features applicable to tibbles, for variables other than
#' the size-distribution variables. (At present, RANADU code plotting the size-
#' distribution variables all is based on the data.frame format.)
#' @aliases df2tibble
#' @author William Cooper
#' @importFrom tibble tibble
#' @export df2tibble
#' @param .d A data-frame following RANADU conventions, or alternately
#' a tibble produced by conversion from such a data.frame.
#' @param reverse (default: FALSE) Convert from a tibble to data.frame,
#' following RANADU conventions for size-distribution variables.
#' @return A tibble corresponding to the input data.frame but in which
#' any two-dimensional size-distribution vectors are converted to lists.
#' The first index in the list corresponds to the Time variable, and
#' the next to the size distribution array. If "reverse" then a tibble
#' so produced is converted back to a RANADU-convention data.frame.
#' @examples 
#' RAFtibble <- df2tibble(RAFdata)
df2tibble <- function(.d, reverse = FALSE) {
  if (reverse) {
    for (cl in names(.d)) {
      ## xoncert any lists to 2d vectors
      clv <- eval (parse (text = paste0 ('.d$', cl)))
      if (is.list(clv)) {
        # print (sprintf( 'var %s is list', cl))
        AA <- unlist(.d[, cl])
        dim(AA) <- c(length(AA) / nrow(.d), nrow(.d)) 
        AA <- t(AA)
        attributes(AA) <- attributes(clv)
        dim(AA) <- c(nrow(.d), length(AA) / nrow(.d)) 
        eval (parse (text = paste0 ('.d$', cl, '<- AA')))
      }
    }
    return(as.data.frame(.d))
  } else {
    for (cl in names(.d)) {
      ## convert any two-dimensional vectors to lists:
      if (length (dim (.d[, cl])) == 2) {
        # print (sprintf('var %s is 2d', cl))
        Attr <- attributes(.d[, cl])
        Attr$dim <- NULL
        ALIST <- apply (.d[, cl], 1, list)
        attributes(ALIST) <- Attr
        ## need the equivalent of .d$CCDP_RPC <- ALIST but with variable name cl
        eval(parse(text = paste0(".d$",cl,"<- ALIST")))
      } 
    }
    return (tibble::as.tibble (.d))
  }
}

tibble2df <- function(.d) {
  df2tibble(.d, reverse = TRUE)
}

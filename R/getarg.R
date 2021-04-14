#' @title getarg
#' @description A convenience function providing flexibility in specification of function arguments.
#' @details The function returns a character-vector of argument names for an input name that can
#' have any of these forms: getarg(c(ATX, DPXC)), getarg(names(Data)), getarg(names(Data)[2:3]), 
#' getarg(nm[2:4]), getarg(ATX), getarg('ATX'), getarg(c('ATX', 'PSXC')), getarg(c(ATX)). 
#' If called with argument Data$ATX, returns c('Data', 'ATX'); for Data[ir, ATX], returns
#' c('Data', 'ir', 'ATX'). These forms are used in some Ranadu functions, notably VSpec(),
#' for more flexibility in the arguments that can be supplied.
#' @aliases getarg, getargs
#' @author William Cooper
#' @importFrom plyr as.quoted
#' @export getarg
#' @param V A variable list specified in any of the ways specified above.
#' @examples 
#' getarg(c(RTH1, RTH2))
#' 
getarg <- function(V) {
  X <- substitute(V)
  if (is.call(X)) {
    V <- try(eval(X), silent=TRUE)
    if(grepl('Error', V[[1]])) {
      V <- eval(as.quoted(X))  # eval(X) for names()
    }
    if (is.character(V[1])) {
    } else {
      V <- as.quoted(X)
      if(is.symbol(V[[1]])) {
        V <- vapply(V, deparse, 'character')
      } 
    }
  } else {
    V <- as.quoted(X)
    if(is.symbol(V[[1]])) {
      V <- vapply(V, deparse, 'character')
    } 
  }
  return(V)
}

## test cases:
# getarg(c(ATX, DPXC))     #3
# getarg(names(Data))      #1
# getarg(names(Data)[2:3]) #1
# getarg(nm[2:4])          #1
# getarg(ATX)              #5
# getarg('ATX')            #5
# getarg(c('ATX', 'PSXC')) #1 or #2 or #4
# getarg(c(ATX))           #3
# getarg(Data$ATX)

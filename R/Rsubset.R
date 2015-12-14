#' @title Rsubset
#' @description Construct subset data.frame while preserving attributes
#' @details The 'subset()' or '[]' methods remove global attributes from the resulting
#' data.frame. This function preserves those attributes or, where necessary (e.g., 
#' Dimensions), changes them appropriately.
#' @aliases RSubset
#' @author William Cooper
#' @export Rsubset
#' @param Data The data.frame from which a subset is extracted. Must contain a
#' POSIX-format Time variable.
#' @param StartT HHMMSS-format starting time, or 0 to start at the first row.
#' @param EndT   HHMMSS-format ending time, or 0 to end at the last row
#' @param Var    A character vector of variables to include, or 'ALL' to include
#' all columns in the data.frame. The default is 'ALL', and Var=NULL produces this
#' result also.
#' @param Test   A logical vector used to include rows. The default is TRUE, which will
#' include all rows. This is provided so that tests like (Data$TASX > 130) can restrict
#' what is included in the subset data.frame
#' @return A subset data.frame that retains the attributes of the original data.frame.
#' @examples 
#' DS <- Rsubset (RAFdata, 201100,201230)
#' 
Rsubset <- function (Data, StartT=0, EndT=0, Var=NULL, Test=TRUE) {
  # Var <- eval (substitute (Var), Data, parent.frame())
  # this works for a single variable but I couldn't get it to work
  # for a case like Var=c(ATX,DPXC). Left some failed attempts here in
  # case I return to this.
#   print (sprintf ("typeof=%s", typeof (substitute (Var))))
#   V <- deparse((eval (substitute (Var))))
#   print (V)
#   if (typeof (substitute (Var)) == "symbol") {
#     Var <- deparse (substitute (Var))
#   }
  if (is.null (Var)) {Var <- "ALL"}
#   if (is.symbol (Var)) {print ("yes, symbol")}
#   if (is.character (Var)) {print ("yes char")}
#   print (Var)
  if (Var == 'ALL' || Var == 'All') {
    D <- subset (Data[setRange (Data, StartT, EndT), ], Test, c(names (Data)))
  } else {
    D <- subset (Data[setRange (Data, StartT, EndT), ], Test, c("Time", Var))
  }
  
  transferAttributes <- function (d, dsub) {  
    ds <- dsub
    for (nm in names (ds)) {
      var <- sprintf ("d$%s", nm)
      A <- attributes (eval (parse (text=var)))
      A$dim <- nrow (ds)
      if (!grepl ('Time', nm)) {
        A$dim <- NULL
        A$class <- NULL
      }
      attributes (ds[,nm]) <- A
    }
    A <- attributes (d)
    A$Dimensions$Time$len <- nrow (ds)
    A$row.names <- 1:nrow (ds)
    A$names <- names (ds)
    attributes (ds) <- A
    return(ds)
  }
  
  DS <- transferAttributes (Data, D)  ## from Data to D
  return (DS)
}

# load the csym data.frame containing symbols and definitions
load (file="~/RStudio/Ranadu/Constants.Rdata")

#' @title StandardConstant
#' @description Provides standard values of constants as used in the Processing Algorithms document, and others.
#' @details If one of the standard symbol names is provided, the routine returns the value associated with that constant. Otherwise, it returns NULL. For special cases described following the parameter Symbol, the returned value is a list of available symbols or an expanded explanation of a specified symbol.
#' @aliases StandardConstant standardConstant
#' @author William Cooper
#' @export StandardConstant
#' @param Symbol One of a set of defined symbols. To see the available symbols, use StandardConstant('?'), and to see a full description of an available symbol, use for example StandardConstant("?Rd").
#' @return The value associated with that symbol.
#' @examples 
#' StandardConstant ("MWW")
StandardConstant <- function (Symbol) {
    if (Symbol == '?') {
    return(row.names(csym))
  }
  if(substring(Symbol,1,1) == '?') {
    return(csym[substring(Symbol,2,nchar(Symbol)),])
  }
  return(csym[Symbol,1])
}

# # here is one way to construct and add to the data.frame 
# # containing the symbol references, values, and explanations:
# csym <- data.frame (c(9.80665,273.15,8.314472e3), row.names=c("g_standard", "Tzero", "Ru"))
# csym <- cbind (csym, data.frame(c("m/s","K","J/(kmol K)")))
# csym <- cbind (csym, data.frame (c("standard-atmosphere acceleration of gravity","T in kelvin corresponding to 0 C", "universal gas constant")))
# colnames(csym) <- c("value","units","description")
# ca <- data.frame ("MWW"=18.0153, row.names="MWW")
# ca[2] <- "kg/kmol"
# ca[3] <- "molecular weight of water"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("MWD"=28.9637, row.names="MWD")
# ca[2] <- "kg/kmol"
# ca[3] <- "molecular weight, weighted average for dry air"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("T3W"=273.16, row.names="T3W")
# ca[2] <- "K"
# ca[3] <- "triple point temperature of water"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("Na"=6.022141e26, row.names="Na")
# ca[2] <- "molecules/kmol"
# ca[3] <- "Avogadro constant"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("kB"=csym["Ru",1]/csym["Na",1], row.names="kB")
# ca[2] <- "J/K"
# ca[3] <- "Boltzman constant"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("Rd"=csym["Ru",1]/csym["MWD",1], row.names="Rd")
# ca[2] <- "J/(kg K)"
# ca[3] <- "gas constant for dry air"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("Rw"=csym["Ru",1]/csym["MWW",1], row.names="Rw")
# ca[2] <- "J/(kg K)"
# ca[3] <- "gas constant for water vapor"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("Re"=6.371229e6, row.names="Re")
# ca[2] <- "m"
# ca[3] <- "radius of the Earth (as used by INUs)"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("cpd"=3.5*csym["Rd",1], row.names="cpd")
# ca[2] <- "J/(kg K)"
# ca[3] <- "specific heat of dry air at constant pressure"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("cvd"=2.5*csym["Rd",1], row.names="cvd")
# ca[2] <- "J/(kg K)"
# ca[3] <- "specific heat of dry air at constant volume"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("gamma"=csym["cpd",1]/csym["cvd",1], row.names="gamma")
# ca[2] <- "dimensionless"
# ca[3] <- "ratio of specific heats for dry air, cpd/cpv"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("Omega"=7.292115e-5, row.names="Omega")
# ca[2] <- "radians/s"
# ca[3] <- "angular rotation rate of the Earth"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("OmegaSch"=sqrt(csym["g_standard",1]/csym["Re",1]), row.names="OmegaSch")
# ca[2] <- "radians/s"
# ca[3] <- "angular frequency of the Schuler oscillation"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("sigmaSB"=5.6704e-8, row.names="sigmaSB")
# ca[2] <- "W/(m^2 K^4)"
# ca[3] <- "Stephan-Boltzmann Constant"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("Cradeg"=pi/180., row.names="Cradeg")
# ca[2] <- "radians/degree"
# ca[3] <- "conversion to radians from degrees"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("Cmeterft"=0.3048, row.names="Cmeterft")
# ca[2] <- "m/ft"
# ca[3] <- "conversion to meters from feet"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("Cmfromnmi"=1852., row.names="Cmfromnmi")
# ca[2] <- "meters/(n mi)"
# ca[3] <- "conversion to meters from n mi"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)
# ca <- data.frame ("Cktmps"=csym["Cmfromnmi",1]/3600., row.names="Cktmps")
# ca[2] <- "knots/(m/s)"
# ca[3] <- "conversion to m/s from knots"
# colnames(ca) <- colnames(csym)
# csym <- rbind (csym, ca)

# here is a model for how to do this faster, if that becomes an issue:
### get dataset
# #load(url("http://dl.dropbox.com/u/61803503/NETtalk.RData"))
# load ("/home/cooperw/Downloads/NETtalk.RData")
# ### set up data.table
# 
# library(data.table)
# library(microbenchmark)
# 
# hash <- function(x) {e <- new.env(hash = TRUE, size = nrow(x),       
#                                   parent = emptyenv()); apply(x, 1, function(col) assign(col[1],     
#                                                                                          as.numeric(col[2]), envir = e)); return(e)}  
# 
# env <- hash(NETtalk)  #assign the dictionary to env
# 
# # NETtalk.table <- data.table(NETtalk, key="word")
# # vectorScan <- function()NETtalk[NETtalk$word=="stuff",]
# # dataTable <- function() NETtalk.table[J("stuff"),]
# hash <- function() get("stuff", e = env)
# 
# (op <- microbenchmark( 
#   #   vectorScan(),
#   #   dataTable(),
#   hash(),        
#   times=1000L))

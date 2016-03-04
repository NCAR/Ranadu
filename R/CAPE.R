## calculate LCL, CAPE, etc.
#' @title CAPE
#' @description Calculates CAPE, convective inhibition, and adiabatic profiles for a sounding.
#' @details For a sounding provided in a data.frame, this function 
#' adds new columns to the data.frame representing pseudo-adiabatic ascent and
#' wet-adiabatic (i.e., reversible) ascent. This routine calls
#' the function LCL and uses the pressure and temperature returned from that function
#' as the starting point for the calculations of upward and downward trajectories. 
#' @aliases CAPE
#' @author William Cooper
#' @export CAPE
#' @import nleqslv stats
#' @param SND A data.frame with named variables 'Pressure', 'Temperature', and
#' 'DewPoint' (note capitalized 'P' in the latter). Respective units should be
#' hPa, deg.C, and deg.C. This data.frame will often be a data.frame or a subset
#' of a dataframe created by Ranadu::getNetCDF(), possibly with times selected
#' using the Start and End arguments to that function. Multiple segments can be
#' bound together using the R function rbind. If the variables 'Pressure',
#' 'Temperature', and 'DewPoint' are not provided, the routine will search for the
#' respective substitutes 'PSCX', 'ATX', and 'DPXC' and will use those instead if they
#' are found. If neither set of veriables is found, the function fails.
#' @param nbins An integer representing the number of bins into which the sounding
#' will be partitioned in pressure. The default is 50.
#' @param player A numeric variable specifying the depth in hPa over which to
#' average the lowest layer in the sounding for calculation of the LCL. The default
#' is 50.
#' @return The supplied data.frame with the addition of six columns, for which
#' all pressure values in the original sounding will have corresponding values 
#' for each of these variables: (1) TP: the
#' temperature for pseudo-adiabatic ascent above or dry-adiabatic descent below the 
#' LCL; (2) TPV: the virtual temperature corresponding to TP; (3) TQ the temperature 
#' for wet-adiabatic ascent above the LCL or
#' dry-adiabatic descent below the LCL; (4) TQV the virtual temperature corresonding
#' to TPV, for which the weight of condensed liquid water is accounted for in the
#' calculation; (5) TVIR: the virtual temperature of the
#' original sounding, used for the calculation of buoyancy; and (6) LWC: the profile of
#' condensed liquid water content [g/m^3] for reversible ascent.
#' The function also adds some attributes to the
#' data.frame: LCL (lifted condensation level) pressure and temperature (attribute
#' names 'LCLp' and LCLt' in respective units of hPa and deg.C), the peak LWC
#' that develops during the wet-adiabatic ascent, the CAPE (convective available
#' potential energy, attribute name 'CAPE') and the corresponding value for reversible
#' adiabatic ascent (attribute name 'CAPEW'), the convective inhibition (attribute
#' name 'CIN'), the level of free convection (attribute name 'LFC'), and the
#' LCL value of pseudo-adiabatic equivalent potential temperature ('THP') and of
#' wet-equivalent potential temperature ('THQ'). Units for CAPE, CAPEwet, and CIN 
#' are J/kg, for LFC is hPa, and for THP and THQ are kelvin. These values can be 
#' retrieved from the returned data.frame via calls like 'attr(NSND, "LFC")' where
#' NSND is returned from CAPE().
#' @examples 
#' \dontrun{
#' Data <- getNetCDF ('/scr/raf_data/CONTRAST/CONTRASTrf01.nc', 
#'                    c('PSXC', 'ATX', 'DPXC'), Start=250400, End=260100)
#' NSND <- CAPE (Data)
#' }

## example of usage:
# Data <- getNetCDF ('/Data/CONTRAST/CONTRASTrf01.nc', c('PSXC', 'ATX', 'DPXC'),
#                    Start=250400, End=260100)
# NSND <- CAPE (Data)
# 
# S <- SkewTSounding (Data, AverageInterval=2)
# S2 <- SkewTSounding (data.frame (Pressure=NSND$Pressure, Temperature=NSND$TP, DewPoint=-120), ADD=TRUE)
# S3 <- SkewTSounding (data.frame (Pressure=NSND$Pressure, Temperature=NSND$TQ, DewPoint=-120), ADD=TRUE)
# S <- S + geom_path (data=S2, aes (x=AT, y=P), colour='red', lwd=1.0)
# S <- S + geom_path (data=S3, aes (x=AT, y=P), colour='green', lwd=1.0)
# plcl <- attr (NSND, 'LCLp'); tlcl <- attr (NSND, 'LCLt')
# maxLWC <- attr (NSND, 'MaxLWC'); pmaxLWC <- attr (NSND, 'pMaxLWC')
# SP <- SkewTSounding (data.frame(Pressure=plcl, Temperature=tlcl, DewPoint=-120), ADD=TRUE)
# S <- S + geom_point (data=SP, aes(x=AT, y=P), pch=19, colour='darkorange', size=4)
# labelText <- paste(sprintf('orange dot: LCL %.1f hPa %.2f degC', plcl, tlcl), 
#                    'red line: pseudo-adiabatic ascent', 
#                    'bright green line: wet-adiabatic ascent',
#                    sprintf ('max LWC: %.2f g/m3 at %.1f hPa', maxLWC, pmaxLWC),
#                    sprintf ('cape=%.0f J/kg (adiabatic cape=%.0f)', 
#                             attr(NSND, 'CAPE'), attr (NSND, 'CAPEW')),
#                    sprintf ('conv. inh. %.0f J/kg, LFC=%.0f hPa', 
#                             attr(NSND, 'CIN'), attr(NSND, 'LFC')), sep='\n')
# S <- S + geom_label (aes(x=0, y=2.85, label=labelText), size=4.5, fill='ivory', hjust='left')
# print(S)

CAPE <- function (SND, nbins=50, player=50) {
  TZERO <- StandardConstant('Tzero')
  namesSND <- names (SND)
  if ('Pressure' %in% namesSND) {
    prd <- SND$Pressure
  } else {
    if ('PSXC' %in% namesSND) {
      prd <- SND$PSXC
    } else {
      print ('CAPE data.frame is missing a variable for pressure; cannot proceed')
      return ()
    }
  }
  if ('Temperature' %in% namesSND) {
    trd <- SND$Temperature
  } else {
    if ('ATX' %in% namesSND) {
      trd <- SND$ATX
    } else {
      print ('CAPE data.frame is missing a variable for temperature; cannot proceed')
      return ()
    }
  }
  if ('DewPoint' %in% namesSND) {
    dpd <- SND$DewPoint
  } else {
    if ('DPXC' %in% namesSND) {
      dpd <- SND$DPXC
    } else {
      print ('CAPE data.frame is missing a variable for dewpoint; cannot proceed')
      return ()
    }
  }
  ## remove any NAs:
  testNA <- !is.na(prd) & !is.na(trd) & !is.na(dpd)
  prd <- prd[testNA]
  trd <- trd[testNA]
  dpd <- dpd[testNA]
  ## assume measured supersaturation is erronous:
  dpd[dpd > trd] <- trd[dpd > trd]
  pmaxl <- max (prd, na.rm=TRUE)
  ## find average theta and mixing ratio in lowest 'DPLCL mb'player' hPa
  ## (Do this before binning to use all data)
  ev <- MurphyKoop (dpd)
  rmix <- MixingRatio (ev/prd)
  theta <- PotentialTemperature (prd, trd, ev)
  thetabar <- mean (theta[prd > (pmaxl-player)], na.rm=TRUE)
  rbar <- mean (rmix[prd > (pmaxl-player)], na.rm=TRUE)
  if (is.na (thetabar)) {return ()}
  CL <- LCL (1000, thetabar-TZERO, rbar)
  # print (sprintf ('thetabar %f rbar %f CL[1] %f CL[2] %f', thetabar, rbar, CL[1], CL[2]))
  plcl <- CL[1]
  tlcl <- CL[2]
  ## now average measurements by binning in pressure:
  B1 <- binStats (data.frame (AT=trd, P=prd), bins=nbins)
  B2 <- binStats (data.frame (DP=dpd, P=prd), bins=nbins)
  NEWSND <- data.frame(Pressure = B1$xc, Temperature = B1$ybar, DewPoint = B2$ybar)
  NEWSND <- NEWSND[do.call (order, -NEWSND), ]    ## order by decreasing pressure
  ev <- MurphyKoop (NEWSND$DewPoint)
  rmix <- 0.622 * ev / (NEWSND$Pressure - ev)
  NEWSND$TV <- VirtualTemperature (NEWSND$Temperature, rmix) 
  ## find pseudo-adiabatic equivalent potential temperature and wet-equiv pot T at LCL
  THP <- EquivalentPotentialTemperature(plcl, tlcl)  ## function will insert equilibrium e
  THQ <- WetEquivalentPotentialTemperature(plcl, tlcl)

  ## now calculate profile downward and two upward
  findDA <- function (theta, r, p) {
    EoverP <- r / (0.622 + r)
    SH <- SpecificHeats(EoverP)
    RbyCp <- SH[,3] / SH[,1]
    return (theta * (p/1000)^RbyCp-TZERO)
  }
  findEA <- function (thp, r, p) {
    fzPAET <- function (t, p, thp) {
      return (EquivalentPotentialTemperature (p, t) - thp)  
    }
    lp <- length (p)
    EoverP <- r / (0.622 + r)
    e <- EoverP * p
    te <- vector (length=lp)
    for (i in 1:lp) {
      X <- nleqslv::nleqslv (-10, fzPAET, jac=NULL, p[i], thp)
      te[i] <- X$x
    }
    return (te)
  }
  
  ## set up placeholder variables for the new columns
  newcol <- NEWSND$Temperature
  NEWSND <- cbind(NEWSND, data.frame(TP=newcol, TPV=newcol, TQ=newcol, TQV=newcol, LWC=rep(0., nrow(NEWSND))))
  ixlow <- NEWSND$Pressure > plcl
  ixhigh <- NEWSND$Pressure < plcl
  if (sum (ixhigh) < 1) {
    print ('LCL is above range of pressures, so no CAPE or profiles can be calculated.')
    print ('Returning NA')
    return (NA)
  }
  ## given p and theta, find t:
  prb <- NEWSND$Pressure[ixlow]
  ## pseudo-adiabatic ascent:
  NEWSND$TP[ixlow] <- findDA(thetabar, rbar, prb)
  NEWSND$TQ[ixlow] <- NEWSND$TP[ixlow]
  prp <- NEWSND$Pressure[ixhigh]
  NEWSND$TP[ixhigh] <- findEA (THP, rbar, prp)
  ## adiabatic ascent:
  X <- AdiabaticTandLWC (plcl, tlcl, prp)
  trw <- X$Tobs
  ALWC <- X$ALWC
  NEWSND$TQ[ixhigh] <- trw
  NEWSND$LWC[ixhigh] <- ALWC
  ## now have temperature profiles. However, CAPE/CIN based on buoyancy, so
  ## need virtual temperature.  For trp, can assume equilibrium vapor pressure. For 
  ## trw, also use equilibrium, but must compensate for weight of water content.
  ## For both, also use the environmental sounding in terms of virtual
  ## temperature:
  # tvenv <- tv[prd < plcl]
  eqp <- MurphyKoop (NEWSND$TP[ixhigh])
  reqp <- c(rep (rbar, sum (ixlow)), MixingRatio (eqp/prp))
  NEWSND$TPV <- VirtualTemperature (NEWSND$TP, reqp)
  eqw <- MurphyKoop (NEWSND$TQ[ixhigh])
  reqw <- c(rep (rbar, sum(ixlow)), MixingRatio (eqw/prp))
  NEWSND$TQV <- VirtualTemperature (NEWSND$TQ, reqw)
  rhoair <- NEWSND$Pressure * 100 / (StandardConstant ('Rd') * (NEWSND$TQV + TZERO))
  qc <- NEWSND$LWC * 1.e-3 / rhoair
  NEWSND$TQV = NEWSND$TQV - (NEWSND$TQV+TZERO)*qc
  
  ## ready to find CAPE, LFC, CIN
  ## LFC: first point where buoyancy is positive
  cape <- 0; cinp <- 0; capew <- 0; cinw <- 0
  plast <- plcl
  blast <- 0; blastw <- 0
  lfc <- 0
  maxLWC <- max(ALWC, na.rm=TRUE)
  ilwcmax <- which (maxLWC == ALWC)
  pmaxLWC <- prp[ilwcmax]
  ep <- reqp / (reqp + 0.622)
  ew <- reqw / (reqw + 0.622)
  ## note, by definition of virtual temperature, need dry-air gas constant here:
  Buoyancy <- SpecificHeats (0)[,3] * (NEWSND$TPV - NEWSND$TV)  ## poor variable name; see notes
  BuoyancyQ <- SpecificHeats (0)[,3] * (NEWSND$TQV - NEWSND$TV)
  LogP <- log(NEWSND$Pressure)
  ## use LagrangeInterpolation function to provide Buoyancy given LogP
  LI <- data.frame (LogP=LogP, Buoyancy=Buoyancy)
  LIQ <- data.frame (LogP=LogP, Buoyancy=BuoyancyQ)
  ## define function for integration
  LIF <- function (x, LI) {
    return (LagrangeInterpolate (.x=x, .n=7, .D=LI))
  }
  ## where does parcel become negatively buoyant?
  plow <- min (NEWSND$Pressure[NEWSND$TPV-NEWSND$TV > 0], na.rm=TRUE)  ## top point for the integration
  plowQ <- min (NEWSND$Pressure[NEWSND$TQV-NEWSND$TV > 0], na.rm=TRUE)
  phigh <- NEWSND$Pressure[(Buoyancy > 0) & (NEWSND$Pressure < plcl)][1]  ## highest-pressure positive Buoyancy
  phighQ <- NEWSND$Pressure[(BuoyancyQ > 0) & (NEWSND$Pressure < plcl)][1]
  ## refine the limits:
  if (plow > min (NEWSND$Pressure, na.rm=TRUE)) {
    iplow <- which (plow == NEWSND$Pressure)
    plow <- plow - Buoyancy[iplow] / (Buoyancy[iplow] - Buoyancy [iplow + 1]) * (plow - NEWSND$Pressure[iplow + 1])
  }
  if (is.na(phigh) || is.na(phighQ)) {
    print ('no points with positive buoyancy; returning only original sounding')
    return (NEWSND[, c('Pressure', 'Temperature', 'DewPoint')])
  }
  if (phigh < max (NEWSND$Pressure, na.rm=TRUE)) {
    iphigh <- which (phigh == NEWSND$Pressure)
    phigh <- NEWSND$Pressure[iphigh-1] - Buoyancy[iphigh-1] / (Buoyancy[iphigh] - Buoyancy [iphigh - 1]) * 
      (phigh - NEWSND$Pressure[iphigh - 1])
  }
  if (plowQ > min (NEWSND$Pressure, na.rm=TRUE)) {
    iplowQ <- which (plowQ == NEWSND$Pressure)
    plowQ <- plowQ - BuoyancyQ[iplowQ] / (BuoyancyQ[iplowQ] - BuoyancyQ [iplowQ + 1]) * (plowQ - NEWSND$Pressure[iplowQ + 1])
  }
  if (phighQ < max (NEWSND$Pressure, na.rm=TRUE)) {
    iphighQ <- which (phighQ == NEWSND$Pressure)
    phighQ <- NEWSND$Pressure[iphighQ-1] - BuoyancyQ[iphighQ-1] / (BuoyancyQ[iphighQ] - BuoyancyQ [iphighQ - 1]) * 
      (phighQ - NEWSND$Pressure[iphighQ - 1])
  }
  lphigh <- log (phigh)
  lplow <- log (plow)
  lphighQ <- log (phighQ)
  lplowQ <- log (plowQ)
  lfc <- phigh
  # print (sprintf ('plow=%.3f, phigh=%.3f plowQ=%.3f, phighQ=%.3f', plow, phigh, plowQ, phighQ))
  Int <- integrate (f=LIF, lower=lphigh, upper=lplow, LI, subdivisions=max(100, nbins), rel.tol=0.01)
  IntQ <- integrate (f=LIF, lower=lphighQ, upper=lplowQ, LIQ, subdivisions=max (100, nbins), rel.tol=0.01)
  # print (sprintf ('results of integration are %.1f %.1f', Int$value, IntQ$value))
  ## similarly, get convective inhibition calculated as integral from surface to LFC:
  plowB <- lfc
  phighB <- plcl
  if (lfc < plcl) {
    lplowB <- log(plowB)
    lphighB <- log (phighB)
    IntB <- integrate (f=LIF, lower=lphighB, upper=lplowB, LI, subdivisions=max(100, nbins), rel.tol=0.01)
    cin <- IntB$value
  } else {
    cin <- 0
  }
## the following is an alternate Euler-method integration:
#   ix <- 1:nrow(NEWSND)
#   ix <- ix[ixhigh]
#   for (i in ix) {
# #     dc <- SpecificHeats (reqp[i]/(reqp[i]+0.622))[3] * 0.5 * (tvp[i]-tvenv[i]+blast) * log (plast/prp[i])
# #     dcw <- SpecificHeats (reqw[i]/(reqw[i]+0.622))[3] * 0.5 * (tvw[i]-tvenv[i]+blastw) * log (plast/prp[i])
#     dc <- SpecificHeats (0)[3] * 0.5 * (NEWSND$TPV[i]-NEWSND$TV[i]+blast) * log (plast/NEWSND$Pressure[i])
#     dcw <- SpecificHeats (0)[3] * 0.5 * (NEWSND$TQV[i]-NEWSND$TV[i]+blastw) * log (plast/NEWSND$Pressure[i])
#     # print (sprintf ('i=%d, p=%.1f, tvp/tveng=%.2f %.2f, dc=%.1f', i, prp[i], tvp[i], tvenv[i], dc))
# #     print (sprintf ('i=%d dc=%.2f reqp=%.5f TPV=%.2f TV=%.2f plast=%.2f p=%.2f',
# #                     i, dc, reqp[i], NEWSND$TPV[i], NEWSND$TV[i], plast, NEWSND$Pressure[i]))
#     if (dc > 0) {
#       cape <- cape + dc
#       # if (lfc == 0) {lfc <- prp[i]}
#     } else {
#       if (cape < 1) {cinp <- cinp+dc}
#     }
#     if (dcw > 0) {
#       capew <- capew + dcw
#     } else {
#       if (capew == 0) {cinw <- cinw+dcw}
#     }
#     plast <- NEWSND$Pressure[i]
#     blast <- NEWSND$TPV[i] - NEWSND$TV[i]   # tvp[i]-tvenv[i]
#     blastw <- NEWSND$TQV[i]-NEWSND$TV[i]
#   }
#   print (sprintf ('cape=%.2f, cin=%.2f', cape, cinp))
#   cape <- cape
#   capew <- capew
#   cin <- cin
  attr (NEWSND, 'LCLp') <- CL[1]
  attr (NEWSND, 'LCLt') <- CL[2]
  attr (NEWSND, 'THP') <- THP
  attr (NEWSND, 'THQ') <- THQ
  attr (NEWSND, 'CAPE') <- -Int$value
  attr (NEWSND, 'CAPEW') <- -IntQ$value
  attr (NEWSND, 'CIN') <- cin
  attr (NEWSND, 'LFC') <- lfc
  attr (NEWSND, 'MaxLWC') <- maxLWC
  attr (NEWSND, 'pMaxLWC') <- pmaxLWC
  return (NEWSND)
}



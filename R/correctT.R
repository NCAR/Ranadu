#' @title correctT
#' @description Calculate time-response-corrected and dynamic-heating-filtered
#' temperature variables for each temperature measurement in a supplied data.frame.
#' @details For a data.frame supplied with some variables representing the air
#' temperature, corrected values for each are calculated and added to the supplied
#' data.frame. This routine is specific to the temperature sensors on the
#' NSF/NCAR GV and C-130, either unheated Rosemount sensors or heated HARCO
#' sensors, and it depends on conventional attributes in the NCAR/EOL/RAF
#' netCDF files to determine how to handle each sensor. The supplied data.frame 
#' must contain additional variables as specified for the first parameter below.
#' @author William Cooper
#' @export correctT
#' @importFrom zoo na.approx
#' @importFrom signal filter, filtfilt, butter
#' @param .data A data.frame containing measurements of air temperature and
#' also airspeed (TASX), Mach Number (either MACHX or XMACH2), ambient pressure
#' (PSXC) and water vapor pressure (either EWX or EDPC),
#' @return The supplied data.frame with new variables added that represent 
#' correction for time response (added with suffix "C") and replacement of the
#' conventional dynamic-heating correction by a version filtered to match
#' the time response of the sensor (added with suffix "F").
#' @examples 
#' DFC <- correctT (RAFdata)
## Using this script: from a unix shell:

require(Ranadu, quietly = TRUE, warn.conflicts=FALSE)
# needed packages
library(zoo)
require(signal)
load(file='../SensibleHeatFlux/ARF.Rdata')    ## the filters
load(file='../SensibleHeatFlux/PAR.Rdata')  ## the response parameters
# 
# print (sprintf ('run parameters: Project = %s, Flight = %s, FFT = %s RTN = %s UH1 = %s',
#                 Project, Flight, FFT, RTN, UH1))

recoveryF <- function(D, TV) {  ## TV should be a temperature column in D
  # get the recovery factor from the attribute:
  rf.txt <- attr(TV, 'RecoveryFactor')
  if (is.null (rf.txt)) {  ## protection for omitted attribute
    rf <- 0.985
  } else {
    if (is.numeric(rf.txt)) {
      rf <- rf.txt
    } else {
      if (grepl('mach', rf.txt)) {
        rf <- gsub('mach', 'MACHX', rf.txt)
        rf <- gsub(' log', ' * log', rf)
        rf <- gsub(' \\(', ' * \\(', rf)
        rf <- with(D, eval(parse(text=rf)))
        rf <- SmoothInterp(rf, .Length = 0)
      } else {
        rf <- 0.985  ## placeholder -- if needed, add decoding here
      }
    }
  }
  return (rf)
}

correctDynamicHeating <- function(D, AT) {
  platform <- attr(D, 'Platform')
  platform <- ifelse (grepl('677', platform), 'GV', 'C130')
  heated <- attr(D[, AT], 'long_name')
  heated <- ifelse(grepl('nheated', heated), FALSE, TRUE)
  if (grepl('^ATR', AT)) {heated <- FALSE}  ## old name convention
  # Select the right filter
  Rate <- attr(D, 'Rate')
  if (Rate == 25) {
    if (platform == 'GV') {
      if (heated) {
        AF <- ARH
        Lsh <- LshiftH
      } else {
        AF <- ARG
        Lsh <- Lshift
      }
    } else {  ## - C130 case
      if (heated) {
        AF <- ARH
        Lsh <- LshiftH
      } else {
        AF <- AR
        Lsh <- Lshift
      }
    }
  } else { ## assumed 1 Hz
    if (platform == 'GV') {
      if (heated) {
        AF <- ARH1
        Lsh <- LshiftH1
      } else {
        AF <- ARG1
        Lsh <- Lshift1
      }
    } else {
      if (heated) {
        AF <- ARH1
        Lsh <- LshiftH1
      } else {
        AF <- AR1
        Lsh <- Lshift1
      }
    }
  }

  rf <- recoveryF(D, AT)
  # Is the associated recovery temperature present?
  dep <- attr(D[, AT], 'Dependencies')
  RT <- gsub(' .*', '', gsub('^. ', '', dep))
  # Get the dynamic-heating correction:
  if ('EWX' %in% names(D)) {
    ER <- SmoothInterp(D$EWX / D$PSXC, .Length = 0)
    CP <- SpecificHeats(ER)
  } else {
    CP <- SpecificHeats()
  }
  if (RT %in% names(D)) {
    X <- rf * D$MACHX^2 * CP[, 3] / (2 * CP[, 2])
    Q <- (273.15 + D[, RT]) * X / (1 + X)
  } else {
    Q <- rf * D$TASX^2 / 2010
  }
  # Interpolate to avoid missing values
  Q <- SmoothInterp(Q, .Length = 0)
  # Filter
  QF <- as.vector(signal::filter(AF, Q))
  QF <- ShiftInTime(QF, .shift=(-(Lsh) * 1000 / Rate), .rate = Rate)
  # Change the measured air temperature by adding back Q and subtracting QF
  ATC <- D[, AT] + Q - QF
  return(ATC)
}

## The following applies either to recovery temperature or to air temperature after
## filtering the dynamic-heating term:
correctTemperature <- function(D, RT, responsePar = 
                                 list(a = 0.733, tau1 = 0.0308, tau2 = 0.447)){
  Rate <- attr(D, 'Rate')
  RTT <- D[, RT]
  # Protect against missing values by interpolating:
  RTT <- zoo::na.approx(as.vector(RTT), maxgap = 1000 * Rate, na.rm = FALSE, rule = 2)
  RTT[is.na(RTT)] <- 0
  heated <- attr(D[, RT], 'long_name')
  heated <- ifelse(grepl('nheated', heated), FALSE, TRUE)
  ## This indication is often missing from RTFx:
  if ((grepl('RTF', RT)) || (grepl('ATF', RT)) || (grepl('^ATR', RT)) || (grepl('^RTR', RT))) {
    heated <- FALSE
  }
  a <- responsePar$a
  tau1 <- responsePar$tau1
  tau2 <- responsePar$tau2
  if (heated) {
    # DTMDT <- c(0, diff(RTT, 2), 0) * Rate / 2
    # DTM2DT2 <- (c(diff(RTT), 0) - c(0, diff(RTT))) * Rate ^ 2
    DTMDT <-  (c(0, 8 * diff(RTT, 2), 0) -
                 c(0, 0, diff(RTT, 4), 0, 0)) * Rate / 12
    DTM2DT2 <- (-c(0,0, RTT)[1:nrow(D)] + 16*c(0,RTT)[1:nrow(D)] 
                - 30 * RTT + 16 * c(RTT[2:nrow(D)], 0) 
                - c(RTT[3:nrow(D)], 0, 0)) * (Rate^2) / 12
    RTC <- (tau1 + tau2) * DTMDT + RTT + tau1 * tau2 * DTM2DT2
    RTC <- zoo::na.approx (
      as.vector(RTC),
      maxgap = 1000 * Rate,
      na.rm = FALSE,
      rule = 2
    )
    CutoffPeriod <- ifelse(Rate == 25, Rate / 2, 5)
    RTC <- signal::filtfilt (signal::butter (3,
                                             2 / CutoffPeriod), RTC)
  } else {
    ## RTT is the working solution; Ts is the support temperature
    Ts <- RTT
    Rate <- attr (D, 'Rate')
    # DTMDT <- c(0, diff(RT, 2), 0) * Rate / 2
    DTMDT <-  (c(0, 8 * diff(RTT, 2), 0) -
                 c(0, 0, diff(RTT, 4), 0, 0)) * Rate / 12
    fS <- function(y, i) {
      ((tau1 * DTMDT[i] + RTT[i] - (1 - a) * y) / a - y) / (Rate * tau2)
    }
    
    Ts <- rk4.integrate (fS, Ts[1], 1:nrow(D))
    RTC <- (1 / a) * (tau1 * DTMDT + RTT - (1 - a) * Ts)
  }
  return(RTC)
}

addFFTsoln <- function(D, RV, responsePar) {
  j1 <- which(D$TASX > 90)[1]
  j2 <- which(D$TASX > 90)
  j2 <- j2[length(j2)]
  DS <- D[j1:j2,]
  Rate <- attr(D, 'Rate')
  DS$TASX <- SmoothInterp(DS$TASX, .Length = 0)
  DS$QCF <- SmoothInterp(DS$QCF, .Length = 0)
  DS[, RV] <- SmoothInterp(DS[, RV], .Length = 0)
  RVC <- paste0(RV, 'C')
  DS[, RVC] <- DS[, RV]  ## column to hold the corrected values
  DD <- D
  DD[, RVC] <- DD[, RV]
  N <- ifelse(Rate == 25, 2^14, 2*9)  ## about 10.9 min at 25 Hz, 8.5 min at 1 Hz
  DT <- N / (Rate * 60)
  df <- Rate / N
  ## as needed for the Fourier transform (fft()):
  frq <- c(seq(0, Rate / 2, by = df), seq(-Rate / 2 + df, -df, by = df))
  N2 <- N %/% 2
  fmax <- 2
  nlim <- which(frq > fmax)[1]
  ## Proceed sequentially through the time series:
  irw <- nrow(DS)
  ir <- seq(1, irw - N, by = N2)
  i1 <- N2 %/% 2 + 1
  i2 <- i1 + N2 - 1
  rhozero <- 1013.25 * 100 / (287.05 * 288.15)
  rPar <- responsePar
  for (i in ir) {
    DSW <- DS[i:(i+N-1), ]
    f <- fft (DSW[, RV])
    ## get the mean values of air density and Mach number, for tau1 adjustment:
    MRHO <- MachNumber(DSW$PSXC, DSW$QCXC) * 
      DSW$PSXC * 100 / (287.05 * (273.15 + DSW$ATX)) / rhozero
    rPar$tau1 <- responsePar$tau1 * (0.3 / mean(MRHO, na.rm = TRUE)) ^ 0.68
    ## Modify the spectrum by the inverse of the response function:
    AFFT <- LTphase(frq, rPar)
    AFFT$frq <- frq
    AFFT$Phase <- AFFT$Phase * pi / 180
    fp <- fft(DSW[, RV])
    H <- complex (modulus = AFFT$Amp, argument = AFFT$Phase)
    xn <- Re(fft(fp / H, inverse = TRUE)) / N
    FFT <- xn
    if (i == 1) {
      DS[1:i2, RVC] <- FFT[1:i2]
    } else if (i == ir[length(ir)]) {
      DS[(i+i1-1):(i+N-1), RVC] <- FFT[i1:N]
    } else {
      ## save only the central part
      DS[(i + (i1:i2) - 1), RVC] <- FFT[i1:i2]
    }
  }
  rvc <- c(DD[1:(j1-1), RVC], DS[, RVC], DD[(j2+1):nrow(DD), RVC])
  return(rvc)
}

correctT <- function(.data) {
  ## Find the available air_temperature variables:
  Rate <- attr(.data, 'Rate')
  nms <- names(.data)
  TVARS <- vector()
  for (nm in nms) {
    measurand <- attr(.data[, nm], 'measurand')
    if (!(is.null(measurand)) && grepl('air_temp', measurand)) {
      ## Omit unheated sensor if 1-Hz?
      lna <- attr(.data[, nm], 'long_name')
      if (Rate == 25 || !(grepl('nheated', lna) || grepl('^ATR', nm))) {
        TVARS <- c(TVARS, nm)
      }
    }
  }
  TVARS <- TVARS[-which ('ATX' == TVARS)]  # don't include ATX, AT_A, AT_A2
  if ('AT_A' %in% TVARS) {TVARS <- TVARS[-which('AT_A' == TVARS)]}
  if ('AT_A2' %in% TVARS) {TVARS <- TVARS[-which('AT_A2' == TVARS)]}
  if ('AT_VXL' %in% TVARS) {TVARS <- TVARS[-which('AT_VXL' == TVARS)]}
  if ('AT_VXL2' %in% TVARS) {TVARS <- TVARS[-which('AT_VXL2' == TVARS)]}
  if ('OAT' %in% TVARS) {TVARS <- TVARS[-which('OAT' == TVARS)]} #omit Ophir T
  RVARS <- sub('^A', 'R', TVARS)
  ## get the old netCDF variables needed to calculate the modified variables
  VarList <- standardVariables(TVARS)
  ## Treat case, in some old projects, where EWX and MACHX are not present:
  if (!('EWX' %in% nms)) {
    VarList <- VarList[-which('EWX' == VarList)]
    VarList <- c(VarList, 'EDPC')
  }
  if (!('MACHX' %in% nms)) {
    VarList <- VarList[-which('MACHX' == VarList)]
    VarList <- c(VarList, 'XMACH2')
  }
  if (!('GGALT' %in% nms)) {
    VarList <- VarList[-which('GGALT' == VarList)]
    VarList <- c(VarList, 'GGALT_NTL')
  }
  ## Add the recovery temperatures if present; otherwise recalculate:
  RVS <- RVARS[RVARS %in% nms]
  ## The next lines deal with the case where, for a 25-Hz file, the recovery 
  ## temperature is present only at 1-Hz rate (e.g., WECANrf08h.nc)
  if (length(RVS) > 0) {
    if (Rate == 25) {
      for (RV in RVS) {  ## only add if present at 25 Hz in netCDF file
        if (attr(.data[, RV], 'Dimensions')[[1]] == 'sps25') {
          VarList <- c(VarList, RV)
        }
      }
    } else {
      VarList <- c(VarList, RVS)
    }
  }
  if ('EDPC' %in% VarList) {
    .data$EWX <- .data$EDPC
  }
  if ('XMACH2' %in% VarList) {
    .data$MACHX <- sqrt(.data$XMACH2)
  }
  if ('GGALT_NTL' %in% VarList) {
    .data$GGALT <- .data$GGALT_NTL
  }

  ## Calculate the new variables:
  E <- SmoothInterp(.data$EWX / .data$PSXC, .Length = 0)
  .data$Cp <- SpecificHeats (E)[, 1]
  .data$DH <- .data$TASX^2 / (2 * .data$Cp)
  ## Recalculate recovery temperatures if missing:
  retrievedRVARS <- rep(FALSE, length(RVARS))
  for (RV in RVARS) {
    if(!(RV %in% VarList)) {
      TV <- sub('^R', 'A', RV)
      prb <- 'HARCO'
      if (TV == 'ATH2') {prb <- 'HARCOB'}
      if (grepl('ATF', TV) || grepl('ATR', TV)) {prb <- 'UNHEATED'}
      retrievedRVARS[which(RV == RVARS)] <- TRUE
      .data[, RV] <- SmoothInterp(.data[, TV] + RecoveryFactor(.data$MACHX, prb) * .data$DH, .Length = 0)
      ## modify the attributes for saving in the new file:
      attr(.data[, RV], 'long_name') <- paste0("Recovery Air Temperature, ", prb)
      attr(.data[, RV], 'standard_name') <- NULL
      attr(.data[, RV], 'actual_range') <- NULL
      attr(.data[, RV], 'Dependencies') <- NULL
      attr(.data[, RV], 'measurand') <- "recovery_temperature"
      attr(.data[, RV], 'label') <- paste0("recovery temperature (", RV, ") [deg C]")
      attr(.data[, RV], 'RecoveryFactor') <- NULL
      attr(.data[, RV], 'DataQuality') <- "Reconstructed"
    }
  }
  platform <- attr(.data, 'Platform')
  platform <- ifelse (grepl('677', platform), 'GV', 'C130')
  # filter DH:
  CutoffPeriod <- rep(Rate * 1.0, length(TVARS)) # Standard is 1 s for DH filtering
  probe <- rep('HARCO', length(TVARS))  # used to determine the recovery factor
  PAR <- list()
  if (length(TVARS) > 0) {
    for (i in 1:length(TVARS)) {
      PAR[[length(PAR) + 1]] <- ParamSH
    }
  }
  # check for ATF or ATR
  ic <- which(grepl('ATF', TVARS))
  if (length(ic) > 0) {
    CutoffPeriod[ic] <- Rate * 0.5  # 0.5 s for ATFx
    probe[ic] <- 'UNHEATED'
    if (platform == 'GV') {
      PAR[[ic]] <- ParamSF
    } else {
      PAR[[ic]] <- Param1
    }
  }
  ic <- which(grepl('ATR', TVARS))  ## old naming convention (e.g., FRAPPE)
  if (length(ic) > 0) {
    CutoffPeriod[ic] <- Rate * 0.5  # 0.5 s for ATFx
    probe[ic] <- 'UNHEATED'
    for (icc in ic) {
      if (platform == 'GV') {
        PAR[[icc]] <- ParamSF
      } else {
        PAR[[icc]] <- Param1
      }
    }
  }
  ic <- which(grepl('ATH.*2', TVARS))  ## .* allows for names like ATHL2
  if (length(ic) > 0) {
    probe[ic] <- 'HARCOB'
    PAR[[ic]] <- ParamSH
  }
  if (Rate == 1) {  # Protection against script failure for a LRT file
    CutoffPeriod[CutoffPeriod == 1] <- 2.2
    CutoffPeriod[CutoffPeriod == 0.5] <- 2.2
  }
  
  DHM <- rep(.data$DH, length(TVARS))
  dim(DHM) <- c(length(.data$DH), length(TVARS))
  newTVARS <- paste0(TVARS, 'C')
  newRVARS <- paste0(RVARS, 'C')
  newRVARS2 <- paste0(RVARS, 'C2')
  newTVARS2 <- paste0(TVARS, 'C2')
  filteredTVARS <- paste0(TVARS, 'F')
  for (i in 1:length(TVARS)) {
    .data[, filteredTVARS[i]] <- correctDynamicHeating(.data, as.character(TVARS[i]))
  }
  
  ## Calculate the corrected temperatures:
  for (i in 1:length(TVARS)) {
    .data[, newTVARS[i]] <- correctTemperature(.data, filteredTVARS[i], responsePar = PAR[[i]])
    .data[, newRVARS[i]] <- correctTemperature(.data, RVARS[i], responsePar = PAR[[i]])
    # if (FFT) {
    #   .data[, newRVARS2[i]] <- addFFTsoln(.data, RVARS[i], responsePar = PAR[[i]])
    # }
    # get the recovery factor from the attribute:
    rf <- recoveryF(.data, TVARS[[i]])
    # Is the associated recovery temperature present?
    dep <- attr(.data[, TVARS[i]], 'Dependencies')
    RT <- gsub(' .*', '', gsub('^. ', '', dep))
    # Get the dynamic-heating correction:
    if ('EWX' %in% names(.data)) {
      ER <- SmoothInterp(.data$EWX / .data$PSXC, .Length = 0)
      CP <- SpecificHeats(ER)
    } else {
      CP <- SpecificHeats()
    }
    if (RT %in% VarList) {
      X <- rf * .data$MACHX^2 * CP[, 3] / (2 * CP[, 2])
      Q <- (273.15 + .data[, RT]) * X / (1 + X)
    } else {
      Q <- rf * .data$TASX^2 / 2010
    }
    # Interpolate to avoid missing values
    Q <- SmoothInterp(Q, .Length = 0)
    ## This should be the same as newTVARS except for numerical effects
    # .data[, newTVARS2[i]] <- .data[, newRVARS[i]] - Q  ## using diff.eq.
    # if (FFT) {
    #   .data[, newTVARS2[i]] <- .data[, newRVARS2[i]] - Q  ## using FFT
    # }
  }
  return(.data)
}

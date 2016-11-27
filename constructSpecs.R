graphSpecs <- function () {
  specs <- list()
  specs$type <- 'history'
  specs$panels <- 2
  specs$columns <- 1
  specs$restrict <- FALSE
  .var <- c('GGALT', 'PALT')
  .col <- c('blue','darkgreen')
  .lw <- c(1,1.5)
  .lt <- c(1,2)
  .lab <- .var
  .ylim <- c(NA,NA)
  .logY <- FALSE
  .smoothed <- c(FALSE,FALSE)
  .fixed <- FALSE
  .SGlength <- c(61, 61)
  sf <- function (.var, .col, .lw, .lt, .lab, .ylim, .logY, .smoothed, .fixed, .SGlength) {
    list(var=.var, col=.col, lw=.lw, lt=.lt,
                      lab=.lab, ylim=.ylim, logY=.logY, 
                      smoothed=.smoothed, fixed=.fixed, SGlength=.SGlength)
  }
  s1 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .smoothed, .fixed, .SGlength)
  .var <- c('ATX', 'DPXC')
  .lab <- .var
  s2 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .smoothed, .fixed, .SGlength)
  .var <- c('PSXC', 'PS_A')
  .lab <- .var
  s3 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .smoothed, .fixed, .SGlength)
  .var <- c('QCXC', 'QC_A')
  .lab <- .var
  s4 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .smoothed, .fixed, .SGlength)
  .var <- c('WSC', 'WIC')
  .lab <- .var
  s5 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .smoothed, .fixed, .SGlength)
  .var <- c('WDC', 'THDG')
  .lab <- .var
  s6 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .smoothed, .fixed, .SGlength)
  specs$panel <- list(s1, s2, s3, s4, s5, s6)
  return (specs)
}
specvarSpecs <- function () {
  specs <- list ()
  specs$type='variance'
  specs$restrict <- FALSE
  .var <- 'WIC'
  .cvar <- 'TASX'
  .lab <- .var
  .xlim <- c(NA,NA)
  .ylim <- c(NA,NA)
  .fixed <- FALSE
  .spectype <- 'fft'
  .fftpts <- 512
  .fftwindow <- 'Parzen'
  .fftavg <- 50
  .ffttype <- 'fp(f)'
  .acvtau <- 600
  .acvwindow <- 'Parzen'
  .acvavg <- 50
  .acvtype <- 'fp(f)'
  .MEMtype <- 'fp(f)'
  .MEMpoles <- 50
  .MEMres <- 0.0001
  .MEMavg <- 50
  .MEMadd <- FALSE
  .MEMcolor <- 'blue'
  s <- function (.var, .cvar, .lab, .xlim, .ylim,
                 .fixed, .spectype, .fftpts, .fftwindow,
                 .fftavg, .ffttype, .acvtau, .acvwindow, .acvavg,
                 .acvtype, .MEMtype, .MEMpoles, .MEMres, .MEMavg, .MEMadd, .MEMcolor) {
    list (var=.var, cvar=.cvar, lab=.lab, xlim=.xlim,
          ylim=.ylim, fixed=.fixed, spectype=.spectype,
          fftpts=.fftpts, fftwindow=.fftwindow,
          fftavg=.fftavg, ffttype=.ffttype, acvtau=.acvtau,
          acvwindow=.acvwindow, acvavg=.acvavg, acvtype=.acvtype,
          MEMtype=.MEMtype,
          MEMpoles=.MEMpoles, MEMres=.MEMres, MEMavg=.MEMavg, .MEMadd, .MEMcolor)
  }
  s1<- s(.var, .cvar, .lab, .xlim, .ylim,
          .fixed, .spectype, .fftpts, .fftwindow,
          .fftavg, .ffttype, .acvtau, .acvwindow, .acvavg, 
          .acvtype, .MEMtype, .MEMpoles, .MEMres, .MEMavg, .MEMadd, .MEMcolor)
  specs$Definition <- s1
  return (specs)
}
scatSpecs <- function () {
  specs <- list()
  specs$type <- 'scatterplot'
  specs$panels <- 6
  specs$columns <- 2
  specs$restrict <- FALSE
  .varx <- c('GGALT')
  .vary <- c('ATX', 'DPXC')
  .col <- c('blue','darkgreen')
  .size <- c(4,4)
  .symbol <- c(20,20)
  .lab <- .vary
  .xlim <- c(NA,NA)
  .ylim <- c(NA,NA)
  .logX <- FALSE
  .logY <- FALSE
  .fixed <- FALSE
  s <- function (.varx, .vary, .col, .size, .symbol, .lab, .xlim, .ylim, .logX, .logY, .fixed) {
    list(varx=.varx, vary=.vary, col=.col, size=.size, symbol=.symbol, lab=.lab,
         xlim=.xlim, ylim=.ylim, logX=.logX, logY=.logY, fixed=.fixed)
  }
  s1 <- s(.varx, .vary, .col, .size, .symbol, .lab, .xlim, .ylim, 
                        .logX, .logY, .fixed)
  .varx <- 'PSXC'
  .vary <- c('ATX', 'DPXC')
  .lab <- .vary
  s2 <- s(.varx, .vary, .col, .size, .symbol, .lab, .xlim, .ylim, 
                        .logX, .logY, .fixed)
  .varx <- 'GGALT'
  .vary <- c('WSC','TASX')
    .lab <- .vary
  s3 <- s(.varx, .vary, .col, .size, .symbol, .lab, .xlim, .ylim, 
                        .logX, .logY, .fixed)
  .vary <- c('QCXC', 'QCXC')
  .lab <- .vary
  s4 <- s(.varx, .vary, .col, .size, .symbol, .lab, .xlim, .ylim, 
                        .logX, .logY, .fixed)
  .vary <- c('WSC', 'WIC')
  .lab <- .vary
  s5 <- s(.varx, .vary, .col, .size, .symbol, .lab, .xlim, .ylim, 
                        .logX, .logY, .fixed)
  .vary <- c('WDC', 'THDG')
  .lab <- .vary
  s6 <- s(.varx, .vary, .col, .size, .symbol, .lab, .xlim, .ylim, 
                        .logX, .logY, .fixed)
  specs$panel <- list (s1, s2, s3, s4, s5, s6)
  return (specs)
}

ps <- list ()
ps$Project <- 'ORCAS'
ps$Flight <- 1
ps$TypeFlight <- 'rf'
D <- Ranadu::getNetCDF ('/Data/ORCAS/ORCASrf01.nc')
ps$Times <- c(D$Time[1], D$Time[nrow(D)])
ps$Plot <- list (graphSpecs (), graphSpecs(), graphSpecs(), graphSpecs(), graphSpecs())
ps$Hist <- list(graphSpecs (), graphSpecs(), graphSpecs(), graphSpecs(), graphSpecs())
ps$Hist[[1]]$type <- 'histogram'
ps$Hist[[2]]$type <- 'histogram'
ps$Hist[[3]]$type <- 'histogram'
ps$Hist[[4]]$type <- 'histogram'
ps$Hist[[5]]$type <- 'histogram'
ps$StatVar <- Ranadu::standardVariables()
ps$Variance <- list (specvarSpecs (), specvarSpecs (), specvarSpecs (), specvarSpecs(), specvarSpecs())
ps$Restrictions <- plotSpec$Restrictions
# load(file='plotSpec.def')
ps$Scat <- list (scatSpecs(), scatSpecs(), scatSpecs(), scatSpecs(), scatSpecs())
ps$Bin <- list (scatSpecs(), scatSpecs(), scatSpecs(), scatSpecs(), scatSpecs())
ps$paluchLWC <- "PLWCC"
ps$PaluchTimes <- ps$Times
ps$PaluchCTimes <- ps$Times
plotSpec <- ps
save (plotSpec, file='plotSpec.def')


## clear global environment that might be left from the last run
rm(list=ls(all=TRUE))

suppressMessages (
  library(shiny, quietly=TRUE, warn.conflicts=FALSE)
)
library(shinyBS, quietly=TRUE, warn.conflicts=FALSE)
suppressMessages (suppressWarnings (
  library(Ranadu, quietly=TRUE, warn.conflicts=FALSE))
)
library(gtable)
library(grid)
library(XML)
library(tcltk)

# source ('R/plotTrack.R')
# source ('R/PlotWAC.R')
# source ('R/getNetCDF.R')
# source ('R/makeNetCDF.R')
# source ('R/setVariableList.R')
# source ('R/CAPE.R')
# source ('R/setVariableList.R')
## if this is set TRUE then messages will print in the console
## indicating which functions are entered, to trace the sequence
## of interactions when window entries are changed.
Trace <- FALSE
Trace <- TRUE

## assemble a list of projects for which an appropriately named rf01
## exists in the data directory:

PJ <- c('ORCAS', 'CSET', 'NOREASTER', 'HCRTEST',
        'DEEPWAVE', 'CONTRAST', 'SPRITE-II', 'MPEX', 'DC3',
        'TORERO', 'HIPPO-5', 'HIPPO-4', 'HIPPO-3', 'HIPPO-2',
        'HIPPO-1','PREDICT', 'START08', 'PACDEX', 'TREX')
for (P in PJ) {
  if (grepl('HIPPO', P)) {
    fn <- sprintf ('%sHIPPO/%srf01.nc', DataDirectory (), P)
  } else {
    fn <- sprintf ('%s%s/%srf01.nc', DataDirectory (), P, P)
    if (!file.exists (fn)) {
      fn <- sub ('\\.nc', '.Rdata', fn)
    }
    if (!file.exists (fn)) {
      fn <- sprintf ('%s%s/%stf01.nc', DataDirectory (), P, P)
    }
    if (!file.exists (fn)) {
      fn <- sub ('\\.nc', '.Rdata', fn)
    }
  }
  if (!file.exists (fn)) {PJ[PJ==P] <- NA}
}
PJ <- PJ[!is.na(PJ)]

## specification files for plots are lists
trackSpecs <- function () {
  specs <- list()
  specs$type <- 'track'
  .var <- c('LATC', 'LONC', 'WDC', 'WSC')
  specs$panels <- 1
  specs$panel <- list (var=.var)
  return (specs)
}
graphSpecs <- function () {
  specs <- list()
  specs$type <- 'history'
  specs$panels <- 6
  specs$columns <- 1
  specs$restrict <- FALSE
  .var <- c('GGALT', 'PALT')
  .col <- c('blue','darkgreen')
  .lw <- c(1,1.5)
  .lt <- c(1,2)
  .lab <- .var
  .ylim <- c(NA,NA)
  .logY <- FALSE
  .stamp <- FALSE
  .fixed <- FALSE
  sf <- function (.var, .col, .lw, .lt, .lab, .ylim, .logY, .stamp, .fixed) {
    list(var=.var, col=.col, lw=.lw, lt=.lt,
         lab=.lab, ylim=.ylim, logY=.logY, 
         stamp=.stamp, fixed=.fixed)
  }
  s1 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .stamp, .fixed)
  .var <- c('ATX', 'DPXC')
  .lab <- .var
  s2 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .stamp, .fixed)
  .var <- c('PSXC', 'PS_A')
  .lab <- .var
  s3 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .stamp, .fixed)
  .var <- c('QCXC', 'QC_A')
  .lab <- .var
  s4 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .stamp, .fixed)
  .var <- c('WSC', 'WIC')
  .lab <- .var
  s5 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .stamp, .fixed)
  .var <- c('WDC', 'THDG')
  .lab <- .var
  s6 <- sf(.var, .col, .lw, .lt, .lab, .ylim, .logY, .stamp, .fixed)
  specs$panel <- list(s1, s2, s3, s4, s5, s6)
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
  .vary <- c('QCXC', 'QC_A')
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
ltyps <- c('solid', 'dashed', 'dotted', 'd-dot', 'lg dash') ## in order, line types 1:5

netCDFfile <- NA
CCDP <- NA
CFSSP <- NA
CUHSAS <- NA
CPCASP <- NA
C1DC <- NA
# graphSpecs <- function () {
#   specs <- list()
#   specs$type <- 'history'
#   specs$panels <- 6
#   specs$columns <- 1
#   .var <- c('GGALT', 'PALT')
#   .col <- c('blue','darkgreen')
#   .lw <- c(1,1.5)
#   .lt <- c('solid', 'dashed')
#   .lab <- .var
#   .ylim <- c(NA,NA)
#   .float <- FALSE
#   .logY <- FALSE
#   .stamp <- FALSE
#   specs$panel <- list(var=.var, col=.col, lw=.lw, lt=.lt,
#                       lab=.lab, ylim=.ylim, float=.float, logY=.logY, 
#                       stamp=.stamp)
#   .var <- c('ATX', 'DPXC')
#   .col <- c('blue','darkgreen')
#   .lab <- .var
#   .ylim <- c(NA,NA)
#   .float <- FALSE
#   .logY <- FALSE
#   .stamp <- FALSE
#   specs$panel <- list (specs$panel, list(var=.var, col=.col, lw=.lw, lt=.lt,
#                                          lab=.lab, ylim=.ylim, float=.float, logY=.logY, 
#                                          stamp=.stamp))
#   .var <- c('PSXC', 'PS_A')
#   .col <- c('blue','darkgreen')
#   .lab <- .var
#   specs$panel[[3]] <- list(var=.var, col=.col, lw=.lw, lt=.lt,
#                            lab=.lab, ylim=.ylim, float=.float, logY=.logY, 
#                            stamp=.stamp)
#   .var <- c('QCXC', 'QC_A')
#   .col <- c('blue','darkgreen')
#   .lab <- .var
#   specs$panel[[4]] <- list(var=.var, col=.col, lw=.lw, lt=.lt,
#                            lab=.lab, ylim=.ylim, float=.float, logY=.logY, 
#                            stamp=.stamp)
#   .var <- c('WSC', 'WIC')
#   .col <- c('blue','darkgreen')
#   .lab <- .var
#   specs$panel[[5]] <- list(var=.var, col=.col, lw=.lw, lt=.lt,
#                            lab=.lab, ylim=.ylim, float=.float, logY=.logY, 
#                            stamp=.stamp)
#   .var <- c('WDC', 'THDG')
#   .col <- c('blue','darkgreen')
#   .lab <- .var
#   specs$panel[[6]] <- list(var=.var, col=.col, lw=.lw, lt=.lt,
#                            lab=.lab, ylim=.ylim, float=.float, logY=.logY, 
#                            stamp=.stamp)
#   return (specs)
# }
# 
# plotSpec <- list ()
# plotSpec[[1]] <- graphSpecs ()
# plotSpec[[2]] <- graphSpecs ()
# plotSpec[[3]] <- graphSpecs ()
# plotSpec[[4]] <- graphSpecs ()
# plotSpec[[5]] <- graphSpecs ()

# load ('plotSpec.def')
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# functions used later:
hline <<- function(y, col='black', lwd=1, lty=2) {
  abline(h=y, col=col, lwd=lwd, lty=lty)
}

formatTime <- function (time) {
  t <- as.POSIXlt (time, tz='UTC')
  tt <- sprintf ("%d:%02d:%02d", t$hour, t$min, t$sec)
  return (tt)
}

saveConfig <- function (inp) {
  save (plotSpec, file=inp$save, ascii=TRUE)
}
loadConfig <- function (inp) {
  load (file=inp$restore)
  plotSpec <<- plotSpec
  print (sprintf ('loadConfig, file=%s', inp$restore))
}
load (file='plotSpec.def')  ## this loads initial values of plotSpec and Restrictions

fileChoose <- function (newwd) {
  oldwd <- setwd (newwd)
#   while(getwd() != normalizePath(newwd)) {
#     Sys.sleep(0.02)
#   }
#   print (c('wd:', getwd(), newwd, oldwd))
#   Z <- list.files()
#   print (Z)
  try(fn <- file.choose (), silent=TRUE)
  # if (!exists('fn')) {fn <- NULL}
  setwd (oldwd)
  return (fn)
}

Project <- plotSpec$Project
Flight <- plotSpec$Flight
TypeFlight <- plotSpec$TypeFlight
Production <- FALSE
fn <- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), Project, 
               Project, TypeFlight, Flight)
if (!file.exists (fn)) {
  if (Trace) {print (sprintf ('%s not found', fn))}
  fn <- sub ('\\.nc', '.Rdata', fn)
}
if (!file.exists (fn)) {
  if (Trace) {print (sprintf ('%s not found', fn))}
  fn <- sprintf ('%s%s/%stf01.nc', DataDirectory (), Project, Project)
}
if (!file.exists (fn)) {
  if (Trace) {print (sprintf ('%s not found', fn))}
  fn <- sub ('\\.nc', '.Rdata', fn)
}
fname <- fn
fname.last <- ''
## if Production load production-file info
if (Production) {
  print (sprintf ('production section in global, Production=%d',
                  Production))
  dr <- sprintf ('%s../raf/Prod_Data/%s', DataDirectory (), Project)
  scmd <- sprintf ('ls -lt `/bin/find %s -ipath "\\./movies" -prune -o -ipath "\\./*image*" -prune -o -name %s%s%02d.Rdata`',
                   dr, Project, 'rf', Flight)
  fl <- system (scmd, intern=TRUE)[1]
  if ((length (fl) > 0) && (!grepl ('total', fl))) {
    fn <- sub ('.* /', '/', fl[1])
  }
}

if (!file.exists (fn)) {
  if (Trace) {print (sprintf ('%s not found', fn))}
  warning ('need tf01 or rf01 to initialize')
  return (VRPlot)
}
# print (sprintf ('setting chp/slp from %s', fn))
FI <- DataFileInfo (fn)
quickPlotVar <- 'GGALT'

limitData <- function (Data, inp, lim=NA) {
  DataV <- Data
  namesV <- names(DataV)
  namesV <- namesV[namesV != "Time"]
  if (is.na (lim)) {lim <- inp$restrict}
  if (lim) {
    t <- rep (FALSE, nrow(DataV))
    Restrictions <- plotSpec$Restrictions
    for (i in 1:nrow(Restrictions)) {
      if (Restrictions$apply[i]) {
        t <- t | (!is.na (DataV[, Restrictions$RVAR[i]]) & 
                    ((DataV[, Restrictions$RVAR[i]] < Restrictions$min[i]) |
                       (DataV[, Restrictions$RVAR[i]] > Restrictions$max[i])))
      }
    }
    #     t <- !is.na (DataV$TASX) & (DataV$TASX < inp$minTAS)
    #     t <- t | (abs(DataV$ROLL) > inp$maxROLL)
    #     t <- t | (DataV$GGALT/1000 < inp$minZ)
    #     t <- t | (DataV$VSPD > inp$maxROC)
    t[is.na(t)] <- FALSE
    DataV[t, namesV] <- NA
  }
  return (DataV)
}

end2d <- c(0, 0, 0)
last2d <- end2d
## function to read a 2D record
readRecord <- function (cfile) {
  a <- readBin(cfile, integer(), 10, size=2, signed=FALSE, endian='swap')
  if (length (a) < 10) {return (-1)}
  probe <- a[1]
  hour <- a[2]
  minute <- a[3]
  second <- a[4]
  year <- a[5]
  month <- a[6]
  day <- a[7]
  tas <- a[8]
  msec <- a[9]
  overld <- a[10]
  #   print (sprintf ('record %d date %d-%02d-%02d time %d:%02d:%02d.%03d probe %x resltion %d diodes %d tas %d overld %d',
  #                   i, year, month, day, hour, minute, second, msec, probe, resltion, nDiodes, tas, overld))
  start2d <<- end2d
  end2d <<- c(hour, minute, second)
  image <- readBin(cfile, raw(), 4096, endian='swap')
}

## the following function writes some configuration for interaction with
## 'Xanadu' where the spectral analysis is performed.
setXanadu <- function (fnew, start, end, var, cvar, wlow, whigh, type) {
  ## edit the .def files for the Xanadu call
  if (end < start) {
    end <- end + 240000
  }
  lines <- readLines ("Xanadu.def")
  newlines <- vector ("character")
  for (line in lines) {
    if (grepl ("XANFILE", line)) {
      line <- gsub ("=.*", sprintf ("=%s", gsub ("\\.nc", '', fnew)), line)
    }
    newlines[length (newlines) + 1] <- line
  }
  writeLines (newlines, "Xanadu.def")
  ## and the otto.def file
  lines <- readLines ("otto.def.template")
  newlines <- vector ("character")
  for (line in lines) {
    if (grepl ("START", line)) {
      line <- gsub (" [0-9]*", sprintf (" %d", start), line)
    }
    if (grepl ("END", line)) {
      line <- gsub (" [0-9]*", sprintf (" %d", end), line)
    }
    if (substr (line, 1, 4) == "VAR ") {
      line <- gsub (" [A-Z]*", sprintf (" %s", var), line)
    }
    if (substr (line, 1, 6) == "COVAR ") {
      line <- gsub (" [A-Z]*", sprintf (" %s", cvar), line)
    }
    if (substr (line, 1, 4) == "WLOW") {
      line <- gsub (" .*", sprintf (" %f", wlow), line)
    }
    if (substr (line, 1, 5) == "WHIGH") {
      line <- gsub (" .*", sprintf (" %f", whigh), line)
    }
    if (substr (line, 1, 6) == 'BATMEM') {
      n <- ifelse (type == 'MEM', 1, 0)
      line <- gsub (' .*', sprintf(' %d', n), line)
    }
    if (substr (line, 1, 6) == 'BATFFT') {
      n <- ifelse (type == 'fft', 1, 0)
      line <- gsub (' .*', sprintf(' %d', n), line)
    }
    if (substr (line, 1, 6) == 'BATACV') {
      n <- ifelse (type == 'acv', 1, 0)
      line <- gsub (' .*', sprintf(' %d', n), line)
    }
    if (type == 'fft') {
      if (substr (line, 1, 4) == 'SEGL') {
        line <- sub (' .*', sprintf (' %d', 
                                     plotSpec$Variance[[1]]$Definition$fftpts), line)
      }
      if (substr (line, 1, 6) == 'WINDOW') {
        window <- switch (plotSpec$Variance[[1]]$Definition$fftwindow,
                          Parzen=1,
                          Welch=3,
                          Hanning=4,
                          2)
        line <- sub (' .*', sprintf (' %d', window-1), line)
      }
      if (substr (line, 1, 7) == 'SMOOTHB') {
        line <- sub (' .*', sprintf (' %d', 
                                     plotSpec$Variance[[1]]$Definition$fftavg), line)
      }
      if (substr (line, 1, 7) == 'SHOWFFT') {
        typ <- switch (plotSpec$Variance[[1]]$Definition$ffttype,
                       'fp(f)'=4,
                       'p(f)'=2,
                       'eps(f)'=8,
                       0)
        line <- sub (' .*', sprintf (' %d', typ), line)
      }
      if (substr (line, 1, 8) == 'SHOWCFFT') {
        typ <- switch (plotSpec$Variance[[1]]$Definition$ffttype,
                       'cospec. / quad.'=32,
                       'coherence / phase'=16,
                       'both fp(f)'=48,
                       1)
        line <- sub (' .*', sprintf (' %d', typ), line)
      }
    }
    if (type == 'acv') {
      if (substr (line, 1, 7) == 'SHOWACV') {
        typ <- switch (plotSpec$Variance[[1]]$Definition$acvtype,
                       'fp(f)'=4,
                       'p(f)'=2,
                       'autocorrelation'=16,
                       1)
        line <- sub (' .*', sprintf (' %d', typ), line)
      }
      if (substr (line, 1, 7) == 'SMOOTHS') {
        line <- sub (' .*', sprintf (' %d', plotSpec$Variance[[1]]$Definition$acvtau), line)
      }
      if (substr (line, 1, 7) == 'SMOOTHB') {
        line <- sub (' .*', sprintf (' %d', 
                                     plotSpec$Variance[[1]]$Definition$acvavg), line)
      }
      if (substr (line, 1, 6) == 'WINDOW') {
        window <- switch (plotSpec$Variance[[1]]$Definition$acvwindow,
                          Parzen=1,
                          Welch=3,
                          Hanning=4,
                          2)
        line <- sub (' .*', sprintf (' %d', window-1), line)
      }
    }
    if (type == 'MEM') {
      if (substr (line, 1, 7) == 'SHOWMEM') {
        typ <- switch (plotSpec$Variance[[1]]$Definition$MEMtype,
                       'fp(f)'=4,
                       'p(f)'=2,
                       1)
        line <- sub (' .*', sprintf (' %d', typ), line)
      }
      if (substr (line, 1, 5) == 'POLES') {
        line <- sub (' .*', sprintf (' %d', 
                                     plotSpec$Variance[[1]]$Definition$MEMpoles), line)
      }
      if (substr (line, 1, 7) == 'SMOOTHB') {
        line <- sub (' .*', sprintf (' %d', 
                                     plotSpec$Variance[[1]]$Definition$MEMavg), line)
      }
      if (substr (line, 1, 4) == 'RESN') {
        line <- sub (' .*', sprintf (' %f', plotSpec$Variance[[1]]$Definition$MEMres), line)
      }
    }
    
    newlines[length (newlines) + 1] <- line
  }
  writeLines (newlines, "otto.def")
  return()
}

choose2Dfile <- function () {
  oldwd <- setwd ('/Data')
  fname2 <<- file.choose ()
  setwd (oldwd)
}

chooseVar <- function (fname, inp) {
  plotSpec$StatVar <<- setVariableList (fname, plotSpec$StatVar)
}
chooseQVar <- function (fname, inp) {
  quickPlotVar <<- setVariableList (fname, single=TRUE)
}
chooseXfrVar <- function (fname, inp) {
  xVarList <<- setVariableList (fname, VarList)
}

addedVariables <- c('PITCH', 'THETA', 'THETAP')

makeVarList <- function () {
  if (Trace) {print ('entered VarList')}
  VarList <- standardVariables (addedVariables)
  for (plt in 1:length(plotSpec$Plot)) {
    for (pnl in 1:plotSpec$Plot[[plt]]$panels) {
      VarList <- c(VarList, plotSpec$Plot[[plt]]$panel[[pnl]]$var)
    }
  }
  for (plt in 1:length(plotSpec$Hist)) {
    for (pnl in 1:plotSpec$Hist[[plt]]$panels) {
      VarList <- c(VarList, plotSpec$Hist[[plt]]$panel[[pnl]]$var)
    }
  }
  for (plt in 1:length(plotSpec$Scat)) {
    for (pnl in 1:plotSpec$Scat[[plt]]$panels) {
      VarList <- c(VarList, plotSpec$Scat[[plt]]$panel[[pnl]]$varx)
      VarList <- c(VarList, plotSpec$Scat[[plt]]$panel[[pnl]]$vary)
    }
  }
  for (plt in 1:length(plotSpec$Bin)) {
    for (pnl in 1:plotSpec$Bin[[plt]]$panels) {
      VarList <- c(VarList, plotSpec$Bin[[plt]]$panel[[pnl]]$varx)
      VarList <- c(VarList, plotSpec$Bin[[plt]]$panel[[pnl]]$vary)
    }
  }
  for (iv in 1:length(plotSpec$Variance)) {
    VarList <- c(VarList, plotSpec$Variance[[iv]]$Definition$var, plotSpec$Variance[[iv]]$Definition$cvar)
  }
  if (plotSpec$paluchLWC %in% FI$Variables) {VarList <- c(VarList, plotSpec$paluchLWC)}
  if ('THETAQ' %in% FI$Variables) {VarList <- c(VarList, 'THETAQ')}
  if (length(nwc <- which (grepl ('CONCD_', FI$Variables))) == 1) {
    VarList <- c(VarList, FI$Variables[nwc])
    if (Trace) {print (sprintf ('added %s to VarList', FI$Variables[nwc]))}
  }
  VarList <- c(VarList, c('LATC', 'LONC', 'WDC', 'WSC', 'ATX', 
                          'DPXC', 'TASX', 'ROLL', 'VSPD',
                          'THDG', 'SSLIP'), plotSpec$StatVar, quickPlotVar)
  VarList <- unique (VarList)
  ## if variable is in specialData, exclude it:
  if (exists ('specialData')) {
    vwh <- which (VarList %in% names (specialData))
    if (length(vwh) > 0) {
      VarList <- VarList [-vwh]
    }
  }
  return (VarList)
}
VarList <- makeVarList()
VarListLast <- VarList


fname.last <- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), plotSpec$Project, 
                       plotSpec$Project, plotSpec$TypeFlight, plotSpec$Flight)
Data <- getNetCDF (fname.last, VarList)

# times <- c(Data$Time[1], Data$Time[nrow(Data)])
step <- 60
minT <- Data$Time[1]
minT <- minT - as.integer (minT) %% step + step
maxT <- Data$Time[nrow(Data)]
maxT <- maxT - as.integer (maxT) %% step 
times <- c(minT, maxT)
if (plotSpec$Times[1] > times[1]) {times <- c(plotSpec$Times[1], maxT)}
if (plotSpec$Times[2] < times[2]) {times <- c(times[1], plotSpec$Times[2])}

# Restrictions <- data.frame()
# Restrictions[1, 'RVAR'] <- 'TASX'
# Restrictions[1, 'apply'] <- TRUE
# Restrictions[1, 'min'] <- 130
# Restrictions[1, 'max'] <- 300
defFiles <- list.files(pattern = "^plotSpec")

transferAttributes <- function (d, dsub) {    
  ds <- dsub
  for (nm in names (d)) {
    if (nm %in% names (dsub)) {
      var <- sprintf ("d$%s", nm)
      A <- attributes (eval (parse (text=var)))
      A[[1]] <- nrow (ds)
      if (!grepl ('Time', nm)) {
        A$dim <- NULL
        A$class <- NULL
      }
      attributes (ds[,nm]) <- A
    }
  }
  A <- attributes (d)
  A$Dimensions$Time$len <- nrow (ds)
  A$row.names <- 1:nrow (ds)
  A$names <- names (ds)
  attributes (ds) <- A
  return(ds)
}

saveRdata <- function (Data, inp) {
  print ('entered saveRdata')
  netCDFfile <- nc_open (sprintf ('%s%s/%s%s%02d.nc', DataDirectory (),
                                  inp$Project, inp$Project, inp$typeFlight,
                                  inp$Flight))
  nms <- c('Time', 'TASX')
  Time <- ncvar_get (netCDFfile, "Time")
  TASX <- ncvar_get (netCDFfile, "TASX")
  time_units <- ncatt_get (netCDFfile, "Time", "units")
  tref <- sub ('seconds since ', '', time_units$value)
  Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
  namesCDF <- names (netCDFfile$var)
  if (length (grep ("CCDP_", namesCDF)) > 0) {
    nm <- namesCDF[grepl("^CCDP_", namesCDF)]
    nms <- c(nms, 'CCDP')
    CCDP <- ncvar_get (netCDFfile, nm)
    CellSizes <- ncatt_get (netCDFfile, nm, "CellSizes")
    CellLimitsD <- CellSizes$value
    attr (CCDP, 'CellLimits') <- CellLimitsD
  }
  if (length (grep ("CS100_", namesCDF)) > 0) {
    nm <- namesCDF[grepl("^CS100_", namesCDF)]
    nms <- c(nms, 'CS100')
    CFSSP <- ncvar_get (netCDFfile, nm)
    CellSizes <- ncatt_get (netCDFfile, nm, "CellSizes")
    CellLimitsF <- CellSizes$value
    attr (CFSSP, 'CellLimits') <- CellLimitsF
  }
  if (length (grep ("CUHSAS_", namesCDF)) > 0) {
    nm <- namesCDF[grepl("^CUHSAS_", namesCDF)]
    nms <- c(nms, 'CUHSAS')
    CUHSAS <- ncvar_get (netCDFfile, nm)
    CellSizes <- ncatt_get (netCDFfile, nm, "CellSizes")
    CellLimitsU <- CellSizes$value
    attr (CUHSAS, 'CellLimits') <- CellLimitsU
  }
  if (length (grep ("CPCASP_", namesCDF)) > 0) {
    nm <- namesCDF[grepl("^CPCASP_", namesCDF)]
    nms <- c(nms, 'CPCASP')
    CUHSAS <- ncvar_get (netCDFfile, nm)
    CellSizes <- ncatt_get (netCDFfile, nm, "CellSizes")
    CellLimitsP <- CellSizes$value
    attr (CUHSAS, 'CellLimits') <- CellLimitsP
  }
  if (length (grep ("C1DC_", namesCDF)) > 0) {
    nm <- namesCDF[grepl("^C1DC_", namesCDF)]
    nms <- c(nms, 'C1DC')
    C1DC <- ncvar_get (netCDFfile, nm)
    CellSizes <- ncatt_get (netCDFfile, nm, "CellSizes")
    CellLimits <- CellSizes$value
    attr (C1DC, 'CellLimits') <- CellLimits
  }
  fn <- sprintf ('%s%s/%s%s%02d.Rdata', DataDirectory (),
                 inp$Project, inp$Project, inp$typeFlight,
                 inp$Flight)
  size.distributions <- mget (nms)
  save (Data, size.distributions, file=fn)
  print (sprintf ('saved data.frame and size distributions to %s', fn))
}



seeManual <- function () {
  if (suppressWarnings(library(rstudio, logical.return=TRUE))) {
    rstudio::viewer (paste (path.package ('Ranadu'), 'RanaduShinyManual.pdf', sep='/'), height='maximize')
  }
}


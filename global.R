
## clear global environment that might be left from the last run
rm(list=ls(all=TRUE))

suppressMessages (
  library(shiny, quietly=TRUE, warn.conflicts=FALSE)
)
suppressMessages (suppressWarnings (
  library(Ranadu, quietly=TRUE, warn.conflicts=FALSE))
)
library(gtable)
library(grid)

source ('R/plotTrack.R')
## if this is set TRUE then messages will print in the console
## indicating which functions are entered, to trace the sequence
## of interactions when window entries are changed.
Trace <- FALSE
# Trace <- TRUE

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
ltyps <- c('solid', 'dashed', 'dotted', 'd-dot', 'lg dash') ## in order, line types 1:5
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

Project <- PJ[1]
Flight <- 1
Production <- FALSE
fn <- sprintf ('%s%s/%srf%02d.nc', DataDirectory (), Project, Project, Flight)
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
FI <<- DataFileInfo (fn)

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

saveConfig <- function (inp) {
  save (plotSpec, file=inp$save, ascii=TRUE)
}
loadConfig <- function (inp) {
  load (file=inp$restore)
  plotSpec <<- plotSpec
  print (sprintf ('loadConfig, file=%s', inp$restore))
}
load (file='plotSpec.def')  ## this loads initial values of plotSpec and Restrictions

chooseVar <- function (fname, inp) {
  sVarList <<- setVariableList (fname, sVarList)
}

sVarList <- c('ATX', 'DPXC', 'GGALT', 'WIC')

makeVarList <- function () {
  VarList <- 'GGALT'
  for (plt in 1:length(plotSpec$Plot)) {
    for (pnl in 1:plotSpec$Plot[[plt]]$panels) {
      VarList <- c(VarList, plotSpec$Plot[[plt]]$panel[[pnl]]$var)
    }
  }
  VarList <- c(VarList, c('LATC', 'LONC', 'WDC', 'WSC', 'ATX', 
                          'DPXC', 'TASX', 'ROLL', 'VSPD',
                          'THDG', 'SSLIP', sVarList))
  VarList <- unique (VarList)
  return (VarList)
}
VarList <- makeVarList()



Data <- getNetCDF (sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), plotSpec$Project, 
                            plotSpec$Project, plotSpec$TypeFlight, plotSpec$Flight), VarList)

# times <- c(Data$Time[1], Data$Time[nrow(Data)])
    step <- 60
    minT <- Data$Time[1]
    minT <- minT - as.integer (minT) %% step + step
    maxT <- Data$Time[nrow(Data)]
    maxT <- maxT - as.integer (maxT) %% step 
    times <- c(minT, maxT)

# Restrictions <- data.frame()
# Restrictions[1, 'RVAR'] <- 'TASX'
# Restrictions[1, 'apply'] <- TRUE
# Restrictions[1, 'min'] <- 130
# Restrictions[1, 'max'] <- 300
defFiles <- list.files(pattern = "^plotSpec")

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
    rstudio::viewer ('DataReviewManual.pdf', height='maximize')
  }
}


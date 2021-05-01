#' @title standardVariables
#' @description Standard set of variables, usually used in a call to getNetCDF 
#' @details Sets a standard list of variable names in VarList suitable for use in a call 
#' to getNetCDF. Optionally, add "list" to the variables.
#' The standard variables are ATX, DPXC, EWX, GGALT, LATC, LONC, MACHX, MR, PALT, 
#' PSXC, QCXC, TASX, WDC, WSC, WIC. For NCAR/RAF-produced aircraft-data files, these
#' variables are respectively the air temperature (degC), dew-point temperature (degC),
#' aircraft altitude (m), aircraft latitude (deg N), aircraft longitude (deg. E),
#' Mach Number, mixing ratio (g/kg), pressure altitude (m), ambient pressure (hPa),
#' dynamic pressure (hPa), true airspeed (m/s), wind direction (degrees relative to 
#' true north), wind speed (m/s), and vertical wind (m/s). For additional information
#' on these and other variables used in those data archives, see the document 
#' ProcessingAlgorithms.pdf with link in the github Wiki for this R package.
#' @aliases StandardVariables,standardVariables
#' @author William Cooper
#' @export standardVariables
#' @param list An optional list of variable names to add to the standard list
#' @param SRC An indicator of which institution's list should be used. Default
#' is 'NCAR'; other choices are 'UWYO' and 'FAAM'.
#' @return A character vector containing a standard set of variable names,
#' with additions as specified in "list". For NCAR the standard set is
#' ATX, DPXC, EWX, GGALT, LATC, LONC, MACHX, MR, PALT, PSXC, QCXC, TASX,
#' WDC, WSC, WIC.
#' @examples 
#' standardVariables (c("VEW", "PLWCC"))
standardVariables <- function (list=NULL, SRC='NCAR') {
  if (SRC == 'UWYO') {
    VarList <- c('trose', 'tdp', 'h2omx', 'GALT', 'LATC', 'LONC',
                 'PALT', 'ps_hads_a', 'tas', 'hwdir', 'hwmag', 'hw')
  } else if (SRC == 'FAAM') {
    VarList <- c('TTDI', 'DEWP', 'GALT', 'CLAT', 'CLNG', 'PHGT', 'SPR',
                 'PSP', 'TAS', 'LWC', 'TWC')
  } else {
    VarList <-c("ATX", "DPXC", "EWX", "GGALT", "LATC", "LONC", 
                "MACHX", "MR", "PALT", "PSXC", "QCXC", "TASX", 
                "WDC", "WSC", "WIC") 
  }
  if (length(list) > 0) {
    VarList <- c(VarList, list)
  }  
  return (VarList)
}

#' @title getNetCDF
#' @description Loads selected variables in a specified netCDF data file into a data.frame.
#' @details 'Time' is converted to a POSIXct variable, and other variables specified in 
#' VarList are included in the data.frame. By default, the entire file is loaded, but 
#' optional arguments Start and End can limit the time range. After reading the data, the 
#' netCDF file is closed before returning the data.frame to the calling program.
#' The global attributes in the netCDF file are loaded as attributes of the returned 
#' data.frame, and attributes of each requested variable are also assigned to that column 
#' in the data.frame from the variable attributes in the netCDF file. A 'label' attribute
#' is added where possible, constructed from the 'standard_name' and 'units' attributes,
#' and the label also contains the variable name. Some translations are applied to the
#' conventional standard_names and units; see "transl" below.
#' When working with attributes, it is a feature of R data.frames that subsetting loses 
#' all the assigned variable attributes. To preserve them, copy them via 
#' A <- attributes (Data$VAR), remove A$dim (e.g., A$dim <- NULL),
#' and re-assign via attributes (DataNew$VAR) <- A. The function does handle some
#' multi-dimensional variables (e.g., CCDP, CFSSP, the size distributions measured by the CDP
#' and FSSP); these are returned as data.frame variables that are columns in the returned
#' more general data.frame. They are two-dimensional, with the first dimension matching the
#' rows in the returned data.frame and the second having a length equal to the measured size
#' distribution (e.g., 30 for the CDP, 15 for the FSSP). The values are the number concentration
#' in each bin, and the variable is assigned an attribute "BinSize" that contains the mid-points
#' of the bins in the size distribution. The returned values are concentrations per bin (units
#' /cm^3) and, for BinSize, diameter in micrometers. The returned size distribution does not 
#' include the legacy first bin in the netCDF files; the first bin is the first measurement. 
#' The routine does work for 25-Hz files, for which it returns with fractional-second times.
#' @aliases getnetcdf GetNetCDF
#' @author William Cooper
#' @import ncdf4
#' @importFrom signal filter sgolay
#' @importFrom stats approx
#' @suggests magrittr
#' @export getNetCDF
#' @param fname string, full-path file name, e.g., "/scr/raf_data/PREDICT/PREDICTrf01.nc".
#' Also accepted are an OPENDAP URL. If used interactively, the default is to call the
#' function "setFileName()" to select the data file. When not interactive, the default
#' produces an error so "fname" must be supplied.
#' @param VarList vector of variable names to load from the netCDF file. Use "ALL" to load 
#' everything except vector variables like the size distributions. (This option may produce 
#' quite large data.frames.) The default is the list given by standardVariables (). 
#' SPECIAL NOTE: Some variable names
#' have a suffix indicating the location on the aircraft, like _LWI (left-wing inboard).
#' To avoid having to supply these, a partial name can be supplied, like "CONCD_", and
#' the routine will find the first matching variable and use that variable name. These
#' can always be overridden by providing the full name; this is just a convenience to
#' avoid having to look up where a particular measurement was installed in a given project.
#' @param Start An optional numeric giving the desired start time in HHMMSS format
#' @param End An optional numeric giving the desired end time in HHMMSS format
#' @param F An optional numeric entered in the data.frame as a column 'RF' all set to 
#' this integer. This may be useful when the resulting data.frame is combined with others, 
#' to have a variable distinguishing different flights.
#' @return data.frame containing the specified variables as columns, along with 'Time' 
#' and optionally the flight number 'RF'. The netCDF-file attributes and variable
#' attributes are assigned to the data.frame and columns, respectively.
#' @examples 
#' \dontrun{D <- getNetCDF ("PathToFile.nc", c("Var1", "Var2", "Var3"))}
#' \dontrun{D <- getNetCDF ("PathToFile.nc", c("Var1", "Var2"), 133000, 143000, 5)}
getNetCDF <- function (fname=setFileName(), VarList=standardVariables(), Start=0, End=0, F=0) {
  # This function reads the netCDF file 'fname' and extracts 
  # the variables specified in 'VarList', returning the
  # results in a data.frame. It includes the flight number F
  # in the data.frame, as variable RF. It converts "Time",
  # seconds after a reference time in the netCDF files, to
  # a POSIXct date/time variable.
  
  ## get the header information
  netCDFfile = nc_open (fname) 
  namesCDF <- names (netCDFfile$var)
  ## check source/institution:
  ATTG <- ncatt_get (netCDFfile, 0)   # get list of global attributes
  SOURCE <- 'NCAR'
  if ('Source' %in% names (ATTG) && grepl('Wyoming', ATTG$Source)) {
    SOURCE <- 'UWYO'
  }
  ## special section for FAAM data ##
  if ('source' %in% names (ATTG) && grepl('FAAM', ATTG$source)) {
    SOURCE <- 'FAAM'
  }
  FAAM <- ifelse (SOURCE == 'FAAM', TRUE, FALSE)
  UWYO <- ifelse (SOURCE == 'UWYO', TRUE, FALSE)
  if (FAAM) {
    ## get short names for variables instead of netCDF var name
    snames <- namesCDF
    for (VFAAM in namesCDF) {
      ATTV <- ncatt_get (netCDFfile, VFAAM)
      snames <- snames [-which (VFAAM == snames)] # remove and replace
      if ('short_name' %in% names(ATTV)) {
        snames[VFAAM] <- sub (' ', '', ATTV$short_name)
      } 
    }
  }
  if ("ALL" %in% VarList) {
    VarList <- names (netCDFfile$var)
    if (FAAM) {
      ## get short names for variables instead of netCDF var name
      snames <- namesCDF
      for (VFAAM in namesCDF) {
        ATTV <- ncatt_get (netCDFfile, VFAAM)
        snames <- snames [-which (VFAAM == snames)] # remove and replace
        if ('short_name' %in% names(ATTV)) {
          snames[VFAAM] <- sub (' ', '', ATTV$short_name)
        } 
      }
      VarList <- snames
    }
  }
  if ('Time' %in% VarList) { ## if "Time" is present, remove it
	                     ## (It will be added separately.)
    VarList <- VarList[-which(VarList == 'Time')]
  }

  ## check that requested variables are present in netCDF file; fail otherwise
  for (V in VarList) {
    if (is.na(V)) {next}
    if (FAAM) {
      if (length (which (grepl (V, snames)))) {next}
    } else if (length (which (grepl (V, namesCDF)))) {next}
    cat (sprintf ("requested variable %s not in netCDF file;\n ----> getNetCDF returning with error", V))
    return (-1)
  }
  if (UWYO) {
    Time <- ncvar_get (netCDFfile, 'time')
    time_units <- ncatt_get (netCDFfile, "time", "units")
  } else {
    Time <- ncvar_get (netCDFfile, "Time")
    time_units <- ncatt_get (netCDFfile, 'Time', 'units')
  }
  DL <- length (Time)
  ## set the maximum data rate (but not above 100 Hz):
  Rate <- 1
  nms <- names(netCDFfile$dim)
  if (!UWYO) {    ## only use Rate=1 for UWYO for now
    if ("sps25" %in% nms) {Rate <- 25}
    if ("sps50" %in% nms) {Rate <- 50}
    ## comment next line when LAMS 100-Hz vector present but no others
    # if ("sps100" %in% nms) {Rate <- 100}
  }
  # print (sprintf ("output rate for this data.frame is %d", Rate))
  # Expand Time to be high-rate if necessary
  if (Rate > 1) {
    T <- vector ("numeric", Rate*length(Time))
    for (i in 1:length(Time)) {
      for (j in 0:(Rate-1)) {
        T[(i-1)*Rate+j+1] <- Time[i] + j/Rate
      }  
    }
    Time <- T
  }
  # time_units <- ncatt_get (netCDFfile, "Time", "units")
  tref <- sub ('seconds since ', '', time_units$value)
  Time <- as.POSIXct (as.POSIXct (tref, tz='UTC')+Time, tz='UTC')
  # see if limited time range wanted:
  i1 <- ifelse ((Start != 0), getIndex (Time, Start), 1)
  if (i1 < 0) {i1 <- 1}
  i2 <- ifelse ((End != 0), getIndex (Time, End) + Rate - 1, length (Time))
  if (i2 < 1) {i2 <- length(Time)}
  # if (End != 0) {
  #   i2 <- getIndex (Time, End) + Rate - 1
  # } else {
  #   i2 <- length (Time)
  # }
  r <- i1:i2
  # r is the appropriate index for any rate, but also need
  # the 1-Hz and 5-Hz indices for interpolation:
  r1 <- ((i1-1)/Rate+1):((i2-1)/Rate+1)
  DL <- length (r1)
  Time <- Time[r]
  SE <- getStartEnd (Time)
  ## save 'Time' attributes:
  if (UWYO) {
    ATT <- ncatt_get (netCDFfile, "time")   # get list of Time attributes
  } else {
    ATT <- ncatt_get (netCDFfile, "Time")   # get list of Time attributes
  }
  for (A in names (ATT)) {
    attr(Time, A) <- ATT[[A]]
  }
  d <- data.frame(Time)
  ## save the dimensions, useful for archiving or re-writing to netCDF:-------------
  ##    but, to save space, omit the list of times
  nf <- netCDFfile
  if (UWYO) {
    nf$dim[1]$time$vals <- NULL
  } else {
    nf$dim[1]$Time$vals <- NULL
  }
  attr (d, "Dimensions") <- nf$dim
  ## Save all the global attributes in the netCDF file as 'd' attributes:----------
  ATT <- ncatt_get (netCDFfile, 0)   # get list of global attributes
  for (A in names (ATT)) {
    attr(d, A) <- ATT[[A]]
  }
  attr (d, "R_dataframe_created") <- date()    # add two global attributes
  attr (d, "Rate") <- Rate
  
  ######------------------------------------------------------------------
  IntFilter <- function (X, inRate, outRate) {
    if (inRate == outRate) {return (X)}
    ratio <- as.integer(outRate/inRate)    ## expected to be an integer
    ratio <- outRate / inRate              ## try 2.5 for CDP etc.
    ## beware of missing values
    z <- zoo::na.approx (as.vector(X), maxgap=1000, na.rm = FALSE)
    z[is.na(z)] <- 0
    x <- 0:(length(X)-1)
    A <- stats::approx (x, z, n=length(X)*ratio-ratio+1)
    T <- A$y
    SGL <- as.integer(ifelse (ratio %% 2, ratio, ratio+1))
    if (SGL <= 3) {SGL <- 5}
    # print (sprintf ('SGL=%f', SGL))
    T <- signal::filter(signal::sgolay(3,SGL),T)  # normally 75 pts
    # T <- signal::filter(signal::butter(3, 0.5), T)
    ## now shift to match outRate:
    ## The values are the average over the ensuing time period, so
    ## the interpolated values should be shifted forward 1/2 the
    ## ratio. This leaves constant values at start and end.
    ## Special Note re SRT: Variables like TASX end up at 1 Hz
    ## because ATX is sampled at 1 Hz. The result is that recorded
    ## values are apparently shifted 1.5 s later in time.
    n <- as.integer (ratio / 2)
    NL = length(T)
    T <- c(rep(T[1],n), T, rep(T[NL],ratio-n-1))  ## OK, even or odd ratio
    return (T)
  }
  ######------------------------------------------------------------------
  
  ## Add the requested variables:------------------------------------------------
  SizeDist <- function (V, netCDFfile, X) { ## used for size-distribution variables
    # print (sprintf ('SizeDist, V=%s', V))
    # print (str(X))
    CellSizes <- ncatt_get (netCDFfile, V, "CellSizes")
    if (CellSizes$hasatt == TRUE) { ## arrays like A1DC_ don't have CellSizes
      CellLimits <- CellSizes$value
      Bins <- length(CellLimits)-1
      ## For some reason there are 64 bin-limits for the PIP:
      if((grepl('^CPIP', V)) || (grepl('^APIP', V))) {
          Bins <- Bins - 1
      }
      BinSize <- vector('numeric', Bins)
      for (j in 1:Bins) {
        BinSize[j] <- (CellLimits[j] + CellLimits[j+1]) / 2    
      }
    } else { ## substitute from other attributes if available
             ## (but ACDP_ doesn't have appropriate attributes):
      if (grepl('^ACDP', V)) {
        Bins <- 30
        Resln <- 2  ## placeholder
      } else {
        Resln <- ncatt_get (netCDFfile, V, "Resolution")$value
        # Bins <- ncatt_get (netCDFfile, V, "nDiodes")$value 
        # if (Bins == 64) {Bins <- 63}
        Bins <- dim(X)[1] - 1
      }
      BinSize <- rep(Resln, Bins)
      CellLimits <- c(0, cumsum(BinSize))
    }
    ## handle higher-than-1 Rate:
    DM <- length(dim(X))
    if (DM == 3) {
      inputRate <- dim(X)[2]
      CC <- vector ('numeric', Bins*Rate*dim(X)[3])
      dim(CC) <- c(Bins, dim(X)[3] * Rate)
      for (j in 2:(Bins+1)) {
        Y <- IntFilter (X[j, , ], inputRate, Rate)
        CC[j-1,] <- Y
      }
      XN <- t(CC)
    } else if (DM == 2) {
      ## if this is 1-Hz altho HR file (e.g., UHSAS), need to interpolate to HR
      if (Rate > 1) {
        inputRate <- 1
        CC <- vector('numeric', Bins*Rate*dim(X)[2])
        dim(CC) <- c(Bins, Rate*dim(X)[2])
        for (j in 2:(Bins+1)) {
          Y <- IntFilter (X[j, ], inputRate, Rate)
          CC[j-1,] <- Y
        }
        XN <- t(CC)
      } else {
        CC <- X[2:(Bins+1),]
        XN <- t(CC)
      }
    } else {
      ## interpolate to Rate:
      inputRate <- dim(X)[2]
      CC <- vector('numeric', Bins*Rate*dim(X)[3])
      dim(CC) <- c(Bins, Rate*dim(X)[3])
      dim(X) <- c(Bins, dim(X)[2]*dim(X)[3])
      for (j in 2:(Bins+1)) {
	      Y <- IntFilter (X[j, ], inputRate, Rate)
	      CC[j-1,] <- Y
      }
      XN <- t(CC)
    }
    return(list(XN, BinSize, CellLimits))
  }
  
  #################### start of processing loop ############
  for (V in VarList) {
    if (is.na(V)) {next}
    if (FAAM) {
      SV <- names(snames[which(V == snames)])
    } 
    ## fill in location-tag for variable name if needed:
    if (substr(V, nchar(V), nchar(V)) == '_') {    ## must end in _
	    ## in case of multiple matches, must supply full names
      for (ncn in namesCDF) { 
        if (grepl (V, ncn)) {V <- ncn; break}   ## note, takes 1st match
      }
    }
    ## save dimensions for the variable:
    datt <- list()
    if (FAAM) {
      for (dd in netCDFfile$var[[SV]]$dim) {
        datt[[length(datt)+1]] <- dd$name
      } 
      X <- ncvar_get (netCDFfile, SV)
      ATT <- ncatt_get (netCDFfile, SV)
    } else {
      for (dd in netCDFfile$var[[V]]$dim) {
        datt[[length(datt)+1]] <- dd$name
      }    ## later, save datt as an attribute of V
      X <- ncvar_get (netCDFfile, V)
      ATT <- ncatt_get (netCDFfile, V)
      ## special treatment for CCDP, CS100, CUHSAS, C1DC, CS200:
      if (grepl ('CCDP_', V) || grepl('ACDP_', V)) {
        RL <- SizeDist(V, netCDFfile, X)
        X <- RL[[1]]
        CellLimitsD <- RL[[2]]
        BinSizeD <- RL[[3]]
        attr (X, 'CellLimits') <- CellLimitsD
        attr (X, 'BinSize') <- BinSizeD
      }
      if (grepl ('^C1DC_', V) || grepl('^A1DC_', V)) {
        RL <- SizeDist(V, netCDFfile, X)
        X <- RL[[1]]
        CellLimits2 <- RL[[2]]
        BinSize2 <- RL[[3]]
        attr (X, 'CellLimits') <- CellLimits2
        attr (X, 'BinSize') <- BinSize2
      }
      if (grepl ('CS200_', V) || grepl ('AS200_', V)) {
        RL <- SizeDist(V, netCDFfile, X)
        X <- RL[[1]]
        CellLimitsP <- RL[[2]]
        BinSizeP <- RL[[3]]
        attr (X, 'CellLimits') <- CellLimitsP
        attr (X, 'BinSize') <- BinSizeP
      }
      if (grepl ('CS100_', V) || grepl ('AS100_', V)) {
        RL <- SizeDist(V, netCDFfile, X)
        X <- RL[[1]]
        CellLimitsF <- RL[[2]]
        BinSizeF <- RL[[3]]
        attr (X, 'CellLimits') <- CellLimitsF
        attr (X, 'BinSize') <- BinSizeF
      }
      if (grepl ('CUHSAS_', V) || grepl ('AUHSAS_', V)) {
        RL <- SizeDist(V, netCDFfile, X)
        X <- RL[[1]]
        CellLimitsU <- RL[[2]]
        BinSizeU <- RL[[3]]
        attr (X, 'CellLimits') <- CellLimitsU
        attr (X, 'BinSize') <- BinSizeU
      }
      if (grepl ('^CPIP_', V) || grepl ('^APIP_', V)) {
        RL <- SizeDist(V, netCDFfile, X)
        X <- RL[[1]]
        CellLimitsP <- RL[[2]]
        BinSizeP <- RL[[3]]
        attr (X, 'CellLimits') <- CellLimitsP
        attr (X, 'BinSize') <- BinSizeP
      }
    }
    ## for Rate == 1, nothing special is needed:
    SDvar <- grepl('CCDP_', V) || grepl('CS100_', V) || grepl('CUHSAS_', V) ||
      grepl('^C1DC_', V) || grepl('CS200_', V) || grepl('^CPIP_', V)
    SDvar <- SDvar || grepl('ACDP_', V) || grepl('AS100_', V) || grepl('AUHSAS_', V) ||
      grepl('^A1DC_', V) || grepl('AS200_', V) || grepl('^APIP_', V)
    if (Rate == 1) {
      if (SDvar) {
        X <- X[r1, ]
      } else {
        X <- X[r1]
      }      
    } else { ## other rates require flattening and possibly interpolation and filtering
      if (SDvar) {
        XX <- X
        Bins <- length(RL[[2]])
        dim(XX) <- c(Rate, dim(X)[1]/Rate, Bins)
        XX <- XX[,r1,]
        dim(XX) <- c(dim(XX)[1]*dim(XX)[2], Bins)
        X <- XX
      } else {
        DM <- length(dim(X))  
        # print (sprintf ('V=%s DM=%d', V, DM))
        if (DM == 2) {    # flatten
          X <- X[,r1]
          inputRate <- dim(X)[1]
          needFilter <- ifelse ((dim(X)[1] != Rate), TRUE, FALSE)
          dim(X) <- dim(X)[1]*dim(X)[2]
          ## see if adjustment to max rate is needed
          # print (sprintf ('needFilter=%s, inputRate=%d, Rate=%d', needFilter, inputRate, Rate))
          if (needFilter) {X <- IntFilter(X, inputRate, Rate)}
        } else {  ## single-dimension (1 Hz) in high-rate file
          X <- X[r1]
          X <- IntFilter (X, 1, Rate)
        }
      }
    } 
    ## add variable attributes as in netCDF file
    for (A in names (ATT)) {
      attr (X, A) <- ATT[[A]]
    }
    attr (X, "Dimensions") <- datt
    # if (grepl('CCDP_', V)) {
      # d$CCDP <- X
    # } else if (grepl('CSP100_', V)) {
      # d$CSP100 <- X
    # } else if (grepl('CUHSAS_', V)) {
      # d$CUHSAS <- X
    # } else if (grepl('^C1DC_', V)) {
      # d$C1DC <- X
    # } else if (grepl('CS200', V)) {
      # d$CS200 <- X
    # } else {
      # print (sprintf ('V=%s, dim=%f', V, dim(X)))
      # print (str(X))
      d[V] <- X
    # }
  }
  if (F != 0) {    # if specified, include the flight number
    RF <- rep (F, times=length(Time))    # label flight number
    d["RF"] <- RF
  }
  nc_close (netCDFfile)
  d <- addLabels(d)
  return (d)
}

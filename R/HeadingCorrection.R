#' @title CorrectHeading
#' @description Calculate an estimate of the sequence of errors in heading.
#' @details For a data.frame supplied with variables representing the attitude angles,
#' body accelerations, GPS-measured ground velocities, and altitude and latitude,
#' estimate the error in the heading via comparison of the measured body accelerations,
#' transformed to the Earth-relative frame, to the accelerations determined via 
#' differentiation of the GPS-measured ground-speed components. Find the correction
#' to heading that provides the best match between these two sources of acceleration.
#' The result is the estimated error, so this should be **subtracted** from the 
#' measured value to get the corrected value. Results are sensitive to timing
#' delays among the variables, so these should be corrected prior to calling this routine.
#' In addition, if there is a variable "Valid" in the data.frame, it should be logical
#' and will be used to restrict the calculation of the corrections to the subset where
#' Valid is TRUE. This can be used, for example, to exclude slow-flight regions where
#' flaps might be deployed, high-rate-of-climb regions, etc.
#' @author William Cooper
#' @export CorrectHeading
#' @import zoo
#' @importFrom signal sgolayfilt
#' @param .data A data.frame containing measurements of body accelerations (called
#' BLATA, BLONGA, BNORMA), ground-speed components from GPS (GGVEW, GGVNS), attitude
#' angles in units of degrees (PITCH, ROLL, THDG, the latter the heading relative to
#' true north), and the latitude and altitude (LAT and GGALT), the latter two for
#' finding the local acceleration of gravity. If variables named PITCHC, ROLLC, and
#' LATC are present those will be used instead. The first two might be produced by
#' prior use of the CorrectPitch() function.
#' @param .span The number of points (default 21) used for estimating the accelerations
#' by differentiation and for smoothing the results. 
#' @param .default The default value to use if the routine does not find enough
#' qualified turns to develop a valid estimate of the heading error. The default
#' is -0.08 deg., as applies to the GV data for DEEPWAVE.
#' @param .Valid An optional logical vector of length matching the rows in .data,
#' which should be TRUE for qualified measurements and FALSE for measurements to
#' exclude from the calculation. This can be used to exclude low-airspeed cases
#' (where flaps might be deployed) or cases of rapid climbs, for example. The default
#' is NULL, in which case the test will be skipped.
#' @param .plotfile The name of a plot to generate (e.g., "./Plot1.pdf") that shows
#' the results from the algorithm.
#' @return A vector of the same length as the supplied data.frame that gives an
#' estimate of the error in heading, the negative of the correction needed.
#' @examples 
#' HeadingCorrection <- CorrectHeading (RAFdata)
CorrectHeading <- function (.data, .span=21, .default=-0.08, .Valid=NULL, .plotfile=NULL) {
  ## note: before calling, should apply timing corrections and pitch/roll corrections
  ##       if desired.
  GeneratePlot <- !is.null (.plotfile) 
  Cradeg <- pi/180
  ## The next correction calculates the correction needed to account for the rotation
  ## of the Earth and of the l-frame (ENU frame). See Noureldin et al., 2013, 
  ## Eqs. 5.55--5.57. Subtract this from the transformed accelerations before using them.
  RotationCorrection <- function (.data, .V) {
    Cradeg <- pi/180
    omegaE <- StandardConstant ('Omega')  ## Earth'r angular velocity
    Re <- StandardConstant ('Re')         ## representative Earth radius, m
    DL <- nrow (.data)
    C <- vector ('numeric', 3*DL); dim(C) <- c(DL,3)
    lat <- .data$LAT * Cradeg
    sinLat <- sin(lat); cosLat <- cos(lat); tanLat <- tan(lat)
    M12 <- -2*omegaE*sinLat-.V[,1]*tanLat/Re
    M13 <- 2*omegaE*cosLat+.V[,1]/Re
    M21 <- 2*omegaE*sinLat+.V[,1]*tanLat/Re
    M23 <- .V[,2]/Re
    M31 <- -2*omegaE*cosLat-.V[,1]/Re
    M32 <- -.V[,2]/Re
    C[,1] <- M12*.V[,2]+M13*.V[,3]
    C[,2] <- M21*.V[,1]+M23*.V[,3]
    C[,3] <- M31*.V[,1]+M32*.V[,2]
    return (C)
  }
  ## check for required variables:
  Required <- c("BLATA", "BLONGA", "BNORMA", "GGVNS", "GGVEW", "GGALT",
                "THDG", "PITCH", "ROLL")
  .names <- names(.data)
  for (.R in Required) {
    if (.R %in% .names) {next}
    print (sprintf ("in CorrectHeading, required variable %s not found; returning 0", .R))
    return(0)
  }
  if (!("LATC" %in% .names) && !("LAT" %in% .names)) {
    print (sprintf ("in CorrectHeading, required variable LAT or LATC not found; returning 0"))
    return(0)
  }
  
  ## get the data rate -- only works with rates of 1 or 25 at present
  Rate <- 1  
  if ((.data$Time[2]-.data$Time[1]) <= 0.04) {Rate <- 25} 
  LD <- nrow(.data)
  D <- .data[!is.na (.data$Time), ]  ## eliminate bad-time records
  
  if (Rate > 1) {
    D1 <- D[(as.numeric(D$Time) %% 1) < 0.01,]  ## extract a 1-Hz data.frame
  } else {
    D1 <- D
  }
  
  ## substitute corrected values if present
  if ("PITCHC" %in% names (D1)) {D1$PITCH <- D1$PITCHC}
  if ("ROLLC" %in% names (D1)) {D1$ROLL <- D1$ROLLC}
  if ("LATC" %in% names (D1)) {D1$LAT <- D1$LATC}
  
  #interpolate if necessary:
  MaxGap <- 1000
  ggvns <- zoo::na.approx (as.vector(D1$GGVNS), maxgap=MaxGap, na.rm = FALSE)
  ggvew <- zoo::na.approx (as.vector(D1$GGVEW), maxgap=MaxGap, na.rm = FALSE)
  D1$BLONGA <- zoo::na.approx (as.vector (D1$BLONGA), maxgap=MaxGap, na.rm=FALSE)
  D1$BLATA <- zoo::na.approx (as.vector (D1$BLATA), maxgap=MaxGap, na.rm=FALSE)
  D1$BNORMA <- zoo::na.approx (as.vector (D1$BNORMA), maxgap=MaxGap, na.rm=FALSE)
  D1$GGALT <- zoo::na.approx (as.vector (D1$GGALT), maxgap=MaxGap, na.rm=FALSE)
  D1$PITCH <- zoo::na.approx (as.vector (D1$PITCH), maxgap=MaxGap, na.rm=FALSE)
  D1$ROLL <- zoo::na.approx (as.vector (D1$ROLL), maxgap=MaxGap, na.rm=FALSE)
  D1$THDG <- zoo::na.approx (as.vector (D1$THDG), maxgap=MaxGap, na.rm=FALSE)
  D1$LAT <- zoo::na.approx (as.vector (D1$LAT), maxgap=MaxGap, na.rm=FALSE)
  vndot <- signal::sgolayfilt (ggvns, 3, .span, m=1)  # m=1 for first deriv.
  vedot <- signal::sgolayfilt (ggvew, 3, .span, m=1)
  G <- Gravity (D1$LAT, D1$GGALT)
  AB <- matrix(c(D1$BLONGA, D1$BLATA, D1$BNORMA+G), ncol=3) #aircraft-frame
  VL <- matrix(c(D1$VNS, D1$VEW, D1$VSPD), ncol=3) 
  AL <- XformLA (D1, AB)                                    #l-frame
  ## now corrected for angular effects
  ## See Noureldin et al, 2013, Eq. (5.55)
  AL <- AL + RotationCorrection (D1, VL)
  
  ## the resulting l-frame accelerations
  D1$LACCX <- AL[, 1]
  D1$LACCY <- AL[, 2]
  D1$LACCZ <- AL[, 3] + G
  
  ## smooth to match GPS-velocity derivatives
  D1$LACCX <- signal::sgolayfilt (D1$LACCX, 3, .span, m=0)
  D1$LACCY <- signal::sgolayfilt (D1$LACCY, 3, .span, m=0)
  D1$LACCZ <- signal::sgolayfilt (D1$LACCZ, 3, .span, m=0)
  
  ## magnitude of horizontal acceleration
  A2 <- D1$LACCX^2 + D1$LACCY^2
  A <- sqrt(A2)
  D1$herr <- (-D1$LACCY*(vedot-D1$LACCX)+D1$LACCX*(vndot-D1$LACCY)) / (Cradeg*A2)
  
  ## a working data.frame, to avoid changing D1
  DT <- D1
  v <- (A > 1) & (abs (DT$herr) < 0.3)
  if (!is.null (.Valid)) {
    v <- v & .Valid
  }
  DT[!v,] <- NA
  DT$TestR <- (!is.na(DT$Time) & (DT$ROLL > 10))     ## right-turn cases
  DT$TestL <- (!is.na(DT$Time) & (DT$ROLL < -10))    ## left-turn cases
  ## create a new data.frame to hold measurements from a sequence of measurements
  ## in turns
  DSET <- data.frame ()
  hmeanR <- vector("numeric")
  hsdR <- vector("numeric")
  tbarR <- vector("numeric")
  hmeanL <- vector("numeric")
  hsdL <- vector("numeric")
  tbarL <- vector("numeric")
  
  setStart <- 0
  NRA <- 40    ## effective averaging seconds for error-bar std. dev.
  NSL <- 25    ## required seconds turn in each direction
  TGAP <- 300  ## seconds constituting break in sets of turn values
  for (k in 1:nrow(DT)) {
    if (DT$TestR[k] || DT$TestL[k]) {
      if (setStart == 0) {
        setStart <- k
        setEnd <- k
        DSET <- rbind(DSET, DT[k,])
      } else if (as.numeric(difftime(DT$Time[k], DT$Time[setEnd], units='sec')) > TGAP) {
        if (length(DSET[DSET$TestR == TRUE, "herr"]) > NSL &&
            length(DSET[DSET$TestL == TRUE, "herr"]) > NSL) {      
          hmnR <- mean (DSET$herr[DSET$TestR], na.rm=TRUE)
          hmnL <- mean (DSET$herr[DSET$TestL], na.rm=TRUE)
          hsdevR <- sd (DSET$herr[DSET$TestR], na.rm=TRUE) / 
            sqrt (length (DSET$TestR)/NRA)
          hsdevL <- sd (DSET$herr[DSET$TestL], na.rm=TRUE) / 
            sqrt (length (DSET$TestL)/NRA)
          tbrR <- mean(DSET$Time[DSET$TestR], na.rm=TRUE)
          tbrL <- mean(DSET$Time[DSET$TestL], na.rm=TRUE)
          # print (sprintf ("R time %s count %d", as.POSIXct (tbrR, origin="2014-07-04", tz="GMT"), length(DSET$TestR)))
          hmeanR <- c(hmeanR, hmnR)
          hsdR   <- c(hsdR, hsdevR)
          tbarR  <- c(tbarR, tbrR)
          # print (sprintf ("L time %s count %d", as.POSIXct (tbrL, origin="2014-07-04", tz="GMT"), length(DSET$TestL)))
          hmeanL <- c(hmeanL, hmnL)
          hsdL   <- c(hsdL, hsdevL)
          tbarL  <- c(tbarL, tbrL)
        }
        setStart <- k
        setEnd <- k
        DSET <- DT[k,]
      } else {
        setEnd <- k
        DSET <- rbind(DSET, DT[k,])
      }
    }
  }
  if (length(DSET[DSET$TestR == TRUE, "herr"]) > NSL &&
      length(DSET[DSET$TestL == TRUE, "herr"]) > NSL) { 
    hmnR <- mean (DSET$herr[DSET$TestR], na.rm=TRUE)
    hmnL <- mean (DSET$herr[DSET$TestL], na.rm=TRUE)
    hsdevR <- sd (DSET$herr[DSET$TestR], na.rm=TRUE) / sqrt (length (DSET$TestR)/NRA)
    hsdevL <- sd (DSET$herr[DSET$TestL], na.rm=TRUE) / sqrt (length (DSET$TestL)/NRA)
    tbrR <- mean(DSET$Time[DSET$TestR], na.rm=TRUE)
    tbrL <- mean(DSET$Time[DSET$TestL], na.rm=TRUE)
    hmeanR <- c(hmeanR, hmnR)
    hsdR   <- c(hsdR, hsdevR)
    tbarR  <- c(tbarR, tbrR)
    hmeanL <- c(hmeanL, hmnL)
    hsdL   <- c(hsdL, hsdevL)
    tbarL  <- c(tbarL, tbrL)
  }
  if (length (tbarL) < 3) {
    print (sprintf ("CorrectHeading found too few qualifying turns (%d); returning default (%.2f)", length (tbarL), .default))
    return (c(rep(.default, LD)))
  }

  ## construct data.frame holding results from each qualifying turn
  EH <- data.frame(tbar=c(tbarL, tbarR), hmeanR=c(rep(NA, length(tbarL)), hmeanR),
                   hmeanL=c(hmeanL, rep(NA, length(tbarR))), 
                   hsdR=c(rep(NA, length(tbarL)), hsdR), 
                   hsdL=c(hsdL, rep(NA, length(tbarR))))
  EH$tbar <- as.POSIXct (EH$tbar, origin="1970-01-01", tz="GMT")
  ## whmean and whsd are not used; left here for reference
  whmean <- weighted.mean (c(EH$hmeanR, EH$hmeanL), 
                           c(1/EH$hsdR^2, 1/EH$hsdL^2), na.rm=TRUE)
  whsd <- sqrt (1/sum(c(1/EH$hsdR^2, 1/EH$hsdL^2), na.rm=TRUE))
  if (GeneratePlot) {
    p <- ggplot(EH, aes(x="tbar"), na.rm=TRUE)
    p <- p + geom_errorbar(aes(ymin=hmeanR-hsdR, ymax=hmeanR+hsdR, colour="right"),
                           width=600, size=1.5, na.rm=TRUE) + ylim (-0.25,0.15)
    p <- p + geom_point (aes(y = hmeanR, colour="right"),size=3.5, na.rm=TRUE)
    p <- p + geom_errorbar(aes(ymin=hmeanL-hsdL, ymax=hmeanL+hsdL, colour="left"),
                           width=600, size=1.5, na.rm=TRUE)
    p <- p + geom_point (aes(y = hmeanL, colour="left"), size=3.5, na.rm=TRUE)
    p <- p + ylab(expression(paste(delta,psi,' [',degree,']')))
    p <- p + xlab ("Time [UTC]")
  }
  yss <- c(EH$hmeanL[!is.na(EH$hmeanL)], EH$hmeanR[!is.na(EH$hmeanR)])
  ywts <- 1/c(EH$hsdL[!is.na(EH$hsdL)], EH$hsdR[!is.na(EH$hsdR)])^2
  SS <- smooth.spline(EH$tbar, yss, w=ywts, df=length(yss)-1, spar=0.7)
  if (GeneratePlot) {
    D1$HC <- predict(SS, as.numeric(D1$Time))$y
    p <- p + geom_line (data=D1, aes (x="Time", y=HC, colour="spline"), lwd=2, na.rm=TRUE)
    # SS2 <- smooth.spline(EH$tbar, yss, df=8, spar=0.4)
    # xss2 <- as.POSIXct (SS2$x, origin="1970-01-01", tz="GMT")
    # p <- p + geom_line(aes(x=xss2, y=SS2$y, colour="spline"), lwd=2, lty=2, na.rm=TRUE)
    cols <- c("right"="blue", "left"="darkgreen", "spline"="red")
    p <- p + scale_colour_manual("turn direction:", values=cols)
    p <- p + guides(color=guide_legend(override.aes=list(shape=c(16,16,NA), 
                                                         linetype=c(0,0,1))))
    p <- p + ggtitle (sprintf ("%d turns, wtd mean and std %.2f %.2f", length (tbarL), whmean, whsd))
    p <- p + theme_WAC()
    suppressMessages(ggsave (.plotfile, p))
  }
  
  ## construct the input-rate heading correction HC
  HC <- predict(SS, as.numeric(D$Time))$y
  return (HC)
}

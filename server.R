require (shiny)

shinyServer(function(input, output, session) {
  
  
  ################ OBSERVERS ########################
  
  observe ({  ## change plot number or number of panels
    input$specRead
    updateNumericInput (session, 'panels', value=plotSpec[[input$plot]]$panels)
  }, priority=100)
  observe ({
    plotSpec[[input$plot]]$panels <<- input$panels 
    plotSpec[[input$plot]]$columns <<- input$cols
  })
  observe ({  ## addVarP
    if (Trace) {print (sprintf ('entered addVarP observer, var=%s', input$addVarP))}
#     if (is.null(input$addVarP) || length (input$addVarP) < 2) {return ()}
#     if (is.null(input$varColor) || length (input$varColor) < 2) {return ()}
    npt <- isolate (input$plot)
    npl <- isolate (input$panel)
    if (Trace) {print (sprintf ('lineV is %d', input$lineV))}
    if (input$lineV > length (plotSpec[[npt]]$panel[[npl]]$var)) {
      v <- plotSpec[[npt]]$panel[[npl]]$var
      v <- c(v, v[length(v)])
      plotSpec[[npt]]$panel[[npl]]$var <<- v
      if (Trace) {print (c('var set to ', v))}
      lbl <- plotSpec[[npt]]$panel[[npl]]$lab
      lbl <- c(lbl, lbl[length(lbl)])
      plotSpec[[npt]]$panel[[npl]]$lab <<- lbl
      cl <- plotSpec[[npt]]$panel[[npl]]$col
      cl <- c(cl, cl[length(cl)])
      plotSpec[[npt]]$panel[[npl]]$col <<- cl
      lw <- plotSpec[[npt]]$panel[[npl]]$lw
      lw <- c(lw, lw[length(lw)])
      plotSpec[[npt]]$panel[[npl]]$lw <<- lw
      lt <- plotSpec[[npt]]$panel[[npl]]$lt
      lt <- c(lt, lt[length(lt)])
      plotSpec[[npt]]$panel[[npl]]$lt <<- lt
      updateSelectInput (session, 'addVarP', selected=v[length(v)])
      updateSelectInput (session, 'varColor', selected=cl[length(cl)])
      updateNumericInput (session, 'lineW', value=lw[length(lw)])
      updateNumericInput (session, 'lineStyle', value=lt[length(lt)])
    }
    if (input$addVarP != 'omit') {
      if (input$addVarP == 'select') {
        reac$newdisplay <- TRUE
      } else {
        plotSpec[[input$plot]]$panel[[input$panel]]$var[input$lineV] <<- input$addVarP
        reac$newdisplay <- TRUE
        reac$newspec <- TRUE
        if ((length(data ()) < 2) || (!(input$addVarP %in% names (data ())))) {
          print ('need new data')
          reac$newdata <- TRUE
        }
      }
    } else {
      v <- plotSpec[[input$plot]]$panel[[input$panel]]$var
      v <- v[-input$lineV]
      if (Trace) {print (sprintf ('new var list is %s', v))}
      plotSpec[[input$plot]]$panel[[input$panel]]$var <<- v
      reac$newdisplay <- TRUE
      reac$newspec <- TRUE
      nms <- names (data ())  ## just a data ref to get reset
      updateSelectInput (session, 'addVarP', selected='select')  
    }
  })
  
  observe ({
    l <- input$lineV
    if (l <= length (plotSpec[[input$plot]]$panel[[input$panel]]$var)) {
      v <- plotSpec[[input$plot]]$panel[[input$panel]]$var[l]
      updateSelectInput (session, 'addVarP', selected=v)
      updateSelectInput (session, 'varColor', selected=
                           plotSpec[[input$plot]]$panel[[input$panel]]$col[l])
      updateNumericInput (session, 'lineW', value=
                            plotSpec[[input$plot]]$panel[[input$panel]]$lw[l])
      updateRadioButtons (session, 'lineStyle', selected=
                            plotSpec[[input$plot]]$panel[[input$panel]]$lt[l])
      
    }
  })
  
  observe ({
    l <- input$lineV
    plotSpec[[input$plot]]$panel[[input$panel]]$col[l] <<- input$varColor
    plotSpec[[input$plot]]$panel[[input$panel]]$lw[l] <<- input$lineW
    plotSpec[[input$plot]]$panel[[input$panel]]$lt[l] <<- input$lineStyle
    reac$newdisplay <- TRUE
    reac$newspec <- TRUE
  })
  
  observe ({
    plotSpec[[input$plot]]$panel[[input$panel]]$logY <<- input$logY
    reac$newdisplay <- TRUE
    reac$newspec <- TRUE
  })
  
  observe ({ 
    input$specRead
    reac$newdisplay <- TRUE
    reac$newspec <- TRUE
  })
  
  observe ({
    if (input$rvNumber > nrow (Restrictions)) {
      newRow <- data.frame (RVAR=isolate (input$rvar), 
                            apply=isolate (input$apply),
                            min=isolate (input$rmin),
                            max=isolate (input$rmax))
      Restrictions <<- rbind (Restrictions, newRow)
    } else {
      updateSelectInput(session, 'rvar',  
                        selected=Restrictions$RVAR[input$rvNumber])
      updateCheckboxInput (session, 'apply',
                           value=Restrictions$apply[input$rvNumber])
      updateNumericInput(session, 'rmin', label=NULL, 
                         value=Restrictions$min[input$rvNumber])
      updateNumericInput(session, 'rmax', label=NULL, 
                         value=Restrictions$max[input$rvNumber])
    }
    reac$newdisplay <- TRUE
    reac$newspec <- TRUE
  })
  
  observe ({
    Restrictions$RVAR[input$rvNumber] <<- input$rvar
    reac$newdisplay <- TRUE
    reac$newspec <- TRUE
  })
  observe ({
    Restrictions$apply[input$rvNumber] <<- input$apply
    reac$newdisplay <- TRUE
    reac$newspec <- TRUE
  })
  observe ({
    Restrictions$min[input$rvNumber] <<- input$rmin
    reac$newdisplay <- TRUE
    reac$newspec <- TRUE
  })
  observe ({
    Restrictions$max[input$rvNumber] <<- input$rmax
    reac$newdisplay <- TRUE
    reac$newspec <- TRUE
  })
  observeEvent (input$specSave, saveConfig (input))
  observeEvent (input$specRead, 
                {loadConfig (input)
                  updateNumericInput (session, 'panels', value=plotSpec[[input$plot]]$panels)
                  updateNumericInput (session, 'cols', value=plotSpec[[input$plot]]$columns)
                } )
  observeEvent (input$savePDF,
                savePDF (Data=data(), inp=input))
  observeEvent (input$savePNG,
                savePNG (Data=data(), inp=input))
  observeEvent (input$saveRdata,
                saveRdata (Data=data(), inp=input))
  observeEvent (input$selectVariables, 
                chooseVar (fname, inp=input))
  observeEvent (input$ncplot, OpenInProgram (data(), warnOverwrite=FALSE))
  observeEvent (input$Xanadu, OpenInProgram (data(), 'Xanadu', warnOverwrite=FALSE))
  observeEvent (input$maneuvers, SeekManeuvers (data ()))
  observeEvent (input$manual, seeManual ())
  
  observe ({                              ## typeFlight
    if (Trace) {print (sprintf ('entered typeFlight observer with value %s', input$typeFlight))}
    typeFlight <<- input$typeFlight
    reac$newdata <- TRUE
  })
  
  
  
  observe ({                             ## time
    if (Trace) {print ('setting time')}
    if (Trace) {print (sprintf ('Project and Flight: %s %s%02d',
                                isolate(input$Project),
                                isolate (input$typeFlight),
                                isolate (input$Flight)))}
    #     reac$newdisplay
    #     reac$newdisplay <- TRUE
    Data <- data ()
    if (length (Data) < 2) {
      reac$newdata <- TRUE
      if (Trace) {print ('error, need data first')}
      return ()
    }
    step <- 60
    minT <- Data$Time[1]
    minT <- minT - as.integer (minT) %% step
    maxT <- Data$Time[nrow(Data)]
    maxT <- maxT - as.integer (maxT) %% step + step
    if (Trace) {print (sprintf ('slider values %s %s', formatTime (minT),
                                formatTime (maxT)))}
    updateSliderInput(session, inputId='times', label=NULL,
                      value=c(minT, maxT),
                      min=minT, max=maxT)
    times <<- c(minT, maxT)
  }, priority=0)
  
  
  observe ({                          ## global time
    if (Trace) {
      print ('entering global-time observer')
      print (input$times)
    }
    reac$newdata
    reac$newdisplay
    times <<- input$times
  })
  
  ################ REACTIVES ########################
  
  reac <- reactiveValues (newdata=FALSE, newdisplay=FALSE, newspec=TRUE)
  
  flightType <- reactive ({              ## typeFlight
    ## reset typeFlight to rf
    updateRadioButtons (session, 'typeFlight', label=NULL, selected='rf')
    'rf'
  })
  
  data <- reactive({                     ## data
    if (Trace) {
      print ('entered data')
      print (reac$newdata)
    }
    # Project <<- Project <- input$Project
    reac$newdata
    reac$newdata <- FALSE
    ## these would be needed for translation to new cal coefficients
    ## VarList <- c(VarList, "RTH1", "RTH2", "RTF1")
    VarList <- 'GGALT'
    for (plt in 1:length (plotSpec)) {
      for (pnl in 1:plotSpec[[plt]]$panels) {
        VarList <- c(VarList, plotSpec[[plt]]$panel[[pnl]]$var)
      }
    }
    VarList <- c(VarList, c('LATC', 'LONC', 'WDC', 'WSC', 'ATX', 'DPXC', 'TASX', 'ROLL', 'VSPD'))
    VarList <<- VarList <- unique (VarList)
    if (grepl ('HIPPO', input$Project)) {
      fname <<- sprintf ('%sHIPPO/%s%s%02d.nc', DataDirectory (), input$Project,
                         typeFlight, input$Flight)
    } else {
      fname <<- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), input$Project,
                         input$Project, typeFlight, input$Flight)
    }
    #     if (input$Production) {
    #       print (sprintf ('Production section, input$Production=%d', input$Production))
    #       dr <- sprintf ('%s../raf/Prod_Data/%s', DataDirectory (), Project)
    #       scmd <- sprintf ('ls -lt `/bin/find %s -ipath "\\./movies" -prune -o -ipath "\\./*image*" -prune -o -name %s%s%02d.nc`',
    #                        dr, Project, input$typeFlight, input$Flight)
    #       fl <- system (scmd, intern=TRUE)[1]
    #       if ((length (fl) > 0) && (!grepl ('total', fl))) {
    #         fname <- sub ('.* /', '/', fl[1])
    #       }
    #       scmd <- sub ('\\.nc', '.Rdata', scmd)
    #       fl <- system (scmd, intern=TRUE)[1]
    #       if ((length (fl) > 0) && (!grepl ('total', fl))) {
    #         fname <- sub ('.* /', '/', fl[1])
    #       }
    #     }
    if (Trace) {print (sprintf ('in data, fname=%s', fname))}
    reac$newdisplay <- TRUE
    if (file.exists(fname)) {
      D <- getNetCDF (fname, VarList)
      if (length (D) > 1) {
        fname.last <<- fname
        return (D)
      } else {
        print (sprintf ('fname=%s', fname))
        print (VarList)
        ## stopping to prevent looping
        stop ('variable not found; stopping to avoid looping')
      }
    } else {
      warning (sprintf ('the file %s does not exist', fname))
      fnRdata <- sub ('\\.nc', '.Rdata', fname)
      if (file.exists (fnRdata)) {
        warning ('using Rdata file instead')
        fl <- load (file=fnRdata)
        FI <<- DataFileInfo (fnRdata)
        # loadVRPlot (Project, Production=FALSE, input$Flight, psq)
        fname.last <<- fname
        # print (sprintf ('data returned with dimensions %d', dim(Data)))
        return (Data)
      }
      ## try tf01
      fn <- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), input$Project,
                     input$Project, 'tf', input$Flight)
      if (file.exists (fn)) {
        warning (sprintf ('switched to tf%02d because rf%02d does not exist',
                          input$Flight, input$Flight))
        updateRadioButtons (session, 'typeFlight', label=NULL, selected='tf')
        typeFlight <<- 'tf'
        return (getNetCDF (fn, VarList))
      } else {
        if (Trace) {print ('error in data, returning -1')}
        return (-1)
      }
    }
  })
  
  
  ################ OUTPUTS ########################
  
  
  
  output$display <- renderPlot ({  ## display
    # input$typeFlight
    reac$newspec
    if (is.null(input$times[1])) {
      if (Trace) {print ('in display but input time is NULL')}
      return ()
    }
    if (Trace) {
      print ('display entry, reac$newdisplay is:')
      print (reac$newdisplay)
    }
    input$PlotVar
    Project <- input$Project
    if (reac$newdisplay) {
      # VRPlot <- VRP ()
      if (Trace) {print ('entered display')}
      # VRPlot <<- VRPlot
      Data <- data()
      if (length (Data) < 2) {
        warning (sprintf ('variable error in (%s)', fname))
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
        return ()
      }
      
      if (Trace) {
        print (sprintf ('input$times %s %s', formatTime (input$times[1]),
                        formatTime (input$times[2])))
        print (sprintf ('global times are %s %s',
                        formatTime (times[1]), formatTime (times[2])))
      }
      namesV <- names(Data)
      namesV <- namesV[namesV != "Time"]
      for (n in namesV) {
        Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
      }
      # Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
      if (nrow (Data) <= 1) {
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
        reac$newdisplay <- TRUE
        reac#newdata <- TRUE
        return()
      }
      ## see global.R functions:
      DataV <- limitData (Data, input, input$limits)
      ndv <- names (DataV)
      ## guard against inf. VCSEL limits
      if (('DP_VXL' %in% ndv) && all(is.na(DataV$DP_VXL))) {
        DataV$DP_VXL <- rep(0, nrow(DataV))
      }
      if (('DP_DPR' %in% ndv) && all(is.na(DataV$DP_DPR))) {
        DataV$DP_DPR <- rep(0, nrow(DataV))
      }
      if (('DP_DPL' %in% ndv) && all(is.na(DataV$DP_DPL))) {
        DataV$DP_DPL <- rep(0, nrow(DataV))
      }
      DataV$DPXC[!is.na(DataV$DPXC) & (DataV$DPXC < -1000)] <- NA
      
      SE <- getStartEnd (Data$Time)
      i <- getIndex (Data$Time, SE[1])
      FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, input$typeFlight,
                        input$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                        strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                        strftime(Data$Time[getIndex(Data$Time,SE[2])],
                                 format="%H:%M:%S", tz='UTC'))
      FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
      AddFooter <<- function() {
        CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
        mtext(paste(FigFooter,'generated by Ranadu plot ', input$plot,
                    FigDatestr),1,outer=T,cex=0.75)
      }
      
      #       if (input$limits) {
      #         DataV <<- DataV
      #         eval(parse(text=sprintf("RPlot%d(DataV, plotSpec1)",
      #                                 input$plot)))
      #       } else {
      #         Data <<- Data
      #         eval(parse(text=sprintf("RPlot%d(Data, plotSpec1)",
      #                                 input$plot)))
      #       }
      #       d <- ifelse (input$limits, DataV, Data)
      #       print (dim(d))
      #       print(str(d))
      spec <- plotSpec[[input$plot]]
      nrws <- ceiling (input$panels / input$cols)
      nmx <- nrws * input$cols
      layout(matrix(1:nmx, ncol = input$cols), widths = 1, 
             heights = c(rep(5,spec$panels-1),6))
      op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
      # ylb <- expression (paste ("temperature  ATy  [", degree, "C]"))
      #   g1 <- ggplotWAC (data[, c("Time", spec$panel[[1]]$var)],
      #            ylab=spec$panel[[1]]$var[1], lty=c(1,1,1,2,3), lwd=c(1,1.5,1,2,1),
      #            legend.position='bottomleft')+xlab(NULL)
      #   g2 <- ggplotWAC (data[, c("Time", spec$panel[[2]]$var)],
      #                    ylab=spec$panel[[2]]$var[1], lty=c(1,1,1,2,3), lwd=c(1,1.5,1,2,1),
      #                    legend.position='bottomleft')
      #   # suppressWarnings (print (g))
      #   multiplot (g1,g2)
      for (pnl in 1:input$panels) {
        if ((pnl == spec$panels) || (pnl %% nrws == 0)) {
          op <- par (mar=c(5,4,1,2)+0.1)
        } else {
          op <- par (mar=c(2,4,1,2)+0.1)
        }
        if (spec$panel[[pnl]]$logY) {
          logY <- 'y'
          v <- spec$panel[[pnl]]$var
          for (vv in v) {
            Data <- Data[!is.na (Data[, vv]), ]
            DataV <- DataV[!is.na(DataV[,vv]), ]
          }
        } else {
          logY <- ''
        }
        yl <- NULL
        if (input$fixed && (pnl == input$panel)) {yl <- c(input$lineMin, input$lineMax)}
        lt <- spec$panel[[pnl]]$lt
        ltyps <- c('solid', 'dashed', 'dotted', 'd-dot', 'lg dash')
        for (i in 1:length (lt)) {
          lt[i] <- which (lt[i] == ltyps)
        }
        lt <- as.integer (lt)
        if (input$limits) {
          if (is.null (yl)) {
            plotWAC (DataV[, c('Time', spec$panel[[pnl]]$var)], log=logY,
                     col=spec$panel[[pnl]]$col,
                     lwd=spec$panel[[pnl]]$lw,
                     lty=lt)  
          } else {
            plotWAC (DataV[, c('Time', spec$panel[[pnl]]$var)], ylim=yl, log=logY,
                     col=spec$panel[[pnl]]$col,
                     lwd=spec$panel[[pnl]]$lw,
                     lty=lt)  
          }
        } else {
          if (Trace) {print (c('in plotWAC, col=', spec$panel[[pnl]]$col))}
          if (is.null (yl)) {
            
            plotWAC (Data[, c('Time', spec$panel[[pnl]]$var)], log=logY,
                     col=spec$panel[[pnl]]$col,
                     lwd=spec$panel[[pnl]]$lw,
                     lty=lt) 
          } else {
            plotWAC (Data[, c('Time', spec$panel[[pnl]]$var)], ylim=yl, log=logY,
                     col=spec$panel[[pnl]]$col,
                     lwd=spec$panel[[pnl]]$lw,
                     lty=lt) 
          }
        }
        if (input$footer) {AddFooter ()}
        # }
        #       si <- input$plot
        #       updateSelectInput (session, 'Rplot', selected=st[si])
        
      }
      reac$newspec <- FALSE
    }
  }, width=920, height=640)
  
  
  output$track <- renderPlot ({  ## track
    # input$typeFlight
    reac$newspec
    if (is.null(input$times[1])) {
      if (Trace) {print ('in track but input time is NULL')}
      return ()
    }
    if (Trace) {
      print ('track entry, reac$newdisplay is:')
      print (reac$newdisplay)
    }
    # input$PlotVar
    Project <- input$Project
    if (reac$newdisplay) {
      # VRPlot <- VRP ()
      if (Trace) {print ('entered track')}
      # VRPlot <<- VRPlot
      Data <- data()
      if (length (Data) < 2) {
        warning (sprintf ('variable error in (%s)', fname))
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
        return ()
      }
      
      if (Trace) {
        print (sprintf ('input$times %s %s', formatTime (input$times[1]),
                        formatTime (input$times[2])))
        print (sprintf ('global times are %s %s',
                        formatTime (times[1]), formatTime (times[2])))
      }
      namesV <- names(Data)
      namesV <- namesV[namesV != "Time"]
      for (n in namesV) {
        Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
      }
      # Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
      if (nrow (Data) <= 1) {
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
        reac$newdisplay <- TRUE
        reac#newdata <- TRUE
        return()
      }
      ## see global.R functions:
      DataV <- limitData (Data, input, input$limits2)
      ndv <- names (DataV)
      
      SE <- getStartEnd (Data$Time)
      i <- getIndex (Data$Time, SE[1])
      FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, input$typeFlight,
                        input$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                        strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                        strftime(Data$Time[getIndex(Data$Time,SE[2])],
                                 format="%H:%M:%S", tz='UTC'))
      FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
      AddFooter <<- function() {
        CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
        mtext(paste(FigFooter,'generated by Ranadu plotTrack', #CallingFunction,
                    FigDatestr),1,outer=T,cex=0.75)
      }
      
      par(oma=c(1.1,0,0,0))
      if (input$limits2) {
        plotTrack (DataV, .Spacing=15, .WindFlags=3)
      } else {
        DataSave <<- Data
        plotTrack (Data, .Spacing=15, .WindFlags=3)
      }
      AddFooter ()
      
      # }
      #       si <- input$plot
      #       updateSelectInput (session, 'Rplot', selected=st[si])
      
    }
  }, width=640, height=640)
  
  
  output$theight <- renderPlot ({  ## theight
    # input$typeFlight
    if (is.null(input$times[1])) {
      if (Trace) {print ('in theight but input time is NULL')}
      return ()
    }
    if (Trace) {
      print ('theight entry, reac$newdisplay is:')
      print (reac$newdisplay)
    }
    # input$PlotVar
    Project <- input$Project
    if (reac$newdisplay) {
      # VRPlot <- VRP ()
      if (Trace) {print ('entered theight')}
      # VRPlot <<- VRPlot
      Data <- data()
      if (length (Data) < 2) {
        warning (sprintf ('variable error in (%s)', fname))
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
        return ()
      }
      
      if (Trace) {
        print (sprintf ('input$times %s %s', formatTime (input$times[1]),
                        formatTime (input$times[2])))
        print (sprintf ('global times are %s %s',
                        formatTime (times[1]), formatTime (times[2])))
      }
      namesV <- names(Data)
      namesV <- namesV[namesV != "Time"]
      for (n in namesV) {
        Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
      }
      # Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
      if (nrow (Data) <= 0) {
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
        reac$newdisplay <- TRUE
        reac#newdata <- TRUE
        return()
      }
      ## see global.R functions:
      DataV <- limitData (Data, input, input$limits2)
      ndv <- names (DataV)
      
      SE <- getStartEnd (Data$Time)
      i <- getIndex (Data$Time, SE[1])
      FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, input$typeFlight,
                        input$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                        strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                        strftime(Data$Time[getIndex(Data$Time,SE[2])],
                                 format="%H:%M:%S", tz='UTC'))
      FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
      AddFooter <<- function() {
        CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
        mtext(paste(FigFooter,'generated by ', CallingFunction,
                    FigDatestr),1,outer=T,cex=0.75)
      }
      
      par(oma=c(1.1,0,0,0))
      if (input$limits2) {
        plotWAC (DataV[, c('Time', 'GGALT')])
      } else {
        plotWAC (Data[, c('Time', 'GGALT')])
      }
      AddFooter ()
      
      # }
      #       si <- input$plot
      #       updateSelectInput (session, 'Rplot', selected=st[si])
      
    }
  }, width=640, height=640)  
  
  output$skewT <- renderPlot ({  ## skewT
    # input$typeFlight
    if (is.null(input$times[1])) {
      if (Trace) {print ('in skewT but input time is NULL')}
      return ()
    }
    if (Trace) {
      print ('skewT entry, reac$newdisplay is:')
      print (reac$newdisplay)
    }
    # input$PlotVar
    Project <- input$Project
    if (reac$newdisplay) {
      # VRPlot <- VRP ()
      if (Trace) {print ('entered skewT')}
      # VRPlot <<- VRPlot
      Data <- data()
      if (length (Data) < 2) {
        warning (sprintf ('variable error in (%s)', fname))
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
        return ()
      }
      
      if (Trace) {
        print (sprintf ('input$times %s %s', formatTime (input$times[1]),
                        formatTime (input$times[2])))
        print (sprintf ('global times are %s %s',
                        formatTime (times[1]), formatTime (times[2])))
      }
      namesV <- names(Data)
      namesV <- namesV[namesV != "Time"]
      for (n in namesV) {
        Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
      }
      # Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
      if (nrow (Data) <= 0) {
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
        reac$newdisplay <- TRUE
        reac#newdata <- TRUE
        return()
      }
      ## see global.R functions:
      DataV <- limitData (Data, input, input$limits6)
      ndv <- names (DataV)
      
      SE <- getStartEnd (Data$Time)
      i <- getIndex (Data$Time, SE[1])
      FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, input$typeFlight,
                        input$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                        strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                        strftime(Data$Time[getIndex(Data$Time,SE[2])],
                                 format="%H:%M:%S", tz='UTC'))
      FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
      AddFooter <<- function() {
        CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
        mtext(paste(FigFooter,'generated by ', CallingFunction,
                    FigDatestr),1,outer=T,cex=0.75)
      }
      op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
      if (input$limits6) {
        DF <- DataV[, c("PSXC", "ATX", "DPXC")]
      } else {
        DF <- Data[, c("PSXC", "ATX", "DPXC")]
      }
      colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
      suppressWarnings (print(SkewTSounding (DF, AverageInterval=5, BackgroundSpecs="skewTDiagram.Rdata")
                              +ggtitle(sprintf("Flight %s, whole-flight-average", Flight))))
      
      AddFooter ()
      
      # }
      #       si <- input$plot
      #       updateSelectInput (session, 'Rplot', selected=st[si])
      
    }
  }, width=780, height=640) 
  
  
  output$stats <- renderDataTable ({    ## stats
    if (Trace) {print ('entered stats')}
    input$times
    Ds <- limitData (data(), input)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[, c('Time', VRPlot[[psq[1, input$plot]]])]
    Ds <- Ds[(Ds$Time >= times[1]) & (Ds$Time <= times[2]), ]
    Dstats <- data.frame ()
    Dstats['Time', 1] <- 'Time'
    Dstats['Time', 2] <- NA
    Dstats['Time', 3] <- NA
    Dstats['Time', 4] <- formatTime (Ds$Time[1])
    Dstats['Time', 5] <- formatTime (Ds$Time[nrow(Ds)])
    for (nm in names(Ds)) {
      if (nm == 'Time') {next}
      Dstats[nm, 1] <- nm
      Dstats[nm, 2] <- mean (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 3]   <- sd   (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 4]  <- min  (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 5]  <- max  (Ds[, nm], na.rm=TRUE)
    }
    names(Dstats) <- c('variable', 'mean', 'sd', 'min', 'max')
    row.names (Dstats) <- names(Ds)
    # Dstats[2:nrow(Dstats), 2:5] <- format(Dstats[2:nrow(Dstats),2:5], digits=5, scientific=FALSE)
    for (k in 2:5) {
      Dstats[2:nrow(Dstats), k] <- sprintf('%.3f', as.numeric(Dstats[2:nrow(Dstats), k]))
    }
    if (Trace) {print (str(Dstats))}
    Dstats
  }, options=list(paging=FALSE, searching=FALSE))
  
  output$hist <- renderPlot ({
    input$PlotVar
    input$times
    layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(8,8,8))
    op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))
    if (Trace) {print ('entered hist')}
    Ds <- limitData (data(), input)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[, c('Time', VRPlot[[psq[1, input$plot]]])]
    Ds <- Ds[(Ds$Time > times[1]) & (Ds$Time < times[2]), ]
    kount <- 0
    for (nm in names (Ds)) {
      if (nm == 'Time') {next}
      kount <- kount + 1
      if (kount > 6) {break}
      hist (Ds[ ,nm], freq=FALSE, breaks=50, xlab=nm, 
            ylab='probability density', main=NA)
    }
  }, width=920, height=680)
  
  output$barWvsZ <- renderPlot ({
    if (Trace) {print ('entered barXvsZ')}
    input$PlotVar
    input$times
    layout (matrix(1:6, ncol=3), widths=c(5,5,5), heights=c(8,8))
    op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))
    Ds <- limitData (data(), input)
    
    Ds <- Ds[, c('Time', VRPlot[[psq[1, input$plot]]], 'GGALT')]
    Ds <- Ds[(Ds$Time > times[1]) & (Ds$Time < times[2]), ]
    Ds <- Ds[!is.na (Ds$GGALT), ]
    kount <- 0
    for (nm in names (Ds)) {
      if (nm == 'Time') {next}
      if (nm == 'GGALT') {next}
      kount <- kount + 1
      if (kount > 6) {break}
      DB <- data.frame ('Z1'=Ds[, nm])
      DB[Ds$GGALT > 1000, 'Z1'] <- NA
      for (j in 2:15) {
        zmax <- j*1000
        zmin <- zmax-1000
        V <- sprintf ('Z%d', j)
        DB[,V] <- Ds[, nm]
        DB[(Ds$GGALT < zmin) | (Ds$GGALT > zmax), V] <- NA
      }
      boxplot (DB, horizontal=TRUE, outline=TRUE, 
               xlab=nm, ylab='altitude [km]', names=NULL)
    }
  }, width=920, height=680)
  
  output$listing <- renderDataTable ({
    if (Trace) {print ('entered listing')}
    Ds <- limitData (data(), input)
    Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[(Ds$Time > times[1]) & (Ds$Time < times[2]), ]
    Ds
  }, options=list(paging=TRUE, searching=TRUE))
  
  outputOptions (output, 'display', priority=-10)
  outputOptions (output, 'stats', priority=-10)
  outputOptions (output, 'listing', priority=-10)
  outputOptions (output, 'hist', priority=-10)
  outputOptions (output, 'barWvsZ', priority=-10)
})


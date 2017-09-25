require (shiny)

shinyServer(function(input, output, session) {
  
#   createAlert(session, 'pleasewait', alertId = 'pausing', title = 'start-up window',
#               content = 'please wait a few seconds for the application to\nfetch data and start, Proceed when this message disappears.')
#   
  ################ OBSERVERS ########################
  
  ## use an observer for each item in the plot definition:
  ##    -> write to global plotSpec or other global location
  ##    -> set appropriate reac$ variable (e.g., for new data, new display)
  
  observe ({
    
  }, priority=0)
  
  observeEvent (input$plot_click, {
    print (input$plot_click)
    xcursor <- as.integer(input$plot_click$x)
    xcursor <- xcursor - xcursor %% 60  ## set even minute
    ycursor <- input$plot_click$y
    T1 <- as.POSIXlt(xcursor, origin='1970-01-01', tz='UTC')
    TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    checkTime <<- T1  ## selected to be even-minute value
    updateTextInput (session, 'RefT', value=formatTime (checkTime))
    print (sprintf ('click position is %d %f', TB1, ycursor))
  } )
  
    observeEvent (input$plot_brush, {
    xmin <- as.integer(input$plot_brush$xmin)
    xmax <- as.integer(input$plot_brush$xmax)
    T1 <- as.POSIXlt(xmin, origin='1970-01-01', tz='UTC')
    T2 <- as.POSIXlt(xmax, origin='1970-01-01', tz='UTC')
    TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    TB2 <- T2$hour*10000 + T2$min*100 + T2$sec
    print (sprintf ('brush times are %d %d', TB1, TB2))
    plotSpec$Times[1] <<- T1
    plotSpec$Times[2] <<- T2
    updateSliderInput (session, 'times', value=plotSpec$Times)
    updateTextInput (session, 'tstart', value=formatTime (plotSpec$Times[1]))
    updateTextInput (session, 'tend',   value=formatTime (plotSpec$Times[2]))
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newstats <- reac$newstats + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    isolate (reac$newbin <- reac$newbin + 1)
  } )
  # exprBrush <- quote ({
  #   xmin <- as.integer(input$plot_brush$xmin)
  #   xmax <- as.integer(input$plot_brush$xmax)
  #   T1 <- as.POSIXlt(xmin, origin='1970-01-01', tz='UTC')
  #   T2 <- as.POSIXlt(xmax, origin='1970-01-01', tz='UTC')
  #   TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
  #   TB2 <- T2$hour*10000 + T2$min*100 + T2$sec
  #   print (sprintf ('brush times are %d %d', TB1, TB2))
  # })
  # obsBrush <- observe (exprBrush, quoted=TRUE)
  
  exprProject <- quote ({ 
    if (input$Project != plotSpec$Project) {
      plotSpec$Project <<- input$Project
      plotSpec$fname2d <<- NULL
      if (exists ("cfile", where=1)) {rm(cfile, pos=1)}
      ## clear Paluch times
      updateTextInput (session, 'paluchStart', value='0')
      updateTextInput (session, 'paluchEnd', value='36:00:00')
      updateTextInput (session, 'paluchCStart', value='0')
      updateTextInput (session, 'paluchCEnd', value='36:00:00')
      if (exists ('specialData')) {
        VarList <<- VarList [-which (VarList %in% names(specialData))]
        rm (specialData, pos=1)
      }
    }
    ## set list of available probes
    CHP <- vector ('character')
    if (any (grepl ('CCDP', FI$Variables))) {CHP <- c(CHP, 'CDP')}
    if (any (grepl ('CSP100', FI$Variables))) {CHP <- c(CHP, 'FSSP')}
    if (any (grepl ('CUHSAS', FI$Variables))) {CHP <- c(CHP, 'UHSAS')}
    if (any (grepl ('CS200', FI$Variables))) {CHP <- c(CHP, 'PCASP')}
    if (any (grepl ('C1DC', FI$Variables))) {CHP <- c(CHP, '2DC')}
    updateCheckboxGroupInput (session, 'probe', choices=CHP, selected=CHP)
    CHP <<- CHP
    if (Trace) {print (sprintf ('Project: probe choices are %s', CHP))}
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print ('Project: reset newdata')}
  })
  obsProject <- observe (exprProject, quoted=TRUE)
  
  exprFlight <- quote ({
    if (input$Flight != plotSpec$Flight) {
      plotSpec$Flight <<- input$Flight
      plotSpec$fname2d <<- NULL
      if (exists ("cfile", where=1)) {rm(cfile, pos=1)}
      ## clear Paluch times
      updateTextInput (session, 'paluchStart', value='0')
      updateTextInput (session, 'paluchEnd', value='36:00:00')
      updateTextInput (session, 'paluchCStart', value='0')
      updateTextInput (session, 'paluchCEnd', value='36:00:00')
      if (exists ('specialData')) {
        VarList <<- VarList [-which (VarList %in% names(specialData))]
        rm (specialData, pos=1)
      }
    }
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print ('Flight: reset newdata')}
  })
  obsFlight <- observe (exprFlight, quoted=TRUE)
  
  exprSRC <- quote ({
    plotSpec$SRC <<- input$SRC
    if (exists ("cfile", where=1)) {rm(cfile, pos=1)}
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print ('SRC: reset newdata')}
  })
  obsSRC <- observe (exprSRC, quoted=TRUE)
  
  exprTypeFlight <- quote ({
    plotSpec$TypeFlight <<- input$typeFlight
    plotSpec$fname2d <<- NULL
    if (exists ("cfile", where=1)) {rm(cfile, pos=1)}
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print ('TypeFlight: reset newdata')}
  })
  obsTypeFlight <- observe (exprTypeFlight, quoted=TRUE)
  
  exprTime <- quote ({
    if (Trace) {print (sprintf ('Time: Times %s %s times %s %s', plotSpec$Times[1], plotSpec$Times[2],
                                input$times[1], input$times[2]))}
    
    plotSpec$PaluchTimes <<- input$times
    plotSpec$PaluchCTimes <<- input$times
    if (any (input$times != plotSpec$Times)) {
      plotSpec$Times <<- input$times
      updateTextInput (session, 'tstart', value=formatTime(plotSpec$Times[1]))
      updateTextInput (session, 'tend', value=formatTime(plotSpec$Times[2]))
      updateTextInput (session, 'paluchStart', value=formatTime(plotSpec$Times[1]))
      updateTextInput (session, 'paluchEnd', value=formatTime(plotSpec$Times[2]))
      updateTextInput (session, 'paluchCStart', value=formatTime(plotSpec$Times[1]))
      updateTextInput (session, 'paluchCEnd', value=formatTime(plotSpec$Times[2]))
      isolate (reac$newdisplay <- reac$newdisplay + 1)
      isolate (reac$newhistogram <- reac$newhistogram + 1)
      isolate (reac$newstats <- reac$newstats + 1)
      isolate (reac$newscat <- reac$newscat + 1)
      isolate (reac$newbin <- reac$newbin + 1)
      isolate (reac$newvarp <- reac$newvarp + 1)
      isolate (reac$newskewT <- reac$newskewT + 1)
      if (Trace) {
        print ('Time: reset newdisplay')
        print (sprintf ('Time: times are %s %s', plotSpec$Times[1], plotSpec$Times[2]))
      }
    }
  })
  obsTime <- observe (exprTime, quoted=TRUE, priority=-10)
  
  exprPlotVar <- quote ({
    input$addVarP
    isolate (plt <- input$plot)
    isolate (pnl <- input$panel)
    isolate (lv <- input$lineV)
    
    print (sprintf ('lv, pnl, plt = %d %d %d adv %s', lv,pnl, plt, input$addVarP))
    print (sprintf ('plotSpec is %s', plotSpec$Plot[[plt]]$panel[[pnl]]$var[lv]))
    if ((lv <= length(plotSpec$Plot[[plt]]$panel[[pnl]]$var)) && input$addVarP != plotSpec$Plot[[plt]]$panel[[pnl]]$var[lv]) {
      isolate (reac$newdisplay <- reac$newdisplay + 1)
      if (Trace) {print ('PlotVar: reset newdisplay')}
      if (input$addVarP != 'omit') {
        if (input$addVarP == 'select') {
        } else {
          plotSpec$Plot[[plt]]$panel[[pnl]]$var[lv] <<- input$addVarP
          print (sprintf (' addVarP is %s', input$addVarP))
          if (lv == 1) {
            plotSpec$Plot[[plt]]$panel[[pnl]]$lab[lv] <<- input$addVarP
            updateTextInput (session, 'ylbl', value=input$addVarP)
          }
          if ((ncol (Data) < 2) || (!(input$addVarP %in% names (Data)))) {
            if (exists ('specialData') && (input$addVarP %in% names (specialData))) {
            } else {
              print ('need new data to include new variable - 4')
	      print (c('names in Data:', names(Data)))
	      print (c('names in specialData:', names(specialData)))
              reac$newdata <- reac$newdata + 1
            }
          }
        }
      } else {
        v <- plotSpec$Plot[[plt]]$panel[[pnl]]$var
        v <- v[-lv]
        if (Trace) {print (sprintf ('PlotVar: new var list is %s', v))}
        plotSpec$Plot[[plt]]$panel[[pnl]]$var <<- v
        print (sprintf (' variable deleted; remaining is %s', v))
        # nms <- names (data ())  ## just a data ref to get reset
        updateSelectInput (session, 'addVarP', selected='select')  
      }
    }
  })
  obsPlotVar <- observe (exprPlotVar, quoted=TRUE, priority=100)
  
  exprTstart <- quote ({
    ## ignore it if before start or after finish
    if (Trace) {print (sprintf ('Tstart: input$tstart=%s, min/maxT=%s %s',
                                input$tstart, minT, maxT))}
    # invalidateLater (500, session)
    txt <- input$tstart
    ## protect against typing errors that insert a character:
    if ((nchar(txt) > 0) &&(!grepl('[^0-9:]', txt))) {  ## ^ means not in the list
      hhmmss <- as.integer (gsub (':', '', txt))
      i1 <- getIndex (Data, hhmmss)
      if (i1 > 0) {
        if (plotSpec$Times[1] != Data$Time[i1]) {
          plotSpec$Times[1] <<- Data$Time[i1]
          # freezeReactiveValue (input, 'times')
          updateTextInput (session, 'tstart', value=formatTime (plotSpec$Times[1]))
          isolate (reac$newdisplay <- reac$newdisplay + 1)
          isolate (reac$newhistogram <- reac$newhistogram + 1)
          isolate (reac$newstats <- reac$newstats + 1)
          isolate (reac$newscat <- reac$newscat + 1)
          isolate (reac$newskewT <- reac$newskewT + 1)
          isolate (reac$newvarp <- reac$newvarp + 1)
        }
        updateSliderInput (session, 'times', value=plotSpec$Times)
        if (Trace) {
          print ('Tstart: reset times')
          print (sprintf ('Tstart: Times are %s %s', plotSpec$Times[1], plotSpec$Times[2]))
        }
      }
    }
  })
  obsTstart <- observe (exprTstart, quoted=TRUE)
  
  exprRefT <- quote ({
    txt <- input$RefT
    print (sprintf (' entered RefT, checkTime=%s, RefT=%s', checkTime, txt))
    if ((nchar(txt) > 0) &&(!grepl('[^0-9:]', txt))) {  ## ^ means not in the list
      hhmmss <- as.integer (gsub (':', '', txt))
      i1 <- getIndex (Data, hhmmss)
      if (i1 > 0) {
        checkTime <<- Data$Time[i1]
        updateTextInput (session, 'RefT', value=formatTime (checkTime))
      }
    }
  })
  obsRefT <- observe (exprRefT, quoted=TRUE)
  
  exprPaluchstart <- quote ({
    ## ignore it if before start or after finish
    if (Trace) {print (sprintf ('Paluchstart: input$paluchStart=%s, min/maxT=%s %s',
                                input$paluchStart, minT, maxT))}
    txt <- input$paluchStart
    ## protect against typing errors that insert a character:
    if ((nchar(txt) > 0) &&(!grepl('[^0-9:]', txt))) {  ## ^ means not in the list
      hhmmss <- as.integer (gsub (':', '', txt))
      i1 <- getIndex (Data, hhmmss)
      if (i1 > 0) {
        plotSpec$PaluchTimes[1] <<- Data$Time[i1]
        if (Trace) {
          print ('Paluchstart: reset Paluch times')
          print (sprintf ('Paluchstart: times are %s %s', plotSpec$PaluchTimes[1], plotSpec$PaluchTimes[2]))
        }
      }
      updateTextInput (session, 'paluchStart', value=formatTime(plotSpec$PaluchTimes[1]))
    }
  })
  obsPaluchstart <- observe (exprPaluchstart, quoted=TRUE)
  
  exprPaluchCstart <- quote ({
    ## ignore it if before start or after finish
    if (Trace) {print (sprintf ('PaluchCstart: input$paluchCStart=%s, min/maxT=%s %s',
                                input$paluchCStart, minT, maxT))}
    txt <- input$paluchCStart
    ## protect against typing errors that insert a character:
    if ((nchar(txt) > 0) &&(!grepl('[^0-9:]', txt))) {  ## ^ means not in the list
      hhmmss <- as.integer (gsub (':', '', txt))
      i1 <- getIndex (Data, hhmmss)
      if (i1 > 0) {
        plotSpec$PaluchCTimes[1] <<- Data$Time[i1]
        if (Trace) {
          print ('PaluchCstart: reset PaluchCTimes')
          print (sprintf ('PaluchCstart: PaluchCTimes are %s %s', plotSpec$PaluchCTimes[1], plotSpec$PaluchCTimes[2]))
        }
      }
      updateTextInput (session, 'paluchCStart', value=formatTime(plotSpec$PaluchCTimes[1]))
    }
  })
  obsPaluchCstart <- observe (exprPaluchCstart, quoted=TRUE)
  
  exprTend <- quote ({
    ## ignore it if before start or after finish
    # invalidateLater (500, session)
    txt <- input$tend
    ## protect against typing errors that insert a character:
    if ((nchar(txt) > 0) &&(!grepl('[^0-9:]', txt))) {  ## ^ means not in the list
      hhmmss <- as.integer (gsub (':', '', txt))
      i2 <- getIndex (Data, hhmmss)
      if (i2 > 0) {
        if (plotSpec$Times[2] != Data$Time[i2]) {
          plotSpec$Times[2] <<- Data$Time[i2]
          updateTextInput (session, 'tend', value=formatTime (plotSpec$Times[2]))
          isolate (reac$newdisplay <- reac$newdisplay + 1)
          isolate (reac$newhistogram <- reac$newhistogram + 1)
          isolate (reac$newstats <- reac$newstats + 1)
          isolate (reac$newscat <- reac$newscat + 1)
          isolate (reac$newskewT <- reac$newskewT + 1)
          isolate (reac$newvarp <- reac$newvarp + 1)
        }
        
        # freezeReactiveValue (input, 'times')
        updateSliderInput (session, 'times', value=plotSpec$Times)
        if (Trace) {print (sprintf ('Tend:, updating time to %s %s', formatTime(plotSpec$Times[1]), formatTime(plotSpec$Times[2])))}
        #     isolate (reac$newdisplay <- reac$newdisplay + 1)
        #     isolate (reac$newhistogram <- reac$newhistogram + 1)
        #     isolate (reac$newstats <- reac$newstats + 1)
        #     isolate (reac$newscat <- reac$newscat + 1)
        if (Trace) {
          print ('Tend: reset new times')
          print (sprintf ('Tend: Times are %s %s', plotSpec$Times[1], plotSpec$Times[2]))
        }
      }
    }
  })
  obsTend <- observe (exprTend, quoted=TRUE)
  
  exprPaluchend <- quote ({
    ## ignore it if before start or after finish
    txt <- input$paluchEnd
    ## protect against typing errors that insert a character:
    if ((nchar(txt) > 0) &&(!grepl('[^0-9:]', txt))) {  ## ^ means not in the list
      hhmmss <- as.integer (gsub (':', '', txt))
      i2 <- getIndex (Data, hhmmss)
      if (Trace) {print (sprintf ('Paluchend: i2 is %d, hhmmss=%d', i2, hhmmss))}
      if (i2 > 0) {
        plotSpec$PaluchTimes[2] <<- Data$Time[i2]
        if (Trace) {print (sprintf ('Paluchend b1: updating time to %s %s', formatTime(plotSpec$PaluchTimes[1]), formatTime(plotSpec$PaluchTimes[2])))}
        if (Trace) {
          print ('Paluchend b1: reset Paluch times')
          print (sprintf ('Paluchend b1: PaluchTimes are %s %s', plotSpec$PaluchTimes[1], plotSpec$PaluchTimes[2]))
        }
      } else {
        plotSpec$PaluchTimes[2] <<- Data$Time[nrow(Data)] 
        if (Trace) {print (sprintf ('Paluchend b2: updating time to %s %s', formatTime(plotSpec$PaluchTimes[1]), formatTime(plotSpec$PaluchTimes[2])))}
        if (Trace) {
          print ('Paluchend b2: reset Paluch times')
          print (sprintf ('Paluchend b2: PaluchTimes are %s %s', plotSpec$PaluchTimes[1], plotSpec$PaluchTimes[2]))
        }
      }
    }
    updateTextInput (session, 'paluchEnd', value=formatTime(plotSpec$PaluchTimes[2]))
  })
  obsPaluchend <- observe (exprPaluchend, quoted=TRUE)
  
  exprPaluchCend <- quote ({
    ## ignore it if before start or after finish
    txt <- input$paluchCEnd
    ## protect against typing errors that insert a character:
    if ((nchar(txt) > 0) &&(!grepl('[^0-9:]', txt))) {  ## ^ means not in the list
      hhmmss <- as.integer (gsub (':', '', txt))
      i2 <- getIndex (Data, hhmmss)
      if (Trace) {print (sprintf ('PaluchCend: i2 is %d, hhmmss=%d', i2, hhmmss))}
      if (i2 > 0) {
        plotSpec$PaluchCTimes[2] <<- Data$Time[i2]
        if (Trace) {print (sprintf ('PaluchCend b1: updating PaluchCTimes to %s %s', formatTime(plotSpec$PaluchCTimes[1]), formatTime(plotSpec$PaluchCTimes[2])))}
        if (Trace) {
          print ('PaluchCend b1: reset PaluchCTimes')
          print (sprintf ('PaluchCend: PaluchCTimes are %s %s', plotSpec$PaluchCTimes[1], plotSpec$PaluchCTimes[2]))
        }
      } else {
        plotSpec$PaluchCTimes[2] <<- Data$Time[nrow(Data)] 
        if (Trace) {print (sprintf ('PaluchCend b2: updating PaluchCTimes to %s %s', formatTime(plotSpec$PaluchCTimes[1]), formatTime(plotSpec$PaluchCTimes[2])))}
        if (Trace) {
          print ('PaluchCend: reset PaluchCTimes')
          print (sprintf ('PaluchCTimes are %s %s', plotSpec$PaluchCTimes[1], plotSpec$PaluchCTimes[2]))
        }
      }
    }
    updateTextInput (session, 'paluchCEnd', value=formatTime(plotSpec$PaluchCTimes[2]))
  })
  obsPaluchCend <- observe (exprPaluchCend, quoted=TRUE)
  
  exprPanels <- quote ({
    plotSpec$Plot[[input$plot]]$panels <<- input$panels
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('Panels: reset newdisplay')}
  })
  obsPanels <- observe (exprPanels, quoted=TRUE)
  
  exprCols <- quote ({
    plotSpec$Plot[[input$plot]]$columns <<- input$cols
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('Cols: reset newdisplay')}
  })
  obsCols <- observe (exprCols, quoted=TRUE)
  
  exprlogY <- quote ({
    plotSpec$Plot[[input$plot]]$panel[[input$panel]]$logY <<- input$logY
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('logY: reset newdisplay')}
  })
  obsLogY <- observe (exprlogY, quoted=TRUE)
  
  exprFixed <- quote ({
    plotSpec$Plot[[input$plot]]$panel[[input$panel]]$fixed <<- input$fixed
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('Fixed: reset newdisplay')}
  })
  obsFixed <- observe (exprFixed, quoted=TRUE)
  
  exprSmooth <- quote ({
    input$smooth
    print (sprintf (' input$smooth:'))
    print (input$smooth)
    isolate (lv <- input$lineV)
    plotSpec$Plot[[input$plot]]$panel[[input$panel]]$smooth[lv] <<- input$smooth
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('Smooth: reset newdisplay')}
  })
  obsSmooth <- observe (exprSmooth, quoted=TRUE)
  
  exprPanelMin <- quote ({
    plotSpec$Plot[[input$plot]]$panel[[input$panel]]$ylim[1] <<- input$panelMin
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('PanelMin: reset newdisplay')}
  })
  obsPanelMin <- observe (exprPanelMin, quoted=TRUE)
  
  exprPanelMax <- quote ({
    plotSpec$Plot[[input$plot]]$panel[[input$panel]]$ylim[2] <<- input$panelMax
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('PanelMax: reset newdisplay')}
  })
  obsPanelMax <- observe (exprPanelMax, quoted=TRUE)
  
  exprRestrict <- quote ({
    plotSpec$Plot[[input$plot]]$restrict <<- input$restrict
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('Restrict: reset newdisplay')}
  })
  obsRestrict <- observe (exprRestrict, quoted=TRUE)
  
  exprMEMcolor <- quote ({
    isolate (plt <- input$plot)
    plotSpec$Variance[[plt]]$Definition$MEMcolor <<- input$MEMcolor
    ## isolate (reac$newvarp <- reac$newvarp + 1)
  })
  obsMEMcolor <- observe (exprMEMcolor, quoted=TRUE)
  
  exprMEMadd <- quote ({
    isolate (plt <- input$plot)
    plotSpec$Variance[[plt]]$Definition$MEMadd <<- input$MEMadd
    ## isolate (reac$newvarp <- reac$newvarp + 1)
  })
  obsMEMadd <- observe (exprMEMadd, quoted=TRUE)
  
  exprhPanels <- quote ({
    plotSpec$Hist[[input$plot]]$panels <<- input$hpanels
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hPanels: reset newhistogram')}
  })
  obshPanels <- observe (exprhPanels, quoted=TRUE)
  
  exprhCols <- quote ({
    plotSpec$Hist[[input$plot]]$columns <<- input$hcols
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hCols: reset newhistogram')}
  })
  obshCols <- observe (exprhCols, quoted=TRUE)
  
  exprhPanel <- quote ({
    plt <- input$plot
    pnl <- input$hpanel
    updateCheckboxInput (session, 'hlogY', value=plotSpec$Hist[[plt]]$panel[[pnl]]$logY)
    updateCheckboxInput (session, 'hfixed', value=plotSpec$Hist[[plt]]$panel[[pnl]]$fixed)
    updateNumericInput (session, 'hlineV', value=1)
    updateSelectInput (session, 'haddVarP', 
                       choices=sort(FI$Variables),
                       selected=plotSpec$Hist[[plt]]$panel[[pnl]]$var[1])
    updateSelectInput (session, 'hvarColor', selected=plotSpec$Hist[[plt]]$panel[[pnl]]$col[1])
    updateNumericInput (session, 'hlineW', value=plotSpec$Hist[[plt]]$panel[[pnl]]$lw[1])
    updateRadioButtons (session, 'hlineStyle', selected=ltyps[plotSpec$Hist[[plt]]$panel[[pnl]]$lt[1]])
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hPanel: reset newhistogram')}
  })
  obshPanel <- observe (exprhPanel, priority=8, quoted=TRUE)    
  
  exprhlogY <- quote ({
    plotSpec$Hist[[input$plot]]$panel[[input$hpanel]]$logY <<- input$hlogY
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hlogY: reset newhistogram')}
  })
  obshLogY <- observe (exprhlogY, priority=2, quoted=TRUE)
  
  exprhFixed <- quote ({
    plotSpec$Hist[[input$plot]]$panel[[input$hpanel]]$fixed <<- input$hfixed
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hFixed: reset newhistogram')}
  })
  obshFixed <- observe (exprhFixed, priority=2, quoted=TRUE)
  
  exprhPanelMin <- quote ({
    plotSpec$Hist[[input$plot]]$panel[[input$hpanel]]$ylim[1] <<- input$hpanelMin
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hPanelMin: reset newhistogram')}
  })
  obshPanelMin <- observe (exprhPanelMin, priority=2, quoted=TRUE)
  
  exprhPanelMax <- quote ({
    plotSpec$Hist[[input$plot]]$panel[[input$hpanel]]$ylim[2] <<- input$hpanelMax
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hPanelMax: reset newhistogram')}
  })
  obshPanelMax <- observe (exprhPanelMax, priority=2, quoted=TRUE)
  
  exprhRestrict <- quote ({
    plotSpec$Hist[[input$plot]]$restrict <<- input$limits3
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hRestrict: reset newhistogram')}
  })
  obshRestrict <- observe (exprhRestrict, priority=2, quoted=TRUE)
  
  exprhlineV <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$hpanel)
    lv <- input$hlineV
    if (lv <= length (plotSpec$Hist[[plt]]$panel[[pnl]]$var)) {
      updateSelectInput (session, 'haddVarP', selected=plotSpec$Hist[[plt]]$panel[[pnl]]$var[lv])
      updateSelectInput (session, 'hvarColor', selected=plotSpec$Hist[[plt]]$panel[[pnl]]$col[lv])
      updateNumericInput (session, 'hlineW', value=plotSpec$Hist[[plt]]$panel[[pnl]]$lw[lv])
      updateRadioButtons (session, 'hlineStyle', selected=ltyps[plotSpec$Hist[[plt]]$panel[[pnl]]$lt[lv]])
    } else {
      v <- plotSpec$Hist[[plt]]$panel[[pnl]]$var
      v <- c(v, v[length(v)])
      plotSpec$Hist[[plt]]$panel[[pnl]]$var <<- v
      if (Trace) {print (sprintf ('hlineV: var set to %s', v))}
      lbl <- plotSpec$Hist[[plt]]$panel[[pnl]]$lab
      lbl <- c(lbl, lbl[length(lbl)])
      plotSpec$Hist[[plt]]$panel[[pnl]]$lab <<- lbl
      cl <- plotSpec$Hist[[plt]]$panel[[pnl]]$col
      cl <- c(cl, cl[length(cl)])
      plotSpec$Hist[[plt]]$panel[[pnl]]$col <<- cl
      lw <- plotSpec$Hist[[plt]]$panel[[pnl]]$lw
      lw <- c(lw, lw[length(lw)])
      plotSpec$Hist[[plt]]$panel[[pnl]]$lw <<- lw
      lt <- plotSpec$Hist[[plt]]$panel[[pnl]]$lt
      lt <- c(lt, lt[length(lt)])
      plotSpec$Hist[[plt]]$panel[[pnl]]$lt <<- lt
      updateSelectInput (session, 'haddVarP', selected=v[length(v)])
      updateSelectInput (session, 'hvarColor', selected=cl[length(cl)])
      updateNumericInput (session, 'hlineW', value=lw[length(lw)])
      updateNumericInput (session, 'hlineStyle', value=lt[length(lt)])
    }
    if (Trace) {print ('hlineV: updated specs')}
    # isolate (reac$newhistogram <- reac$newhistogram + 1)
    # if (Trace) {print ('reset newhistogram 13')}
  })
  obshlineV <- observe (exprhlineV, priority=5, quoted=TRUE)
  
  ## observers for scatterplots
  exprsPanels <- quote ({
    plotSpec$Scat[[input$plot]]$panels <<- input$spanels
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('sPanels: reset newscat')}
  })
  obssPanels <- observe (exprsPanels, quoted=TRUE)
  
  exprsCols <- quote ({
    plotSpec$Scat[[input$plot]]$columns <<- input$scols
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('sCols: reset newscat')}
  })
  obssCols <- observe (exprsCols, quoted=TRUE)
  
  exprsPanel <- quote ({
    plt <- input$plot
    pnl <- input$spanel
    updateCheckboxInput (session, 'slogX', value=plotSpec$Scat[[plt]]$panel[[pnl]]$logX)
    updateCheckboxInput (session, 'slogY', value=plotSpec$Scat[[plt]]$panel[[pnl]]$logY)
    updateCheckboxInput (session, 'sfixed', value=plotSpec$Scat[[plt]]$panel[[pnl]]$fixed)
    updateNumericInput (session, 'slineV', value=1)
    updateSelectInput (session, 'saddVarP1', selected=plotSpec$Scat[[plt]]$panel[[pnl]]$varx)
    updateSelectInput (session, 'saddVarP2', selected=plotSpec$Scat[[plt]]$panel[[pnl]]$vary[1])
    updateSelectInput (session, 'svarColor', selected=plotSpec$Scat[[plt]]$panel[[pnl]]$col[1])
    updateNumericInput (session, 'ssize', value=plotSpec$Scat[[plt]]$panel[[pnl]]$size[1])
    updateNumericInput (session, 'symbol', value=plotSpec$Scat[[plt]]$panel[[pnl]]$symbol[1])
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('sPanel: reset newscat')}
  })
  obssPanel <- observe (exprsPanel, priority=8, quoted=TRUE)    
  
  exprslogX <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$logX <<- input$slogX
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('slogX: reset newscat')}
  })
  obssLogX <- observe (exprslogX, priority=2, quoted=TRUE)  
  
  exprslogY <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$logY <<- input$slogY
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('slogY: reset newscat')}
  })
  obssLogY <- observe (exprslogY, priority=2, quoted=TRUE)
  
  exprsFixed <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$fixed <<- input$sfixed
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('sFixed: reset newscat')}
  })
  obssFixed <- observe (exprsFixed, priority=2, quoted=TRUE)
  
  exprsPanelMinx <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$xlim[1] <<- input$spanelMinx
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('sPanelMinx: reset newscat')}
  })
  obssPanelMinx <- observe (exprsPanelMinx, priority=2, quoted=TRUE)
  
  exprsPanelMaxx <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$xlim[2] <<- input$spanelMaxx
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('sPanelMaxx: reset newscat')}
  })
  obssPanelMaxx <- observe (exprsPanelMaxx, priority=2, quoted=TRUE)
  
  exprsPanelMiny <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$ylim[1] <<- input$spanelMiny
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('sPanelMiny: reset newscat')}
  })
  obssPanelMiny <- observe (exprsPanelMiny, priority=2, quoted=TRUE)
  
  exprsPanelMaxy <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$ylim[2] <<- input$spanelMaxy
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('sPanelMaxy: reset newscat')}
  })
  obssPanelMaxy <- observe (exprsPanelMaxy, priority=2, quoted=TRUE)
  
  exprsRestrict <- quote ({
    plotSpec$Scat[[input$plot]]$restrict <<- input$limits4
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('sRestrict: reset newscat')}
  })
  obssRestrict <- observe (exprsRestrict, priority=2, quoted=TRUE)
  
  exprslineV <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$spanel)
    lv <- input$slineV
    if (lv <= length (plotSpec$Scat[[plt]]$panel[[pnl]]$vary)) {
      updateSelectInput (session, 'saddVarP1', selected=plotSpec$Scat[[plt]]$panel[[pnl]]$varx)
      updateSelectInput (session, 'saddVarP2', selected=plotSpec$Scat[[plt]]$panel[[pnl]]$vary[lv])
      updateSelectInput (session, 'svarColor', selected=plotSpec$Scat[[plt]]$panel[[pnl]]$col[lv])
      updateNumericInput (session, 'ssize', value=plotSpec$Scat[[plt]]$panel[[pnl]]$size[lv])
      updateNumericInput (session, 'symbol', value=plotSpec$Scat[[plt]]$panel[[pnl]]$symbol[lv])
    } else {
      vy <- plotSpec$Scat[[plt]]$panel[[pnl]]$vary
      vy <- c(vy, vy[length(vy)])
      plotSpec$Scat[[plt]]$panel[[pnl]]$vary <<- vy
      ## varx remains the same
      if (Trace) {print (sprintf ('slineV: vary set to %s', vy))}
      lbl <- plotSpec$Scat[[plt]]$panel[[pnl]]$lab
      lbl <- c(lbl, lbl[length(lbl)])
      plotSpec$Scat[[plt]]$panel[[pnl]]$lab <<- lbl
      cl <- plotSpec$Scat[[plt]]$panel[[pnl]]$col
      cl <- c(cl, cl[length(cl)])
      plotSpec$Scat[[plt]]$panel[[pnl]]$col <<- cl
      size <- plotSpec$Scat[[plt]]$panel[[pnl]]$size
      size <- c(size, size[length(size)])
      plotSpec$Scat[[plt]]$panel[[pnl]]$size <<- size
      symb <- plotSpec$Scat[[plt]]$panel[[pnl]]$symbol
      symb <- c(symb, symb[length(symb)])
      plotSpec$Scat[[plt]]$panel[[pnl]]$symbol <<- symb
      updateSelectInput (session, 'saddVarP2', selected=vy[length(vy)])
      updateSelectInput (session, 'svarColor', selected=cl[length(cl)])
      updateNumericInput (session, 'ssize', value=size[length(size)])
      updateNumericInput (session, 'symbol', value=symb[length(symb)])
    }
    if (Trace) {print ('slineV: saved specs')}
    # isolate (reac$newscat <- reac$newscat + 1)
    # if (Trace) {print ('reset newscat 13')}
  })
  obsslineV <- observe (exprslineV, priority=5, quoted=TRUE)
  
  exprssymbol <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$spanel)
    lv <- input$slineV
    plotSpec$Scat[[plt]]$panel[[pnl]]$symbol[lv] <<- input$symbol
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {
      print ('ssymbol: reset newscat')
      print (sprintf ('ssymbol: plt,pnl,lv,symbol: %d %d %d %d',plt,pnl,lv,input$symbol))
    }
  })
  obsssymbol <- observe (exprssymbol, quoted=TRUE)
  
  exprssize <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$spanel)
    lv <- input$slineV
    plotSpec$Scat[[plt]]$panel[[pnl]]$size[lv] <<- input$ssize
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('ssize: reset newscat')}
  })
  obsssize <- observe (exprssize, quoted=TRUE)
  
  ## observers for binned plots
  exprbPanels <- quote ({
    plotSpec$Bin[[input$plot]]$panels <<- input$bpanels
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bPanels: reset newbin')}
  })
  obsbPanels <- observe (exprbPanels, quoted=TRUE)
  
  exprbCols <- quote ({
    plotSpec$Bin[[input$plot]]$columns <<- input$bcols
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bCols: reset newbin')}
  })
  obsbCols <- observe (exprbCols, quoted=TRUE)
  
  exprbPanel <- quote ({
    plt <- input$plot
    pnl <- input$bpanel
    updateCheckboxInput (session, 'blogX', value=plotSpec$Bin[[plt]]$panel[[pnl]]$logX)
    updateCheckboxInput (session, 'blogY', value=plotSpec$Bin[[plt]]$panel[[pnl]]$logY)
    updateCheckboxInput (session, 'bfixed', value=plotSpec$Bin[[plt]]$panel[[pnl]]$fixed)
    updateNumericInput (session, 'blineV', value=1)
    updateSelectInput (session, 'baddVarP1', selected=plotSpec$Bin[[plt]]$panel[[pnl]]$varx)
    updateSelectInput (session, 'baddVarP2', selected=plotSpec$Bin[[plt]]$panel[[pnl]]$vary[1])
    updateSelectInput (session, 'bvarColor', selected=plotSpec$Bin[[plt]]$panel[[pnl]]$col[1])
    updateNumericInput (session, 'bsize', value=plotSpec$Bin[[plt]]$panel[[pnl]]$size[1])
    updateNumericInput (session, 'bsymbol', value=plotSpec$Bin[[plt]]$panel[[pnl]]$symbol[1])
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bPanel: reset newbin')}
  })
  obsbPanel <- observe (exprbPanel, priority=8, quoted=TRUE)    
  
  exprblogX <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$logX <<- input$blogX
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('blogX: reset newbin')}
  })
  obsbLogX <- observe (exprblogX, priority=2, quoted=TRUE)  
  
  exprblogY <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$logY <<- input$blogY
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('blogY: reset newbin')}
  })
  obsbLogY <- observe (exprblogY, priority=2, quoted=TRUE)
  
  exprbFixed <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$fixed <<- input$bfixed
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bFixed: reset newbin')}
  })
  obsbFixed <- observe (exprbFixed, priority=2, quoted=TRUE)
  
  exprbPanelMinx <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$xlim[1] <<- input$bpanelMinx
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bPanelMinx: reset newbin')}
  })
  obsbPanelMinx <- observe (exprbPanelMinx, priority=2, quoted=TRUE)
  
  exprbPanelMaxx <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$xlim[2] <<- input$bpanelMaxx
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bPanelMaxx: reset newbin')}
  })
  obsbPanelMaxx <- observe (exprbPanelMaxx, priority=2, quoted=TRUE)
  
  exprbPanelMiny <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$ylim[1] <<- input$bpanelMiny
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bPanelMiny: reset newbin')}
  })
  obsbPanelMiny <- observe (exprbPanelMiny, priority=2, quoted=TRUE)
  
  exprbPanelMaxy <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$ylim[2] <<- input$bpanelMaxy
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bPanelMaxy: reset newbin')}
  })
  obsbPanelMaxy <- observe (exprbPanelMaxy, priority=2, quoted=TRUE)
  
  exprbRestrict <- quote ({
    plotSpec$Bin[[input$plot]]$restrict <<- input$limits4
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bRestrict: reset newbin')}
  })
  obsbRestrict <- observe (exprbRestrict, priority=2, quoted=TRUE)
  
  exprblineV <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$bpanel)
    lv <- input$blineV
    print (sprintf ('blineV: plt=%d, pnl=%d, lv=%d', plt,pnl,lv))
    if (lv <= length (plotSpec$Bin[[plt]]$panel[[pnl]]$vary)) {
      updateSelectInput (session, 'baddVarP1', selected=plotSpec$Bin[[plt]]$panel[[pnl]]$varx)
      updateSelectInput (session, 'baddVarP2', selected=plotSpec$Bin[[plt]]$panel[[pnl]]$vary[lv])
      updateSelectInput (session, 'bvarColor', selected=plotSpec$Bin[[plt]]$panel[[pnl]]$col[lv])
      updateNumericInput (session, 'bsize', value=plotSpec$Bin[[plt]]$panel[[pnl]]$size[lv])
      updateNumericInput (session, 'bsymbol', value=plotSpec$Bin[[plt]]$panel[[pnl]]$symbol[lv])
    } else {
      vy <- plotSpec$Bin[[plt]]$panel[[pnl]]$vary
      vy <- c(vy, vy[length(vy)])
      plotSpec$Bin[[plt]]$panel[[pnl]]$vary <<- vy
      ## varx remains the same
      if (Trace) {print (sprintf ('blineV: vary set to %s', vy))}
      lbl <- plotSpec$Bin[[plt]]$panel[[pnl]]$lab
      lbl <- c(lbl, lbl[length(lbl)])
      plotSpec$Bin[[plt]]$panel[[pnl]]$lab <<- lbl
      cl <- plotSpec$Bin[[plt]]$panel[[pnl]]$col
      cl <- c(cl, cl[length(cl)])
      plotSpec$Bin[[plt]]$panel[[pnl]]$col <<- cl
      size <- plotSpec$Bin[[plt]]$panel[[pnl]]$size
      size <- c(size, size[length(size)])
      plotSpec$Bin[[plt]]$panel[[pnl]]$size <<- size
      symb <- plotSpec$Bin[[plt]]$panel[[pnl]]$symbol
      symb <- c(symb, symb[length(symb)])
      plotSpec$Bin[[plt]]$panel[[pnl]]$symbol <<- symb
      updateSelectInput (session, 'baddVarP2', selected=vy[length(vy)])
      updateSelectInput (session, 'bvarColor', selected=cl[length(cl)])
      updateNumericInput (session, 'bsize', value=size[length(size)])
      updateNumericInput (session, 'bsymbol', value=symb[length(symb)])
    }
    if (Trace) {print ('blineV: set specs')}
    # isolate (reac$newbin <- reac$newbin + 1)
    # if (Trace) {print ('reset newbin 13')}
  })
  obsblineV <- observe (exprblineV, priority=5, quoted=TRUE)
  
  exprbsymbol <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$bpanel)
    lv <- input$blineV
    plotSpec$Bin[[plt]]$panel[[pnl]]$symbol[lv] <<- input$bsymbol
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {
      print ('bsymbol: reset newbin')
      print (sprintf ('bsymbol: plt,pnl,lv,symbol: %d %d %d %d',plt,pnl,lv,input$bsymbol))
    }
  })
  obsbsymbol <- observe (exprbsymbol, quoted=TRUE)
  
  exprbsize <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$bpanel)
    lv <- input$blineV
    plotSpec$Bin[[plt]]$panel[[pnl]]$size[lv] <<- input$bsize
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bsize: reset newbin')}
  })
  obsbsize <- observe (exprbsize, quoted=TRUE)
  
  exprPlot <- quote ({
    plt <- input$plot
    updateNumericInput (session, 'panels', value=plotSpec$Plot[[plt]]$panels)
    updateNumericInput (session, 'cols', value=plotSpec$Plot[[plt]]$cols)
    updateNumericInput (session, 'panel', value=1)
    updateCheckboxInput (session, 'logY', value=plotSpec$Plot[[plt]]$panel[[1]]$logY)
    updateCheckboxInput (session, 'fixed', value=plotSpec$Plot[[plt]]$panel[[1]]$fixed)
    updateCheckboxInput (session, 'smooth', value=plotSpec$Plot[[plt]]$panel[[1]]$smooth[1])
    updateNumericInput (session, 'SGpoints', value=plotSpec$Plot[[plt]]$SGlength[1])
    updateCheckboxInput (session, 'restrict', value=plotSpec$Plot[[plt]]$restrict)
    updateNumericInput (session, 'lineV', value=1)
    updateSelectInput (session, 'addVarP', selected=plotSpec$Plot[[plt]]$panel[[1]]$var[1])
    updateSelectInput (session, 'varColor', selected=plotSpec$Plot[[plt]]$panel[[1]]$col[1])
    updateNumericInput (session, 'lineW', value=plotSpec$Plot[[plt]]$panel[[1]]$lw[1])
    updateRadioButtons (session, 'lineStyle', selected=ltyps[plotSpec$Plot[[plt]]$panel[[1]]$lt[1]])
    
    updateNumericInput (session, 'hpanels', value=plotSpec$Hist[[plt]]$panels)
    updateNumericInput (session, 'hcols', value=plotSpec$Hist[[plt]]$cols)
    updateNumericInput (session, 'hpanel', value=1)
    updateCheckboxInput (session, 'hlogY', value=plotSpec$Hist[[plt]]$panel[[1]]$logY)
    updateCheckboxInput (session, 'hfixed', value=plotSpec$Hist[[plt]]$panel[[1]]$fixed)
    updateCheckboxInput (session, 'hrestrict', value=plotSpec$Hist[[plt]]$restrict)
    updateNumericInput (session, 'hlineV', value=1)
    updateSelectInput (session, 'haddVarP', 
                       choices=c('select', 'omit', sort(FI$Variables)),
                       selected=plotSpec$Hist[[plt]]$panel[[1]]$var[1])
    updateSelectInput (session, 'hvarColor', selected=plotSpec$Hist[[plt]]$panel[[1]]$col[1])
    updateNumericInput (session, 'hlineW', value=plotSpec$Hist[[plt]]$panel[[1]]$lw[1])
    updateRadioButtons (session, 'hlineStyle', selected=ltyps[plotSpec$Hist[[plt]]$panel[[1]]$lt[1]])
    updateSelectInput (session, 'specvar', selected=plotSpec$Variance[[plt]]$Definition$var)
    updateSelectInput (session, 'speccovar', selected=plotSpec$Variance[[plt]]$Definition$cvar)
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    isolate (reac$newbin <- reac$newbin + 1)
    isolate (reac$newvarp <- reac$newvarp + 1)
    if (Trace) {print ('Plot: reset newdisplay etc.')}
  })
  obsPlot <- observe (exprPlot, quoted=TRUE)
  
  exprPanel <- quote ({
    plt <- input$plot
    pnl <- input$panel
    updateCheckboxInput (session, 'logY', value=plotSpec$Plot[[plt]]$panel[[pnl]]$logY)
    updateCheckboxInput (session, 'fixed', value=plotSpec$Plot[[plt]]$panel[[pnl]]$fixed)
    updateCheckboxInput (session, 'smooth', value=plotSpec$Plot[[plt]]$panel[[pnl]]$smooth[1])
    updateNumericInput (session, 'SGpoints', value=plotSpec$Plot[[plt]]$panel[[pnl]]$SGlength[1])
    updateNumericInput (session, 'lineV', value=1)
    updateSelectInput (session, 'addVarP', selected=plotSpec$Plot[[plt]]$panel[[pnl]]$var[1])
    updateSelectInput (session, 'varColor', selected=plotSpec$Plot[[plt]]$panel[[pnl]]$col[1])
    updateNumericInput (session, 'lineW', value=plotSpec$Plot[[plt]]$panel[[pnl]]$lw[1])
    updateRadioButtons (session, 'lineStyle', selected=ltyps[plotSpec$Plot[[plt]]$panel[[pnl]]$lt[1]])
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newbin <- reac$newbin + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('Panel: reset newdisplay etc.')}
  })
  obsPanel <- observe (exprPanel, quoted=TRUE)
  
  exprLineV <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$panel)
    lv <- input$lineV
    if (lv <= length (plotSpec$Plot[[plt]]$panel[[pnl]]$var)) {
      updateSelectInput (session, 'addVarP', selected=plotSpec$Plot[[plt]]$panel[[pnl]]$var[lv])
      updateSelectInput (session, 'varColor', selected=plotSpec$Plot[[plt]]$panel[[pnl]]$col[lv])
      updateNumericInput (session, 'lineW', value=plotSpec$Plot[[plt]]$panel[[pnl]]$lw[lv])
      updateNumericInput (session, 'SGpoints', value=plotSpec$Plot[[plt]]$panel[[pnl]]$SGlength[lv])
      updateCheckboxInput (session, 'smooth', value=plotSpec$Plot[[plt]]$panel[[pnl]]$smooth[lv])
      updateRadioButtons (session, 'lineStyle', selected=ltyps[plotSpec$Plot[[plt]]$panel[[pnl]]$lt[lv]])
    } else {
      v <- plotSpec$Plot[[plt]]$panel[[pnl]]$var
      v <- c(v, v[length(v)])
      plotSpec$Plot[[plt]]$panel[[pnl]]$var <<- v
      if (Trace) {print (sprintf ('LineV: var set to %s', v))}
      lbl <- plotSpec$Plot[[plt]]$panel[[pnl]]$lab
      lbl <- c(lbl, lbl[length(lbl)])
      plotSpec$Plot[[plt]]$panel[[pnl]]$lab <<- lbl
      cl <- plotSpec$Plot[[plt]]$panel[[pnl]]$col
      cl <- c(cl, cl[length(cl)])
      plotSpec$Plot[[plt]]$panel[[pnl]]$col <<- cl
      lw <- plotSpec$Plot[[plt]]$panel[[pnl]]$lw
      lw <- c(lw, lw[length(lw)])
      plotSpec$Plot[[plt]]$panel[[pnl]]$lw <<- lw
      lt <- plotSpec$Plot[[plt]]$panel[[pnl]]$lt
      lt <- c(lt, lt[length(lt)])
      plotSpec$Plot[[plt]]$panel[[pnl]]$lt <<- lt
      sm <- plotSpec$Plot[[plt]]$panel[[pnl]]$smooth
      sm <- c(sm, FALSE)
      plotSpec$Plot[[plt]]$panel[[pnl]]$smooth <<- sm
      sgl <- plotSpec$Plot[[plt]]$panel[[pnl]]$SGlength
      sgl <- c(sgl, 61)
      plotSpec$Plot[[plt]]$panel[[pnl]]$SGlength <- sgl
      updateSelectInput (session, 'addVarP', selected=v[length(v)])
      updateSelectInput (session, 'varColor', selected=cl[length(cl)])
      updateNumericInput (session, 'lineW', value=lw[length(lw)])
      updateNumericInput (session, 'lineStyle', value=lt[length(lt)])
      updateNumericInput (session, 'SGpoints', value=61)
      updateCheckboxInput (session, 'smooth', value=FALSE)
    }
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newbin <- reac$newbin + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('LineV: reset newdisplay')}
  })
  obsLineV <- observe (exprLineV, quoted=TRUE)
  
  
  exprHistVar <- quote ({
    
    isolate (plt <- input$plot)
    isolate (pnl <- input$hpanel)
    isolate (lv <- input$hlineV)
    
    if (Trace) {print (sprintf ('HistVar: entry with haddVarp=%s', input$haddVarP))}
    if (input$haddVarP != 'omit') {
      if (input$haddVarP == 'select') {
      } else {
        plotSpec$Hist[[plt]]$panel[[pnl]]$var[lv] <<- input$haddVarP
        if (((ld <- length(Data)) < 2) || (!(input$haddVarP %in% (nms <- names (Data))))) {
          if (exists ('specialData') && (input$haddVarP %in% names (specialData))) {
          } else {
            print ('need new data to include new variable - 1')
            if (Trace) {print (sprintf ('HistVar: haddVarP is %s', input$haddVarP))}
            if (Trace) {print (sprintf ('HistVar: length of data is %d', ld))}
            if (Trace) {print ('HistVar: names in data'); print (nms)}
            if (Trace) {print ('HistVar: reset newdata')}
            isolate(reac$newdata <- reac$newdata + 1)
          }
        }
      }
    } else {
      v <- plotSpec$Hist[[plt]]$panel[[pnl]]$var
      v <- v[-lv]
      if (Trace) {print (sprintf ('HistVar: new var list is %s', v))}
      plotSpec$Hist[[plt]]$panel[[pnl]]$var <<- v
      # nms <- names (data ())  ## just a data ref to get reset
      updateSelectInput (session, 'haddVarP', selected='select')  
    }
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('HistVar: reset newhistogram')}
  })
  obsHistVar <- observe (exprHistVar, priority=10, quoted=TRUE)
  
  
  exprScatVar1 <- quote ({
    
    plt <- isolate (input$plot)
    pnl <- isolate (input$spanel)
    lv <- isolate (input$slineV)
    
    if (input$saddVarP1 != 'omit') {
      if (input$saddVarP1 == 'select') {
      } else {
        plotSpec$Scat[[plt]]$panel[[pnl]]$varx <<- input$saddVarP1
        if (((ld <- length(Data)) < 2) || (!(input$saddVarP1 %in% (nms <- names (Data))))) {
          if (exists ('specialData') && (input$saddVarP1 %in% names (specialData))) {
          } else {
            print ('ScatVar1: need new data to include new variable - 2')
            if (Trace) {print (sprintf ('ScatVar1: saddVarP1 is %s', input$saddVarP1))}
            if (Trace) {print (sprintf ('ScatVar1: length of data is %d', ld))}
            if (Trace) {print ('ScatVar1: names in data are'); print (nms)}
            if (Trace) {print (sprintf ('ScatVar1: reset newdata'))}
            isolate(reac$newdata <- reac$newdata + 1)
          }
        }
      }
    } 
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('ScatVar1: reset newscat')}
  })
  obsScatVar1 <- observe (exprScatVar1, priority=10, quoted=TRUE)
  
  exprScatVar2 <- quote ({
    
    plt <- isolate (input$plot)
    pnl <- isolate (input$spanel)
    lv <- isolate (input$slineV)
    
    if (input$saddVarP2 != 'omit') {
      if (input$saddVarP2 == 'select') {
      } else {
        plotSpec$Scat[[plt]]$panel[[pnl]]$vary[lv] <<- input$saddVarP2
        if (((ld <- length(Data)) < 2) || (!(input$saddVarP2 %in% (nms <- names (Data))))) {
          if (exists ('specialData') && (input$saddVarP2 %in% names (specialData))) {
          } else {
            print ('need new data to include new variable - 3')
            if (Trace) {print (sprintf ('ScatVar2: saddVarP2 is %s', input$saddVarP2))}
            if (Trace) {print (sprintf ('ScatVar2: length of data is %d', ld))}
            if (Trace) {print ('ScatVar2: names in data are'); print (nms)}
            if (Trace) {print ('ScatVar2: reset newdata')}
            isolate(reac$newdata <- reac$newdata + 1)
          }
        }
      }
    } else {
      v <- plotSpec$Scat[[plt]]$panel[[pnl]]$vary
      v <- v[-lv]
      if (Trace) {print (sprintf ('ScatVar2: new var list is %s', v))}
      plotSpec$Scat[[plt]]$panel[[pnl]]$vary <<- v
      # nms <- names (data ())  ## just a data ref to get reset
      updateSelectInput (session, 'saddVarP2', selected='select')  
    } 
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('ScatVar2: reset newscat')}
  })
  obsScatVar2 <- observe (exprScatVar2, priority=10, quoted=TRUE)
  
  exprbPlotVarX <- quote ({
    
    plt <- isolate (input$plot)
    pnl <- isolate (input$bpanel)
    lv <- isolate (input$blineV)
    plotSpec$Bin[[plt]]$panel[[pnl]]$varx <<- input$baddVarP1
    if (((ld <- length(Data)) < 2) || (!(input$baddVarP1 %in% (nms <- names (Data))))) {
      if (exists ('specialData') && (input$baddVarP1 %in% names (specialData))) {
      } else {
        print ('need new data to include new variable - 5')
        reac$newdata <- reac$newdata + 1
        if (Trace) {
          print (sprintf ('bPlotVarX: baddVarP1 is %s', input$baddVarP1))
          print (sprintf ('bPlotVarX: length of data is %d', ld))
          print ('bPlotVarX: names in data are'); print (nms)
          print ('bPlotVarX: reset newdata')
        }
      }
    }
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bPlotVarX: reset newbin')}
  })
  obsbPlotVarX <- observe (exprbPlotVarX, quoted=TRUE, priority=-9)
  
  exprbPlotVar <- quote ({
    
    plt <- isolate (input$plot)
    pnl <- isolate (input$bpanel)
    lv <- isolate (input$blineV)
    
    if (input$baddVarP2 != 'omit') {
      if (input$baddVarP2 == 'select') {
      } else {
        plotSpec$Bin[[plt]]$panel[[pnl]]$vary[lv] <<- input$baddVarP2
        if ((length(Data) < 2) || (!(input$baddVarP2 %in% names (Data)))) {
          if (exists ('specialData') && (input$baddVarP2 %in% names (specialData))) {
          } else {
            print ('need new data to include new variable - 6')
            reac$newdata <- reac$newdata + 1
          }
        }
      }
    } else {
      v <- plotSpec$Bin[[plt]]$panel[[pnl]]$vary
      v <- v[-lv]
      if (Trace) {print (sprintf ('bPlotVar: new vary list is %s', v))}
      plotSpec$Bin[[plt]]$panel[[pnl]]$vary <<- v
      # nms <- names (data ())  ## just a data ref to get reset
      # updateSelectInput (session, 'saddVarP2', selected='select')  
    }
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('bPlotVar: reset newbin')}
  })
  obsbPlotVar <- observe (exprbPlotVar, quoted=TRUE, priority=-9)
  
  exprylbl <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate (input$panel)
    lv <- isolate (input$lineV)
    plotSpec$Plot[[plt]]$panel[[pnl]]$lab[lv] <<- input$ylbl
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('ylbl: reset newdisplay')}
  })
  obsylbl <- observe (exprylbl, quoted=TRUE)
  
  exprLineColor <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate (input$panel)
    lv <- isolate (input$lineV)
    plotSpec$Plot[[plt]]$panel[[pnl]]$col[lv] <<- input$varColor
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('LineColor: reset newdisplay')}
  })
  obsLineColor <- observe (exprLineColor, quoted=TRUE)
  
  exprsLineColor <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate (input$spanel)
    lv <- isolate (input$slineV)
    plotSpec$Scat[[plt]]$panel[[pnl]]$col[lv] <<- input$svarColor
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('sLineColor: reset newscat')}
  })
  obssLineColor <- observe (exprsLineColor, quoted=TRUE)
  
  exprLineWidth <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate (input$panel)
    lv <- isolate (input$lineV)
    plotSpec$Plot[[plt]]$panel[[pnl]]$lw[lv] <<- input$lineW
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('LineWidth: reset newdisplay')}
  })
  obsLineWidth <- observe (exprLineWidth, quoted=TRUE)
  
  exprSGlength <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate (input$panel)
    lv <- isolate (input$lineV)
    plotSpec$Plot[[plt]]$panel[[pnl]]$SGlength[lv] <<- input$SGpoints
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('SGpoints: reset newdisplay')}
  })
  obsSGlength <- observe (exprSGlength, quoted=TRUE)
  
  exprLineStyle <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate (input$panel)
    lv <- isolate (input$lineV)
    plotSpec$Plot[[plt]]$panel[[pnl]]$lt[lv] <<- which (input$lineStyle == ltyps)
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('LineStyle: reset newdisplay')}
  })
  obsLineStyle <- observe (exprLineStyle, quoted=TRUE)
  
  exprhLineColor <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate(input$hpanel)
    lv <- isolate (input$hlineV)
    plotSpec$Hist[[plt]]$panel[[pnl]]$col[lv] <<- input$hvarColor
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hLineColor: reset newhistogram')}
  })
  obshLineColor <- observe (exprhLineColor, quoted=TRUE)
  
  exprhLineWidth <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate(input$hpanel)
    lv <- isolate (input$hlineV)
    plotSpec$Hist[[plt]]$panel[[pnl]]$lw[lv] <<- input$hlineW
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hLineWidth: reset newhistogram')}
  })
  obshLineWidth <- observe (exprhLineWidth, quoted=TRUE)
  
  exprhLineStyle <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate(input$hpanel)
    lv <- isolate (input$hlineV)
    if (Trace) {print (sprintf ('hLineStyle: lineStyle is %s', input$hlineStyle))}
    plotSpec$Hist[[plt]]$panel[[pnl]]$lt[lv] <<- which (input$hlineStyle == ltyps)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('hLineStyle: reset newhistogram')}
  })
  obshLineStyle <- observe (exprhLineStyle, quoted=TRUE)
  
  exprRNumber <- quote ({
    if (input$rvNumber > nrow (plotSpec$Restrictions)) {
      newRow <- data.frame (RVAR=isolate (input$rvar), 
                            apply=isolate (input$apply),
                            min=isolate (input$rmin),
                            max=isolate (input$rmax))
      plotSpec$Restrictions <<- rbind (plotSpec$Restrictions, newRow)
    } else {
      updateSelectInput(session, 'rvar',  
                        selected=plotSpec$Restrictions$RVAR[input$rvNumber])
      updateCheckboxInput (session, 'apply',
                           value=plotSpec$Restrictions$apply[input$rvNumber])
      updateNumericInput(session, 'rmin', label=NULL, 
                         value=plotSpec$Restrictions$min[input$rvNumber])
      updateNumericInput(session, 'rmax', label=NULL, 
                         value=plotSpec$Restrictions$max[input$rvNumber])
    }
  })
  obsRNumber <- observe (exprRNumber, quoted=TRUE)
  
  exprRvar <- quote ({
    rvN <- isolate (input$rvNumber)
    plotSpec$Restrictions$RVAR[rvN] <<- input$rvar
    plotSpec$Restrictions$apply[rvN] <<- input$apply
    plotSpec$Restrictions$min[rvN] <<- input$rmin
    plotSpec$Restrictions$max[rvN] <<- input$rmax
    isolate (reac$newdata <- reac$newdata + 1)
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('Rvar: reset newdisplay etc.')}
  })
  obsRvar <- observe (exprRvar, quoted=TRUE)
  
  exprPaluchLWC <- quote ({
    plotSpec$paluchLWC <- input$paluchLWC
    isolate (reac$newdata <- reac$newdata + 1)
  })
  obsPaluchLWC <- observe (exprPaluchLWC, quoted=TRUE)
  
  exprVvar <- quote ({
    plt <- isolate(input$plot)
    plotSpec$Variance[[plt]]$Definition$var <<- input$specvar
    isolate (reac$newvarp <- reac$newvarp + 1)
    isolate (reac$newdata <- reac$newdata + 1)  ## may not be necessary?
    if (Trace) {print (sprintf ('Vvar: reset newvarp, specvar is %s', input$specvar))}
  })
  obsVvar <- observe (exprVvar, quoted=TRUE)
  
  exprCvar <- quote ({
    plt <- isolate(input$plot)
    plotSpec$Variance[[plt]]$Definition$cvar <<- input$speccovar
    isolate (reac$newvarp<- reac$newvarp + 1)
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print ('Cvar: reset newvarp')}
  })
  obsCvar <- observe (exprCvar, quoted=TRUE)
  
  exprFFTpts <- quote ({
    plt <- isolate (input$plot)
    fftpts <- input$fftpts
    ## enforce power-of-2
    fftpts <- 2 ^ (log (fftpts) %/% log(2))
    plotSpec$Variance[[plt]]$Definition$fftpts <<- fftpts
    if (fftpts != input$fftpts) {
      updateNumericInput (session, 'fftpts', value=fftpts)
    }
    isolate (reac$newvarp<- reac$newvarp + 1)
    if (Trace) {print ('VVTpts: reset newvarp')}
  })
  obsFFTpts <- observe (exprFFTpts, quoted=TRUE)
  
  exprFFTwdw <- quote ({
    plt <- isolate (input$plot)
    plotSpec$Variance[[plt]]$Definition$fftwindow <<- input$fftwindow
    isolate (reac$newvarp<- reac$newvarp + 1)
    if (Trace) {print ('FFTwdw: reset newvarp')}
  })
  obsFFTwdw <- observe (exprFFTwdw, quoted=TRUE)
  
  exprFFTavg <- quote ({
    plt <- isolate (input$plot)
    plotSpec$Variance[[plt]]$Definition$fftavg <<- input$fftavg
    isolate (reac$newvarp<- reac$newvarp + 1)
    if (Trace) {print ('FFTavg: reset newvarp')}
  })
  obsFFTavg <- observe (exprFFTavg, quoted=TRUE)
  
  exprFFTtype <- quote ({
    plt <- isolate (input$plot)
    plotSpec$Variance[[plt]]$Definition$ffttype <<- input$ffttype
    isolate (reac$newvarp<- reac$newvarp + 1)
    if (Trace) {print ('FFTtype: reset newvarp')}
  })
  obsFFTtype <- observe (exprFFTtype, quoted=TRUE)
  
  exprMEMtype <- quote ({
    plt <- isolate (input$plot)
    plotSpec$Variance[[plt]]$Definition$MEMtype <<- input$MEMtype
    isolate (reac$newvarp<- reac$newvarp + 1)
    if (Trace) {print ('MEMtype: reset newvarp')}
  })
  obsMEMtype <- observe (exprMEMtype, quoted=TRUE)
  
  exprMEMavg <- quote ({
    plt <- isolate (input$plot)
    plotSpec$Variance[[plt]]$Definition$MEMavg <<- input$MEMavg
    isolate (reac$newvarp<- reac$newvarp + 1)
    if (Trace) {print ('MEMavg: reset newvarp')}
  })
  obsMEMavg <- observe (exprMEMavg, quoted=TRUE)
  
  exprMEMpoles <- quote ({
    plt <- isolate (input$plot)
    plotSpec$Variance[[plt]]$Definition$MEMpoles <<- input$MEMpoles
    isolate (reac$newvarp<- reac$newvarp + 1)
    if (Trace) {print ('MEMpoles: reset newvarp')}
  })
  obsMEMpoles <- observe (exprMEMpoles, quoted=TRUE)
  
  exprMEMres <- quote ({
    plt <- isolate (input$plot)
    plotSpec$Variance[[plt]]$Definition$MEMres <<- input$MEMres
    isolate (reac$newvarp<- reac$newvarp + 1)
    if (Trace) {print ('MEMres: reset newvarp')}
  })
  obsMEMres <- observe (exprMEMres, quoted=TRUE)
  
  exprHistBins <- quote({
    isolate (plt <- input$plot)
    isolate (pnl <- input$hpanel)
    plotSpec$Hist[[plt]]$panel[[pnl]]$bins <<- input$hbins
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('HistBins: reset newhist')}
  })
  obsHistBins <- observe (exprHistBins, quoted=TRUE)
  
  exprLfit <- quote ({
    input$fformula
    VF <- isolate (input$response)
    VarList <<- makeVarList()
    choices <- VarList
    if (exists ('specialData')) {
      specialNames <- names (specialData)
      specialNames <- specialNames[specialNames != 'Time']
      choices <- c(choices, specialNames)
    }
    updateSelectInput (session, 'response', choices=sort (choices), selected=VF)
  })
  obsLfit <- observe (exprLfit, quoted=TRUE)
  
  
  observeEvent (input$specSave, saveConfig (input))
  observeEvent (input$specRead, 
                {loadConfig (input)
                  ## When a configuration is loaded, the entire ui needs to be
                  ## updated. The order needs to be: Change everything needed
                  ## to get new data first, with 'freezeReactiveValues()', then
                  ## allow data access to update.
                  # updateSelectInput (session, 'Project', selected=plotSpec$Project)
                  ##
                  # get the full list of input variables
                  InputNames <<- names(input)
                  quickPlotVar <<- ''  ## reset to avoid not-found errors
                  CH <- sort(FI$Variables)  ## for the variable names requiring choices
                  ch.var <- c('addVarP', 'haddVarP', 'saddVarP1', 'saddVarP2', 'baddVarP1',
                              'baddVarP2', 'paluchLWC', 'specvar', 'speccovar', 'rvar')
                  # print (FI$Variables)
                  # freezeReactiveValue (input, 'Project')
                  # freezeReactiveValue (input, 'Flight')
                  # freezeReactiveValue (input, 'addVarP')
                  # freezeReactiveValue (input, 'haddVarP')
                  updateSliderInput (session, 'times', value=plotSpec$Times)
                  updateTextInput (session, 'tstart', value=formatTime (plotSpec$Times[1]))
                  updateTextInput (session, 'tend',   value=formatTime (plotSpec$Times[2]))
                  updateTextInput (session, 'paluchStart', value=formatTime (plotSpec$PaluchTimes[1]))
                  updateTextInput (session, 'paluchEnd', value=formatTime (plotSpec$PaluchTimes[2]))
                  updateTextInput (session, 'paluchCStart', value=formatTime (plotSpec$PaluchCTimes[1]))
                  updateTextInput (session, 'paluchCEnd', value=formatTime (plotSpec$PaluchCTimes[2]))
                  updateSelectInput (session, 'paluchLWC', choices=sort(FI$Variables), 
                                     selected=plotSpec$paluchLWC)
                  ## checkboxes
                  for (i in 1:nrow(InputDF)) {
                    if (InputDF$Type[i] == 'cB') {
                      # freezeReactiveValue (input, InputDF$ID[i])
                      updateCheckboxInput (session, InputDF$ID[i], value=FALSE)
                    } else {
                      if (!is.na(InputDF$Force[i])) {
                        # freezeReactiveValue (input, InputDF$ID[i])
                        if (InputDF$Type[i] == 'nI') {
                          updateNumericInput(session, InputDF$ID[i], value=InputDF$Force[i])
                        } else if (InputDF$Type[i] == 'sI') {
                          updateSelectInput(session, InputDF$ID[i], value=InputDF$Force[i])
                        }
                      } else {
                        if (InputDF$I1[i] == 0) {
                          vvv <- NA
                          next
                        } else {
                          if (InputDF$I2[i] == 0) {
                            vvv <- plotSpec[[InputDF$I1[i]]]
                          } else {
                            if (InputDF$I3[i] == 0) {
                              vvv <- plotSpec[[InputDF$I1[i]]][[InputDF$I2[i]]]
                            } else {
                              if (InputDF$I4[i] == 0) {
                                vvv <- plotSpec[[InputDF$I1[i]]][[InputDF$I2[i]]][[InputDF$I3[i]]]
                              } else {
                                if (InputDF$I5[i] == 0) {
                                  vvv <- plotSpec[[InputDF$I1[i]]][[InputDF$I2[i]]][[InputDF$I3[i]]][[InputDF$I4[i]]]
                                } else {
                                  if (InputDF$I6[i] == 0) {
                                    vvv <- plotSpec[[InputDF$I1[i]]][[InputDF$I2[i]]][[InputDF$I3[i]]][[InputDF$I4[i]]][[InputDF$I5[i]]]
                                  } else {
                                    vvv <- plotSpec[[InputDF$I1[i]]][[InputDF$I2[i]]][[InputDF$I3[i]]][[InputDF$I4[i]]][[InputDF$I5[i]]][[InputDF$I6[i]]]
                                  }
                                }
                              }
                            }
                          }
                        }
                        
                        if (InputDF$Type[i] == 'nI') {
                          updateNumericInput(session, InputDF$ID[i], value=vvv)
                        } else if (InputDF$Type[i] == 'Rb') {
                          updateRadioButtons(session, InputDF$ID[i], selected=vvv)
                        } else if (InputDF$Type[i] == 'sI') {
                          if (InputDF$ID[i] %in% ch.var) {
                            updateSelectInput(session, InputDF$ID[i], selected=vvv, choices=CH)
                            # if (i == 100) {print (sprintf ('update %s choices',vvv));print(CH)}
                          } else {
                            print (sprintf('ID=%s, i=%d, vvv=%s', InputDF$ID[i], i, vvv))
                            updateSelectInput(session, InputDF$ID[i], selected=vvv)
                          }
                        }
                      }
                    }
                  }
                  
                  # plt <- 1; pnl <- 1; lv <- 1; CC <- sort(FI$Variables)
                  updateSelectInput(session, inputId='Project',
                                    selected=plotSpec$Project, choices=PJ)
                  # updateSelectInput (session, 'addVarP', 
                  #                    selected=plotSpec$Plot[[1]]$panel[[1]]$var[1])
                  # updateNumericInput (session, 'Flight', value=plotSpec$Flight)
                  # updateRadioButtons (session, 'typeFlight', selected=plotSpec$TypeFlight)
                  # updateRadioButtons (session, 'SRC', selected=plotSpec$SRC)
                  # updateNumericInput (session, 'plot', value=1)
                  # updateNumericInput (session, 'panels', value=plotSpec$Plot[[1]]$panels)
                  # updateNumericInput (session, 'cols', value=plotSpec$Plot[[1]]$columns)
                  # updateNumericInput (session, 'panel', value=1)
                  # updateCheckboxInput (session, 'logY', value=plotSpec$Plot[[1]]$panel[[1]]$logY)
                  # updateCheckboxInput (session, 'fixed', value=plotSpec$Plot[[1]]$panel[[1]]$fixed)
                  # updateCheckboxInput (session, 'smooth', value=plotSpec$Plot[[1]]$panel[[1]]$smooth[1])
                  # updateNumericInput (session, 'SGpoints', value=plotSpec$Plot[[1]]$panel[[1]]$SGlength[1])
                  # updateNumericInput (session, 'panelMin', value=plotSpec$Plot[[1]]$panel[[1]]$ylim[1])
                  # updateNumericInput (session, 'panelMax', value=plotSpec$Plot[[1]]$panel[[1]]$ylim[2])
                  # updateNumericInput (session, 'lineV', value=1)
                  # updateSelectInput (session, 'varColor', selected=plotSpec$Plot[[1]]$panel[[1]]$col[1])
                  # updateNumericInput (session, 'lineW', value=plotSpec$Plot[[1]]$panel[[1]]$lw[1])
                  # updateRadioButtons (session, 'lineStyle', selected=plotSpec$Plot[[1]]$panel[[1]]$lt[1])
                  # 
                  # updateNumericInput (session, 'hpanels', value=plotSpec$Hist[[1]]$panels)
                  # updateNumericInput (session, 'hcols', value=plotSpec$Hist[[1]]$columns)
                  # updateNumericInput (session, 'hpanel', value=1)
                  # updateCheckboxInput (session, 'hlogY', value=plotSpec$Hist[[1]]$panel[[1]]$logY)
                  # updateCheckboxInput (session, 'hfixed', value=plotSpec$Hist[[1]]$panel[[1]]$fixed)
                  # updateNumericInput (session, 'hpanelMin', value=plotSpec$Hist[[1]]$panel[[1]]$ylim[1])
                  # updateNumericInput (session, 'hpanelMax', value=plotSpec$Hist[[1]]$panel[[1]]$ylim[2])
                  # updateNumericInput (session, 'hlineV', value=1)
                  # updateSelectInput (session, 'haddVarP', 
                  #                    choices=c('select', 'omit', sort(FI$Variables)),
                  #                    selected=plotSpec$Hist[[1]]$panel[[1]]$var[1])
                  # updateSelectInput (session, 'hvarColor', selected=plotSpec$Hist[[1]]$panel[[1]]$col[1])
                  # updateNumericInput (session, 'hlineW', value=plotSpec$Hist[[1]]$panel[[1]]$lw[1])
                  # updateRadioButtons (session, 'hlineStyle', selected=plotSpec$Hist[[1]]$panel[[1]]$lt[1])
                  # updateTextInput (session, 'fnametext', value=plotSpec$fname2d)
                  isolate (reac$newdata <- reac$newdata + 1)
                  isolate (reac$newdisplay <- reac$newdisplay + 1)
                  isolate (reac$newhistogram <- reac$newhistogram + 1)
                  isolate (reac$newstats <- reac$newstats + 1)
                  isolate (reac$newscat <- reac$newscat + 1)
                  isolate (reac$newbin <- reac$newbin + 1)
                } )
  #   observeEvent (input$savePDF,
  #                 savePDF (Data=data(), inp=input))
  #   observeEvent (input$savePNG,
  #                 savePNG (Data=data(), inp=input))
  observeEvent (input$saveRdata,
                saveRdata (Data=data(), inp=input))
  observeEvent (input$nextT, {
    dt <- difftime (plotSpec$Times[2], plotSpec$Times[1])
    plotSpec$Times[1] <<- plotSpec$Times[1] + dt
    plotSpec$Times[2] <<- plotSpec$Times[2] + dt
    updateSliderInput (session, 'times', value=plotSpec$Times)
    updateTextInput (session, 'tstart', value=formatTime (plotSpec$Times[1]))
    updateTextInput (session, 'tend',   value=formatTime (plotSpec$Times[2]))
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newstats <- reac$newstats + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    isolate (reac$newbin <- reac$newbin + 1)
  } )  
  observeEvent (input$prevT, {
    dt <- difftime (plotSpec$Times[2], plotSpec$Times[1])
    plotSpec$Times[1] <<- plotSpec$Times[1] - dt
    plotSpec$Times[2] <<- plotSpec$Times[2] - dt
    updateSliderInput (session, 'times', value=plotSpec$Times)
    updateTextInput (session, 'tstart', value=formatTime (plotSpec$Times[1]))
    updateTextInput (session, 'tend',   value=formatTime (plotSpec$Times[2]))
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newstats <- reac$newstats + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    isolate (reac$newbin <- reac$newbin + 1)
  } )
  observeEvent (input$resetT, {
    print (sprintf ('reached resetT, plotSpec$Times=%s %s', plotSpec$Times[1], plotSpec$Times[2]))
    print (sprintf ('Data times are %s %s', Data$Time[1], Data$Time[nrow(Data)]))
    # global times?
    plotSpec$Times[1] <<- Data$Time[1]
    plotSpec$Times[2] <<- Data$Time[nrow(Data)]
    step <- 60
    minT <- Data$Time[1]
    minT <- minT - as.integer (minT) %% step + step
    maxT <- Data$Time[nrow(Data)]
    maxT <- maxT - as.integer (maxT) %% step 
    times <- c(minT, maxT)
    if (plotSpec$Times[1] > times[1]) {times <- c(plotSpec$Times[1], maxT)}
    if (plotSpec$Times[2] < times[2]) {times <- c(times[1], plotSpec$Times[2])}
    plotSpec$Times <- times
    print (sprintf ('resetting times to %s %s', plotSpec$Times[1], plotSpec$Times[2]))
    updateSliderInput (session, 'times', value=plotSpec$Times)
    updateTextInput (session, 'tstart', value=formatTime (plotSpec$Times[1]))
    updateTextInput (session, 'tend',   value=formatTime (plotSpec$Times[2]))
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newstats <- reac$newstats + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    isolate (reac$newbin <- reac$newbin + 1)
  } )
  observeEvent (input$next2d, {
    isolate (reac$new2d <- reac$new2d + 1)
  })
  observeEvent (input$prev2d, {
    psn <- seek (cfile, where=NA, rw='rb')
    psn <- psn - 4096*2 - 40
    if (psn > 4096+20) {
      seek (cfile, where=psn, 'rb', origin='start')
    }
    # psn <- seek (cfile, where=NA, rw='rb')
    isolate (reac$new2d <- reac$new2d + 1)
  })
  
  observeEvent (input$fname2d, {
    if (plotSpec$SRC != 'NCAR') {
      newwd <- sprintf ('%s%s/%s', DataDirectory (), plotSpec$SRC,
                        plotSpec$Project)
    } else {
      newwd <- sprintf('%s%s', DataDirectory (), plotSpec$Project)
    }
    plotSpec$fname2d <<- fileChoose (newwd)
    rm(cfile, pos=1)
    updateTextInput (session, 'fnametext', value=plotSpec$fname2d)
    isolate(reac$new2d <- reac$new2d + 1)
  })
  
  observeEvent (input$createV, {
    TX <- input$formla
    m <- gregexpr('[[:alnum:]]+', TX)
    V <- regmatches(TX, m)[[1]]
    V <- V[grepl('[[:upper:]]', V)]
    print (sprintf ('required variables are %s', V))
    ## if a requested variable is not present, get new data:
    nms <- names (data ())
    needAddedVariables <- FALSE
    for (VV in V) {
      if (!(VV %in% nms)) {
        addedVariables <<- c(addedVariables, VV)
        print (sprintf (' need to add variable %s to data', VV))
        print (sprintf (' list of added Variables is:'))
        print (addedVariables)
        # reac$newdata <- reac$newdata + 1
        needAddedVariables <- TRUE
      }
    } 
    if (needAddedVariables) {
      reac$newdata <- reac$newdata + 1
    }
    nv <- input$newvar
    assign (nv, with (data (), eval (parse (text=input$formla))))
    isolate (print (summary (eval(parse(text=input$newvar)))))
    if (!exists ('specialData')) {
      specialData <<- data.frame ('Time'=data()$Time)
    }
    specialData[, nv] <<- eval(parse(text=nv))
    FI$Variables <<- c(FI$Variables, nv)
    print (sprintf (' adding %s to FI$Variables', nv))
    isolate (plt <- input$plot)
    isolate (pnl <- input$panel)
    isolate (hpnl <- input$hpanel)
    isolate (spnl <- input$spanel)
    isolate (bpnl <- input$bpanel)
    isolate (lv <- input$lineV)
    isolate (hlv <- input$hlineV)
    isolate (slv <- input$slineV)
    isolate (blv <- input$blineV)
    isolate (rlv <- input$rvNumber)
    choices <- c('select', 'omit',sort(FI$Variables))
    print (sprintf (' setting variable choices to this list:'))
    print (sort(FI$Variables))
    updateSelectInput (session, 'addVarP', choices=choices,
                       selected=plotSpec$Plot[[plt]]$panel[[pnl]]$var[lv])
    updateSelectInput (session, 'haddVarP', choices=choices,
                       selected=plotSpec$Hist[[plt]]$panel[[hpnl]]$var[hlv])
    updateSelectInput (session, 'saddVarP1', choices=choices,
                       selected=plotSpec$Scat[[plt]]$panel[[spnl]]$varx)
    updateSelectInput (session, 'saddVarP2', choices=choices,
                       selected=plotSpec$Scat[[plt]]$panel[[spnl]]$vary[slv])
    updateSelectInput (session, 'baddVarP1', choices=choices,
                       selected=plotSpec$Bin[[plt]]$panel[[bpnl]]$varx)
    updateSelectInput (session, 'baddVarP2', choices=choices,
                       selected=plotSpec$Bin[[plt]]$panel[[bpnl]]$vary[blv])
    updateSelectInput (session, 'specvar', choices=choices, selected=plotSpec$Variance[[1]]$Definition$var)
    updateSelectInput (session, 'speccovar', choices=choices, selected=plotSpec$Variance[[1]]$Definition$cvar)
    updateSelectInput (session, 'rvar', choices=choices,
                       selected=plotSpec$Restrictions$RVAR[rlv])
    VF <- isolate (input$response)
    updateSelectInput (session, 'response', choices=sort(VarList), selected=VF)
    ## force re-read to get this variable added to data:
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {
      print ('createV: reset newdata')
      print ('createV: str(specialData is:')
      print (str(specialData))
    }
  })
  
  # observeEvent (input$prev, Repeat (-1))
  observeEvent (input$statVariables, {
    chooseVar (fname, inp=input)
    ## check if any requested variables not present in Data:
    if (any (!(plotSpec$StatVar %in% VarList))) {
      VarList <<- unique (c(VarList, plotSpec$StatVar))
      # print (c(VarList, plotSpec$StatVar))
      isolate (reac$newdata <- reac$newdata + 1)
    }
    isolate (reac$newstats <- reac$newstats + 1)
  })
  observeEvent (input$check, {
    chooseQVar (fname)
    ## check if any requested variables not present in Data:
    if (any (!(quickPlotVar %in% VarList))) {
      VarList <<- unique (c(VarList, quickPlotVar))
      # print (c(VarList, quickPlotVar))
      isolate (reac$newdata <- reac$newdata + 1)
    }
    isolate (reac$quick <- reac$quick + 1)
  })
  observeEvent (input$xfrVariables, {
    chooseXfrVar (fname, inp=input)
    ## check if any requested variables not present in Data:
    if (any (!(xVarList %in% VarList))) {
      VarList <<- unique (c(VarList, xVarList))
      # print (c(VarList, xVarList))
      isolate (reac$newdata <- reac$newdata + 1)
    }
  })
  observeEvent (input$lfit, {
    TX <- input$fformula
    m <- gregexpr('[[:alnum:]]+', TX)
    V <- regmatches(TX, m)[[1]]
    V <- V[grepl('[[:upper:]]', V)]
    V <- V[V != 'I']
    ## if a requested variable is not present, get new data:
    nms <- names (data ())
    for (VV in V) {
      if (!(VV %in% nms)) {
        if (VV %in% FI$Variables) {
          addedVariables <<- c(addedVariables, VV)
          isolate (reac$newdata <- reac$newdata + 1)
        } else {
          print (sprintf ('error, requested variable %s not found'), VV)
          return ()
        }
      }
    }
    DataF <- limitData (data (), input, lim=input$limitsFit)
    DataF <- DataF[DataF$Time >= plotSpec$Times[1] & DataF$Time < plotSpec$Times[2], ]
    fitm <<- lm(paste(parse (text=isolate(input$response)), '~',  parse (text=isolate(input$fformula)),
                      sep=''), data=DataF, y=TRUE)
    print (summary (fitm))
    print (anova (fitm))
    isolate (reac$updatefit <- reac$updatefit + 1)
  })
  observeEvent (input$ncplot, OpenInProgram (data(), Program=input$otherprogram, warnOverwrite=FALSE))
  observeEvent (input$Xanadu, OpenInProgram (data(), 'Xanadu', warnOverwrite=FALSE))
  observeEvent (input$maneuvers, SeekManeuvers (data ()))
  observeEvent (input$manual, seeManual ())
  
  
  
  
  ################ REACTIVES ########################
  
  reac <- reactiveValues (newdata=0, newdisplay=0, newtrack=0, 
                          newstats=0, newhistogram=0, newscat=0, 
                          newbin=0, newskewT=1, newvarp=0, updatefit=0, new2d=0, quick=0)
  SRCreac <- reactive ({              ## SRC
    ## reset SRC to 'NCAR'
    updateRadioButtons (session, 'SRC', label=NULL, selected='NCAR')
    'rf'
  })
    
  flightType <- reactive ({              ## typeFlight
    ## reset typeFlight to rf
    updateRadioButtons (session, 'typeFlight', label=NULL, selected='rf')
    'rf'
  })
  
  data <- reactive({                     ## data
    if (Trace) {
      print (sprintf ('data: entered, newdata is %d', reac$newdata))
    }
    ## I don't know why these are needed, but apparently they are on loadConfig
    updateSelectInput(session, inputId='SRC',
                      selected=plotSpec$SRC)
    updateSelectInput(session, inputId='Project',
                      selected=plotSpec$Project, choices=PJ)
    updateSliderInput (session, inputId='times', value=plotSpec$Times, min=plotSpec$Times[1],
                       max=plotSpec$Times[2])
    # Project <<- Project <- isolate(input$Project)
    reac$newdata
    # isolate (reac$newdisplay <- reac$newdisplay + 1)
    # isolate (reac$newskewT <- reac$newskewT + 1)
    ## these would be needed for translation to new cal coefficients
    ## VarList <- c(VarList, "RTH1", "RTH2", "RTF1")
    
    if (grepl ('HIPPO', plotSpec$Project)) {
      if (plotSpec$TypeFlight == 'F') {
        fname <<- sprintf ('%sHIPPO/%srf%02dF.nc', DataDirectory (), plotSpec$Project,
                            plotSpec$Flight)
      } else {
        fname <<- sprintf ('%sHIPPO/%s%s%02d.nc', DataDirectory (), plotSpec$Project,
                         plotSpec$TypeFlight, plotSpec$Flight)
      }    
    } else {
      if (plotSpec$TypeFlight == 'F') {
        fname <<- sprintf ('%s%s/%srf%02dF.nc', DataDirectory (), plotSpec$Project,
                           plotSpec$Project, plotSpec$Flight)
        if (Trace) {print (sprintf ('in data, file name is %s', fname))}
      } else if (plotSpec$TypeFlight == 'KF') {
        fname <<- sprintf ('%s%s/%srf%02dKF.nc', DataDirectory (), plotSpec$Project,
                           plotSpec$Project, plotSpec$Flight)
      } else {
        if (plotSpec$SRC != 'NCAR') {
          fname <<- sprintf ('%s%s/%s/%s%s%02d.nc', DataDirectory (),
                             plotSpec$SRC, plotSpec$Project, plotSpec$Project,
                             plotSpec$TypeFlight, plotSpec$Flight)
        } else {
          fname <<- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), plotSpec$Project,
                       plotSpec$Project, plotSpec$TypeFlight, plotSpec$Flight)
        }
      }
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
    if (Trace) {print (sprintf ('data: fname=%s', fname))}
    # reac$newdisplay <- reac$newdisplay + 1
    if (file.exists(fname)) {
      FI <<- DataFileInfo (fname, LLrange=FALSE)
      ## set list of available probes
      CHP <- vector ('character')
      if (any (grepl ('CCDP_', FI$Variables))) {CHP <- c(CHP, 'CDP')}
      if (any (grepl ('CS100_', FI$Variables))) {CHP <- c(CHP, 'FSSP')}
      if (any (grepl ('CUHSAS_', FI$Variables))) {CHP <- c(CHP, 'UHSAS')}
      if (any (grepl ('CS200_', FI$Variables))) {CHP <- c(CHP, 'PCASP')}
      if (any (grepl ('^C1DC_', FI$Variables))) {CHP <- c(CHP, '2DC')}
      updateCheckboxGroupInput (session, 'probe', choices=CHP, selected=CHP)
      if (exists ('specialData')) {
        FI$Variables <- unique(c(FI$Variables, names (specialData)[-1]))
      }
      VarList <- makeVarList ()  
      if ('CDP' %in% CHP) {VarList <- c(VarList, 'CCDP_')}
      if ('FSSP' %in% CHP) {VarList <- c(VarList, 'CS100_')}
      if ('UHSAS' %in% CHP) {VarList <- c(VarList, 'CUHSAS_')}
      if ('PCASP' %in% CHP) {VarList <- c(VarList, 'CS200_')}
      if ('2DC' %in% CHP) {VarList <- c(VarList, 'C1DC_')}
      if ('GGVSPD' %in% VarList && !('GGVSPD' %in% FI$Variables)) {
        if ('GGVSPDB' %in% FI$Variables) {
          VarList[which('GGVSPD' == VarList)] <- 'GGVSPDB'
        } else if ('VSPD_A' %in% FI$Variables) {
          VarList[which('GGVSPD' == VarList)] <- 'VSPD_A'
        } else if ('VSPD_G' %in% FI$Variables) {
          VarList[which('GGVSPD' == VarList)] <- 'VSPD_G'
        }
      }
      VarList <<- VarList ## saved as global for possible inspection
      # if (exists ('specialData')) {rm (specialData, pos=1)} 
      if ((fname != fname.last) || (any(!(VarList %in% VarListLast)))) {
	if (Trace) {print (c(sprintf ('reading data from %s; VarList is:', fname), VarList))}
        D <- getNetCDF (fname, VarList)
        if ('GGVSPDB' %in% VarList) {
          D$GGVSPD <- D$GGVSPDB
        } else if ('VSPD_A' %in% VarList) {
          D$GGVSPD <- D$VSPD_A
        } else if ('VSPD_G' %in% VarList) {
          D$GGVSPD <- D$VSPD_G
        }
        ## beware of cases with a long string of NAs at the start of the flight
        if ('TASX' %in% names (D)) {
          ix <- which (!is.na(D$TASX))
          TatStart <- D$Time[ix[1]]
          TatEnd <- D$Time[ix[length(ix)]]
          DS <- D
          D <- D[D$Time >= TatStart & D$Time <= TatEnd, ]
          D <- transferAttributes (D, DS)
          rm (DS)
        }
        if (fname != fname.last) {
          # plotSpec$Times <<- c(D$Time[1], D$Time[nrow(D)])
          step <- 60
          minT <- D$Time[1]
          minT <<- minT <- minT - as.integer (minT) %% step + step
          maxT <- D$Time[nrow(D)]
          maxT <<- maxT <- maxT - as.integer (maxT) %% step
          # plotSpec$Times <<- c(D$Time[1], D$Time[nrow(D)])
          plotSpec$Times <<- c(minT, maxT)
          if (Trace) {print (sprintf ('data: setting plotSpec$Times to %s %s', 
                                      formatTime (minT), formatTime (maxT)))}
          updateSliderInput (session, 'times', value=plotSpec$Times, min=minT, max=maxT)
          updateNumericInput (session, 'tstart', value=formatTime (plotSpec$Times[1]))
          updateNumericInput (session, 'tend', value=formatTime (plotSpec$Times[2]))
          updateTextInput (session, 'paluchStart', value=formatTime (plotSpec$PaluchTimes[1]))
          updateTextInput (session, 'paluchEnd', value=formatTime (plotSpec$PaluchTimes[2]))
          updateTextInput (session, 'paluchCStart', value=formatTime (plotSpec$PaluchCTimes[1]))
          updateTextInput (session, 'paluchCEnd', value=formatTime (plotSpec$PaluchCTimes[2]))
        }
        if (Trace) {print (sprintf ('data: loaded data.frame from %s', fname))}
      } else {  ## fname is the same, so reuse Data
        D <- Data
      }
      if (exists ('specialData')) {
	  if (!('ROC' %in% names(specialData))) { ## specialVar adds ROC+
            specialData <<- cbind(specialData, specialVar (D))
	  }
      } else {
        specialData <<- specialVar (D)
      }
      if (Trace) {
        print ('names in Data:')
        print (sort(names(Data)))
        print ('names in specialData:')
        print (sort(names(specialData)))
      }
      if (exists ('specialData')) {
        SD <- specialData
        if ('Time' %in% names (SD)) {
          SD$Time <- NULL
        }
        ## skip if variables are already in D
        if (all (names (SD) %in% names (D))) {
	} else {
          DS <- D
          D <- cbind (D, SD)
          D <- transferAttributes (D, DS)
          rm(DS)
        }
      }
      ## remove duplicates:
      NMD <- names(D)
      if (length(unique(NMD)) < length(NMD)) {
	      DS <- D[, NMD]
	      D <- transferAttributes (DS, D)
	      rm (DS)
      }
      if (exists ('specialData')) {
        FI$Variables <<- unique(c(FI$Variables, names (specialData)[-1]))
      }
      if (length (D) > 1) {
        fname.last <<- fname
        VarListLast <<- VarList
        updateSelectInput (session, 'specvar', label=NULL, choices=sort(FI$Variables),  
                           selected=plotSpec$Variance[[1]]$Definition$var)
        updateSelectInput (session, 'speccovar', label=NULL, choices=sort(FI$Variables),  
                           selected=plotSpec$Variance[[1]]$Definition$cvar)
        updateSelectInput (session, 'addVarP', label=NULL, choices=c('select', 'omit', sort(FI$Variables)), 
                           selected=plotSpec$Plot[[input$plot]]$panel[[input$panel]]$var[1])
        updateSelectInput (session, 'haddVarP', label=NULL, choices=c('select', 'omit', sort(FI$Variables)), 
                           selected=plotSpec$Hist[[input$plot]]$panel[[input$panel]]$var[1])
        updateSelectInput (session, 'saddVarP', label=NULL, choices=c('select', 'omit', sort(FI$Variables)), 
                           selected=plotSpec$Scat[[input$plot]]$panel[[input$panel]]$var[1])
        updateSelectInput (session, 'baddVarP', label=NULL, choices=c('select', 'omit', sort(FI$Variables)), 
                           selected=plotSpec$Bin[[input$plot]]$panel[[input$panel]]$var[1])
        Data <<- D
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
      if (plotSpec$SRC != 'NCAR') {
        fn <<- sprintf ('%s%s/%s/%s%s%02d.nc', DataDirectory (),
                         plotSpec$SRC, plotSpec$Project, plotSpec$Project,
                         'tf', plotSpec$Flight)
      } else {
        fn <- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), plotSpec$Project,
                       plotSpec$Project, 'tf', plotSpec$Flight)
      }
      if (file.exists (fn)) {
        warning (sprintf ('switched to tf%02d because rf%02d does not exist',
                          plotSpec$Flight, plotSpec$Flight))
        updateRadioButtons (session, 'typeFlight', label=NULL, selected='tf')
        plotSpec$TypeFlight <<- 'tf'
        return (getNetCDF (fn, VarList))
      } else {
        if (Trace) {print ('data: error in data, returning -1')}
        return (-1)
      }
    }
  })
  
  
  ################ OUTPUTS ########################
  
  plotMain <- function (input) {
    
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Plot[[input$plot]]$restrict)
    plt <- isolate (input$plot)
    spec <- plotSpec$Plot[[plt]]
    nrws <- ceiling ((spec$panels) / spec$columns)
    nmx <- nrws * spec$columns
    layout(matrix(1:nmx, ncol = spec$columns), widths = 1, 
           heights = c(rep(5,spec$panels-1),6))
    op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
    sp <- max (input$panel, spec$panels)
    for (pnl in 1:sp) {
      if ((pnl == sp) || (pnl %% nrws == 0)) {
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
      if (spec$panel[[pnl]]$fixed) {yl <- spec$panel[[pnl]]$ylim}
      par(cex=1.5)
      v <- spec$panel[[pnl]]$var
      print (sprintf('v: %s %s', v[1], v[2]))
      print (sprintf ('smooth: %d %d', spec$panel[[pnl]]$smooth[1], spec$panel[[pnl]]$smooth[2]))
      isolate (SGL <- spec$panel[[pnl]]$SGlength)
      for (i in 1:length(v)) {
        if (spec$panel[[pnl]]$smooth[i]) {
          DataV[,v[i]] <- SmoothInterp (DataV[ ,v[i]], .Length=SGL[i])
          DataR[,v[i]] <- SmoothInterp (DataR[ ,v[i]], .Length=SGL[i])
        }
      }
      if (plotSpec$Plot[[input$plot]]$restrict) {
        if (is.null (yl)) {
          plotWAC (DataV[, c('Time', spec$panel[[pnl]]$var)], log=logY,
                   ylab=spec$panel[[pnl]]$lab[1],
                   col=spec$panel[[pnl]]$col,
                   lwd=spec$panel[[pnl]]$lw,
                   lty=spec$panel[[pnl]]$lt)  
        } else {
          plotWAC (DataV[, c('Time', spec$panel[[pnl]]$var)], ylim=yl, log=logY,
                   ylab=spec$panel[[pnl]]$lab[1],
                   col=spec$panel[[pnl]]$col,
                   lwd=spec$panel[[pnl]]$lw,
                   lty=spec$panel[[pnl]]$lt)  
        }
      } else {
        if (is.null (yl)) {
          ## allow expressions in y-axis label:
          ylabel <- spec$panel[[pnl]]$lab[1]
          if (grepl('^expr:', ylabel)) {
            ylabel <- sub('expr:', '', ylabel)
            ylabel <- parse(text=ylabel)
            print (ylabel)
          } 
          plotWAC (DataR[, c('Time', spec$panel[[pnl]]$var)], log=logY,
                   ylab=ylabel,
                   col=spec$panel[[pnl]]$col,
                   lwd=spec$panel[[pnl]]$lw,
                   lty=spec$panel[[pnl]]$lt) 
        } else {
          plotWAC (DataR[, c('Time', spec$panel[[pnl]]$var)], ylim=yl, log=logY,
                   ylab=spec$panel[[pnl]]$lab[1],
                   col=spec$panel[[pnl]]$col,
                   lwd=spec$panel[[pnl]]$lw,
                   lty=spec$panel[[pnl]]$lt) 
        }
        par(cex=1)
      }
    }
  }
  
  output$display <- renderPlot ({  ## display
    reac$newdisplay
    Project <- plotSpec$Project
    if (Trace) {
      print ('display: entered')
      # Sys.sleep(5)
    }
    if (Trace) {
      print (sprintf ('display: newdisplay is %d', reac$newdisplay))
      print (sprintf ('display: global plotSpec$Times are %s %s',
                      formatTime (plotSpec$Times[1]), formatTime (plotSpec$Times[2])))
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newdisplay <- reac$newdisplay + 1 
      isolate (reac$newdata <- reac$newdata + 1)
      if (Trace) {print ('data: exiting display for new data')}
      return()
    }
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]

    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Plot[[input$plot]]$restrict)
    # i <- getIndex (DataR$Time, SE[1])
    if (plotSpec$TypeFlight == 'F') {
      FigFooter <<- sprintf("%s rf%02dF %s %s-%s UTC,", Project, 
                            plotSpec$Flight, strftime(plotSpec$Times[1], format="%Y-%m-%d", tz='UTC'),
                            strftime(plotSpec$Times[1], format="%H:%M:%S", tz='UTC'),
                            strftime(plotSpec$Times[2], format="%H:%M:%S", tz='UTC'))
    } else {
      FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                          plotSpec$Flight, strftime(plotSpec$Times[1], format="%Y-%m-%d", tz='UTC'),
                          strftime(plotSpec$Times[1], format="%H:%M:%S", tz='UTC'),
                          strftime(plotSpec$Times[2], format="%H:%M:%S", tz='UTC'))
    }
    FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    AddFooter <<- function() {
      isolate (
        mtext(paste(FigFooter,'generated by Ranadu plot ', input$plot,
                    FigDatestr),1,outer=T,cex=0.75)
      )
    }
    # closeAlert(session, alertId = 'pausing')
    if (nrow(DataR) > 0) {
      plotMain (input)    ## isolated in function to be able to save via PDF/PNG
      if (input$footer) {AddFooter ()}
    }
    if (Trace) {
      print ('display: finished plot generation')
    }
  }, width=1024, height=640)
  
  plotScat <- function (input) {  ## plotScat
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Scat[[input$plot]]$restrict)
    plt <- isolate (input$plot)
    spec <- plotSpec$Scat[[plt]]
    nrws <- ceiling (spec$panels / spec$columns)
    nmx <- nrws * spec$columns
    layout(matrix(1:nmx, ncol = spec$columns), widths = 1, 
           heights = c(rep(5,spec$panels-1),6))
    op <- par (mar=c(5,4,1,2)+0.1, oma=c(1.1,0,0,0))
    if (spec$restrict) {
      DataX <- DataV
    } else (
      DataX <- DataR
    )
    for (pnl in 1:spec$panels) {
      #       if ((pnl == spec$panels) || (pnl %% nrws == 0)) {
      #         op <- par (mar=c(5,4,1,2)+0.1)
      #       } else {
      #         op <- par (mar=c(2,4,1,2)+0.1)
      #       }
      logV <- ''
      if (spec$panel[[pnl]]$logX) {
        logV <- 'x'
        v <- spec$panel[[pnl]]$varx
        for (vv in v) {
          DataX <- DataX[!is.na (DataX[, vv]), ]
          DataX <- DataX[DataX[, vv] > 0, ]
        }
      }
      if (spec$panel[[pnl]]$logY) {
        logV <- paste(logV, 'y', sep='')
        v <- spec$panel[[pnl]]$vary
        for (vv in v) {
          DataX <- DataX[!is.na (DataX[, vv]), ]
          DataX <- DataX[DataX[, vv] > 0, ]
        }
      }
      yl <- NULL
      if (spec$panel[[pnl]]$fixed) {
        xl <- spec$panel[[pnl]]$xlim
        yl <- spec$panel[[pnl]]$ylim
      }
      
      ## note that xlab is a necessary argument because otherwise plotWAC uses Time
      if (is.null (yl) || is.null (xl)) {
        if (input$ssmooth) {
          smoothScatter (DataX[, c(spec$panel[[pnl]]$varx, spec$panel[[pnl]]$vary[1])], nrpoints=0,
            colramp=colorRampPalette(blues9), pch=spec$panel[[pnl]]$symbol[1], cex=spec$panel[[pnl]]$size[1])
          if (Trace) {print (sprintf ('plotScat: symbol used %d', spec$panel[[pnl]]$symbol))}
        } else {
          plotWAC (DataX[, c(spec$panel[[pnl]]$varx, spec$panel[[pnl]]$vary)],
                   log=logV, col=spec$panel[[pnl]]$col, type='p',
                   xlab=spec$panel[[pnl]]$varx,
                   pch=spec$panel[[pnl]]$symbol, cex=spec$panel[[pnl]]$size,
                   legend.position='top')
        }
      } else {
        if (input$ssmooth) {
          smoothScatter (DataX[, c(spec$panel[[pnl]]$varx, spec$panel[[pnl]]$vary[1])], 
            log=logV, colramp=colorRampPalette(blues9), xlim=xl,
            ylim=yl, xlab=spec$panel[[pnl]]$varx,nrpoints=0,
            pch=spec$panel[[pnl]]$symbol[1], cex=spec$panel[[pnl]]$size[1])
        } else {
          plotWAC (DataX[, c(spec$panel[[pnl]]$varx, spec$panel[[pnl]]$vary)],
                   log=logV, col=spec$panel[[pnl]]$col, type='p', xlim=xl,
                   ylim=yl, xlab=spec$panel[[pnl]]$varx,
                   pch=spec$panel[[pnl]]$symbol, cex=spec$panel[[pnl]]$size,
                   legend.position='top')
        }
      }
      tt <- ''
      for (iy in 1:length (spec$panel[[pnl]]$vary)) {
        fm <- lm(DataX[, c(spec$panel[[pnl]]$vary[iy], spec$panel[[pnl]]$varx)])
        coef <- coefficients (fm)
        if (coef[1] < 0.) {
          t <- sprintf ("%s=%.3f(%s)%.3f rms %.3f r=%.3f",
                        spec$panel[[pnl]]$vary[iy], coef[2], spec$panel[[pnl]]$varx, coef[1],
                        summary(fm)$sigma, sqrt (summary (fm)$r.squared))
        } else {
          t <- sprintf ("%s=%.3f(%s)+%.3f rms %.3f r=%.3f",
                        spec$panel[[pnl]]$vary[iy], coef[2], spec$panel[[pnl]]$varx, coef[1],
                        summary(fm)$sigma, sqrt (summary (fm)$r.squared))
        }
        if (tt == '') {
          tt <- t
        } else {
          tt <- paste (tt, '   ', t, sep='')
        }
      }
      title(tt, cex.main=0.75)
      
    }
  }
  
  plotBin <- function (input) {
    
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Bin[[input$plot]]$restrict)
    plt <- isolate (input$plot)
    spec <- plotSpec$Bin[[plt]]
    nrws <- ceiling (spec$panels / spec$columns)
    nmx <- nrws * spec$columns
    layout(matrix(1:nmx, ncol = spec$columns), widths = 1, 
           heights = c(rep(5,spec$panels-1),6))
    op <- par (mar=c(5,4,1,2)+0.1, oma=c(1.1,0,0,0))
    if (spec$restrict) {
      DataX <- DataV
    } else (
      DataX <- DataR
    )
    for (pnl in 1:spec$panels) {
      #       if ((pnl == spec$panels) || (pnl %% nrws == 0)) {
      #         op <- par (mar=c(5,4,1,2)+0.1)
      #       } else {
      #         op <- par (mar=c(2,4,1,2)+0.1)
      #       }
      logV <- ''
      if (spec$panel[[pnl]]$logX) {
        logV <- 'x'
        v <- spec$panel[[pnl]]$varx
        for (vv in v) {
          DataX <- DataX[!is.na (DataX[, vv]), ]
          DataX <- DataX[DataX[, vv] > 0, ]
        }
      }
      if (spec$panel[[pnl]]$logY) {
        logV <- paste(logV, 'y', sep='')
        v <- spec$panel[[pnl]]$vary
        for (vv in v) {
          DataX <- DataX[!is.na (DataX[, vv]), ]
          DataX <- DataX[DataX[, vv] > 0, ]
        }
      }
      yl <- NULL
      if (spec$panel[[pnl]]$fixed) {
        xl <- spec$panel[[pnl]]$xlim
        yl <- spec$panel[[pnl]]$ylim
      }
      varx <- spec$panel[[pnl]]$varx
      vary <- spec$panel[[pnl]]$vary
      ## eliminate rows where any are NA
      DataX <- DataX[!is.na (DataX[, varx]), ]
      for (kl in 1:length(vary)) {
        DataX <- DataX[!is.na (DataX[, vary[kl]]), ]
      }
      
      ## find the range in x:
      if (is.null(yl)) {
        xl <- c(min (DataX[, varx], na.rm=TRUE),
                max (DataX[, varx], na.rm=TRUE))
      }
      nm <- vary[1]
      yl <- c(min (DataX[, nm], na.rm=TRUE),
              max (DataX[, nm], na.rm=TRUE))
      for (kl in 2:length (vary)) {
        if ((ymn=min (DataX[, vary[kl]], na.rm=TRUE)) < yl[1]) {
          yl[1] <- ymn
        }
        if ((ymx=max (DataX[, vary[kl]], na.rm=TRUE)) > yl[2]) {
          yl[2] <- ymx
        }
      }
      if (Trace) {print (sprintf ('binPlot: xl=%f', xl))}
      nbns <- 20
      at.loc <- vector(length=nbns)
      mean.loc <- vector (length=nbns)
      # plot.window(xlim = yl, ylim = xl, xaxs = "i")
      for (kl in 1:length (vary)) {
        nm <- vary[kl]
        zmin <- xl[1]
        zmax <- zmin + (xl[2]-xl[1]) / nbns
        at.loc[1] <- (zmin+zmax)/2
        dz <- zmax-zmin
        DB <- data.frame ('Z1'=DataX[, nm])
        DB[DataX[, varx] > zmax, 'Z1'] <- NA
        mean.loc[1] <- mean (DB[, 'Z1'], na.rm=TRUE)
        for (j in 2:nbns) {
          zmin <- zmax
          zmax <- zmax + dz
          at.loc[j] <- zmin + dz
          V <- sprintf ('Z%d', j)
          DB[,V] <- DataX[, nm]
          DB[(DataX[, varx] < zmin) | (DataX[, varx] > zmax), V] <- NA
          mean.loc[j] <- mean (DB[, V], na.rm=TRUE)
        }
        adp <- ifelse (kl == 1, FALSE, TRUE)
        xlb <- ifelse (kl == 1, nm, NA)
        colr <- spec$panel[[pnl]]$col[kl]
        symbl <- spec$panel[[pnl]]$symbol[kl]
        sysz <- spec$panel[[pnl]]$size[kl]
        boxplot (DB, horizontal=TRUE, outline=FALSE, 
                 ylab=spec$panel[[pnl]]$varx, xlab=xlb, 
                 ylim=yl, names=NULL,  at=at.loc, border=colr,
                 yaxt='n', add=adp, pars = list(boxwex = 0.7*dz))
        points (mean.loc, at.loc, col=colr, pch=symbl, cex=sysz)
        lines (mean.loc, at.loc, col=colr, lwd=sysz)
      }
      axis(3, labels=NA, tck=0.02)
      axis (4, labels=NA)
      legend ('top', legend=vary, 
              text.col=spec$panel[[pnl]]$col, lwd=spec$panel[[pnl]]$size, 
              cex=0.80, col=spec$panel[[pnl]]$col)
      axis (2)
      DB <<- DB
      tt <- ''
      for (iy in 1:length (spec$panel[[pnl]]$vary)) {
        fm <- lm(DataX[, c(spec$panel[[pnl]]$vary[iy], spec$panel[[pnl]]$varx)])
        coef <- coefficients (fm)
        if (coef[1] < 0.) {
          t <- sprintf ("%s=%.3f(%s)%.3f rms %.3f r=%.3f",
                        spec$panel[[pnl]]$vary[iy], coef[2], spec$panel[[pnl]]$varx, coef[1],
                        summary(fm)$sigma, sqrt (summary (fm)$r.squared))
        } else {
          t <- sprintf ("%s=%.3f(%s)+%.3f rms %.3f r=%.3f",
                        spec$panel[[pnl]]$vary[iy], coef[2], spec$panel[[pnl]]$varx, coef[1],
                        summary(fm)$sigma, sqrt (summary (fm)$r.squared))
        }
        if (tt == '') {
          tt <- t
        } else {
          tt <- paste (tt, '   ', t, sep='')
        }
      }
      title(tt, cex.main=0.75)
      
    }
  }
  
  
  output$scatterplot <- renderPlot ({  ## scatterplot
    reac$newscat
    Project <- plotSpec$Project
    if (Trace) {
      print ('scatterplot: entered')
      # Sys.sleep(5)
    }
    if (Trace) {
      print (sprintf ('scatterplot: newscat is %d', reac$newscat))
      print (sprintf ('scatterplot: global plotSpec$Times are %s %s',
                      formatTime (plotSpec$Times[1]), formatTime (plotSpec$Times[2])))
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newdisplay <- reac$newdisplay + 1 #<- TRUE
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('scatterplot: exiting scatterplot for new data')}
      return()
    }
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Bin[[input$plot]]$restrict)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      if (plotSpec$TypeFlight == 'F') {
        FigFooter <<- sprintf("%s rf%02dF %s %s-%s UTC,", Project, 
                              plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                              strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                              strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                       format="%H:%M:%S", tz='UTC'))
      } else {
        FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                            plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                            strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                            strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                     format="%H:%M:%S", tz='UTC'))
      }
    )
    FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    AddFooter <<- function() {
      isolate (
        mtext(paste(FigFooter,'generated by Ranadu plot ', input$plot,
                    FigDatestr),1,outer=T,cex=0.75)
      )
    }
    plotScat (input)    ## isolated in function to be able to save via PDF/PNG
    
    if (input$footer) {AddFooter ()}
    if (Trace) {
      print ('scatterplot: finished')
    }
  }, width=920, height=640)
  
  output$binplot <- renderPlot ({  ## binplot
    reac$newbin
    Project <- plotSpec$Project
    if (Trace) {
      print ('binplot: entered')
      # Sys.sleep(5)
    }
    if (Trace) {
      print (sprintf ('binplot: newbin is %d', reac$newbin))
      print (sprintf ('binplot: global plotSpec$Times are %s %s',
                      formatTime (plotSpec$Times[1]), formatTime (plotSpec$Times[2])))
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newbin <- reac$newbin + 1 #<- TRUE
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('binplot: exiting for new data')}
      return()
    }
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Scat[[input$plot]]$restrict)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      if (plotSpec$TypeFlight == 'F') {
        FigFooter <<- sprintf("%s rf%02dF %s %s-%s UTC,", Project, 
                              plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                              strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                              strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                       format="%H:%M:%S", tz='UTC'))
      } else {
        FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                            plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                            strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                            strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                     format="%H:%M:%S", tz='UTC'))
      }
    )
    FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    AddFooter <<- function() {
      isolate (
        mtext(paste(FigFooter,'generated by Ranadu plot ', input$plot,
                    FigDatestr),1,outer=T,cex=0.75)
      )
    }
    plotBin (input)    ## isolated in function to be able to save via PDF/PNG
    
    if (input$footer) {AddFooter ()}
    if (Trace) {
      print ('bin plot: finished')
    }
  }, width=920, height=640)
  
  output$fittext <- renderUI ({
    reac$updatefit
    TXT <- 
      sprintf (
        paste('response variable: %s',
              'fit formula: %s',
              'Residual standard deviation: %.3f, dof=%d',
              'R-squared %.3f', 
              'Coefficients:', sep='<br/>'), 
        input$response,
        input$fformula,
        summary(fitm)$sigma, summary(fitm)$df[2],
        summary(fitm)$r.squared)
    HTML(TXT)
    # summary(fitm)$coefficients
  })
  
  output$coeftable <- renderTable ({
    reac$updatefit
    input$times
    summary(fitm)$coefficients
    # anova(fitm)
  }, digits=5)
  
  output$fitplot <- renderPlot ({  ## fitplot
    Project <- plotSpec$Project
    Flight <- plotSpec$Flight
    tf <- plotSpec$TypeFlight
    input$response
    input$fformula
    reac$updatefit
    input$times  ## make sensitive to time changes
    op <- par (mar=c(5,6,1,1)+0.1,oma=c(1.1,0,0,0))
    if (Trace) {
      print ('fitplot: entered')
      # Sys.sleep(5)
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('fitplot: exiting for new data')}
      return()
    }
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, input$limitsFit)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      if (plotSpec$TypeFlight == 'F') {
        FigFooter <<- sprintf("%s rf%02dF %s %s-%s UTC,", Project, 
                              plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                              strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                              strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                       format="%H:%M:%S", tz='UTC'))        
      } else {
        FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                            plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                            strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                            strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                     format="%H:%M:%S", tz='UTC'))
      }
    )
    FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    AddFooter <<- function() {
      isolate (
        mtext(paste(FigFooter,'generated by Ranadu plot ', input$plot,
                    FigDatestr),1,outer=T,cex=0.75)
      )
    }
    plotWAC (data.frame(x=fitm$y, y=fitm$fitted.values), 
             col='blue', type='p', 
             xlab=input$response,
             ylab='fit value',
             pch=20, cex=0.8, legend.position=NA) 
    V <- DataV[, input$response]
    pts <- c(min(V, na.rm=TRUE), max(V, na.rm=TRUE))
    lines (pts, pts, col='darkorange', lty=2, lwd=2)
  })
  
  output$sdplot <- renderPlot ({  ## CDP etc.
    print (sprintf ('entry to sdplot, probes=%s', input$probe))
    Project <- plotSpec$Project
    Flight <- plotSpec$Flight
    tf <- plotSpec$TypeFlight
    times <- input$times    ## make sensitive to time changes, project, etc.
    input$Project
    input$Flight
    DT <- data ()
    Data <- DT[DT$Time > times[1] & DT$Time < times[2], ]
    Data <- transferAttributes (Data, DT)
    print (c('in Data:', sort(names(Data))))
    nm1 <- nm2 <- nm3 <- nm4 <- nm5 <- character(0)
    nms <- names (Data)
    op <- par (mar=c(5,6,1,1)+0.1,oma=c(1.1,0,0,0))
  
    if ('CDP' %in% input$probe) {
      nm1 <- nms [grepl('CCDP_', nms)]
      if (length (nm1) > 0) {
        CellLimitsD <- attr(Data[, nm1[1]], 'CellSizes')
      }
    }
    if ('FSSP' %in% input$probe) {
      nm2 <- nms [grepl('CS100_', nms)]
      if (length (nm2) > 0) {
        CellLimitsF <- attr(Data[, nm2[1]], 'CellSizes')
      }
    }
    if ('UHSAS' %in% input$probe) {
      nm3 <- nms [grepl('CUHSAS_', nms)]
      if (length (nm3) > 0) {
        CellLimitsU <- attr(Data[, nm3[1]], 'CellSizes')
      }
    }
    if ('2DC' %in% input$probe) {
      nm4 <- nms [grepl('^C1DC_', nms)]
      if (length (nm4) > 0) {
        CellLimits2 <- attr(Data[, nm4[1]], 'CellSizes')
      }
    }
    if ('PCASP' %in% input$probe) {
      nm5 <- nms [grepl('CS200_', nms)]
      if (length (nm5) > 0) {
        CellLimitsP <- attr(Data[, nm5[1]], 'CellSizes')
      }
    }

    ## normalize all:
    if (length(nm1 > 0)) {
      for (nm in nm1) {
        CDP <- colMeans(Data[, nm, ], na.rm=TRUE)
        CDPtot <- sum(CDP, na.rm=TRUE)
        CDP <- CDP / diff (CellLimitsD)
        CDP[CDP <= 0] <- 1.e-6
      }
    }
    if (length(nm2 > 0)) {
      for (nm in nm2) {
        FSSP <- colMeans(Data[, nm, ], na.rm=TRUE)
        FSSPtot <- sum(FSSP, na.rm=TRUE)
        FSSP <- FSSP / diff (CellLimitsF)
        FSSP[FSSP <= 0] <- 1.e-6
      }
    }
    if (length(nm3 > 0)) {
      for (nm in nm3) {
        UHSAS <- colMeans(Data[, nm, ], na.rm=TRUE)
        UHSAStot <- sum(UHSAS, na.rm=TRUE)
        UHSAS <- UHSAS / diff (CellLimitsU)
        UHSAS[UHSAS <= 0] <- 1.e-6
      }
    }
    if (length(nm4 > 0)) {
      for (nm in nm4) {
        TWOD <- colMeans(Data[, nm, ], na.rm=TRUE)
        TWODtot <- sum(TWOD, na.rm=TRUE)
        TWOD <- TWOD * 1.e-3 / diff (CellLimits2)
        TWOD[TWOD <= 0] <- 1.e-9
      }
    }
    if (length(nm5 > 0)) {
      for (nm in nm5) {
        PCASP <- colMeans(Data[, nm, ], na.rm=TRUE)
        PCASPtot <- sum (PCASP, na.rm=TRUE)
        PCASP <- PCASP / diff (CellLimitsP)
        PCASP[PCASP <= 0] <- 1.e-6
      }
    }
  ## now have size distributions; construct plots
  dmin <- 1e10
  dmax <- 0
  cmin=1e10
  cmax=0
  if ('CDP' %in% input$probe && (length(nm1) > 0)) {
    dmin <- min (c(dmin, CellLimitsD[1]), na.rm=TRUE)
    dmax <- max (c(dmax, CellLimitsD[length(CellLimitsD)]), na.rm=TRUE)
    cmin <- min (c(cmin, CDP), na.rm=TRUE)
    cmax <- max (c(cmax, CDP), na.rm=TRUE)
  }
  if ('FSSP' %in% input$probe && (length(nm2) > 0)) {
    dmin <- min (c(dmin, CellLimitsF[1]), na.rm=TRUE)
    dmax <- max (c(dmax, CellLimitsF[length(CellLimitsF)]), na.rm=TRUE)
    cmin <- min (c(cmin, FSSP), na.rm=TRUE)
    cmax <- max (c(cmax, FSSP), na.rm=TRUE)
  }
  if ('UHSAS' %in% input$probe && (length(nm3) > 0)) {
    dmin <- min (c(dmin, CellLimitsU[1]), na.rm=TRUE)
    dmax <- max (c(dmax, CellLimitsU[length(CellLimitsU)]), na.rm=TRUE)
    cmin <- min (c(cmin, UHSAS), na.rm=TRUE)
    cmax <- max (c(cmax, UHSAS), na.rm=TRUE)
  }
  if ('2DC' %in% input$probe && (length(nm4) > 0)) {
    dmin <- min (c(dmin, CellLimits2[1]), na.rm=TRUE)
    dmax <- max (c(dmax, CellLimits2[length(CellLimits2)]), na.rm=TRUE)
    cmin <- min (c(cmin, TWOD), na.rm=TRUE)
    cmax <- max (c(cmax, TWOD), na.rm=TRUE)
  }
  if ('PCASP' %in% input$probe && (length(nm5) > 0)) {
    dmin <- min (c(dmin, CellLimitsP[1]), na.rm=TRUE)
    dmax <- max (c(dmax, CellLimitsP[length(CellLimitsP)]), na.rm=TRUE)
    cmin <- min (c(cmin, PCASP), na.rm=TRUE)
    cmax <- max (c(cmax, PCASP), na.rm=TRUE)
  }
  if (length (input$probe) > 0) {
    xp <- c(dmin, dmax)
    yp <- c(cmin, cmax)
    logT <- ''
    if (grepl ('log-x', input$sdtype)) {logT <- paste (logT, 'x', sep='')}
    if (grepl ('log-y', input$sdtype)) {logT <- paste (logT, 'y', sep='')}
    if (grepl ('both log', input$sdtype)) {logT <- 'xy'}
    ## this call just sets appropriate axes:
    yl <- expression (paste("concentration [cm"^"-3", mu, 'm'^'-1', ']'), sep='')
    plot (xp, yp, type='p', log=logT,
      xlab=expression (paste('diameter [',mu,'m]', sep='')),
      ylab=yl, col='white', cex.lab=2, cex.axis=1.4)
  }
  ttl <- sprintf ("Time=%s--%s ", strftime (Data$Time[1], format="%H:%M:%S", tz='UTC'), 
    strftime (Data$Time[nrow(Data)], format="%H:%M:%S", tz='UTC'))
  legend.names <- vector()
  legend.colors <- vector()
  if ('UHSAS' %in% input$probe && (length (nm3) > 0)) {
    points (CellLimitsU, c(1.e-6, UHSAS), type='S', 
      col='darkgreen', lwd=2)
    legend.names <- c(legend.names, 'UHSAS')
    legend.colors <- c(legend.colors, 'darkgreen')
    if (!is.na(UHSAStot)) {
      ttl <- paste0 (ttl, sprintf(" CONCU=%.2f", UHSAStot))
    }
  }
  if (('PCASP' %in% input$probe) && (length (nm5) > 0) && (!is.na(PCASPtot))) {
    points (CellLimitsP, c(1.e-6, PCASP), type='S', 
      col='darkorange', lwd=2)
    legend.names <- c(legend.names, 'PCASP')
    legend.colors <- c(legend.colors, 'darkorange')
    if (!is.na(PCASPtot)) {
      ttl <- paste0 (ttl, sprintf(" CONCP=%.2f", PCASPtot))
    }
  }
  if ('CDP' %in% input$probe && (length (nm1) > 0)) {
    points (CellLimitsD, c(1.e-6, CDP), type='S', 
      col='blue', lwd=2)
    legend.names <- c(legend.names, 'CDP')
    legend.colors <- c(legend.colors, 'blue')
    if (!is.na(CDPtot)) {
      ttl <- paste0 (ttl, sprintf(" CONCD=%.2f", CDPtot))
    }
  }
  if ('FSSP' %in% input$probe && (length (nm2) > 0)) {
    points (CellLimitsF, c(1.e-6, FSSP), type='S', 
      col='violet', lwd=2)
    legend.names <- c(legend.names, 'FSSP')
    legend.colors <- c(legend.colors, 'violet')
    if (!is.na(FSSPtot)) {
      ttl <- paste0 (ttl, sprintf(" CONCF=%.2f", FSSPtot))
    }
  }
  if ('2DC' %in% input$probe && (length (nm4) > 0)) {
    points (CellLimits2, c(1.e-9, TWOD), type='S', 
      col='red', lwd=2)
    legend.names <- c(legend.names, '2DC')
    legend.colors <- c(legend.colors, 'red')
    if (!is.na(TWODtot)) {
      ttl <- paste0 (ttl, sprintf(" CONC1DC=%.4f", TWODtot))
    }
  }
  
  if (length (input$probe) > 0) {
    title (ttl)
    print(c('title', ttl))
    print (legend.names)
    print (legend.colors)
    legend ("topright", legend=legend.names, col=legend.colors,
      lwd=c(2,1), cex=0.75)
  }
}, width=800, height=640)
  
  output$varplot <- renderImage ({  ## varplot
    reac$newvarp
    Project <- plotSpec$Project
    print (sprintf ('spectype is %s', input$spectype))
    # spec <- plotSpec$Var[[input$plot]]
    if (Trace) {
      print ('varplot: entered')
      # Sys.sleep(5)
    }
    if (Trace) {
      print (sprintf ('varplot: newvarp is %d', reac$newvarp))
      print (sprintf ('varplot: global plotSpec$Times are %s %s',
                      formatTime (plotSpec$Times[1]), formatTime (plotSpec$Times[2])))
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      # reac$newvarp <- reac$newvarp + 1 
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('varplot: exiting for new data')}
      return()
    }
   
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    DataR <- transferAttributes (DataR, Data)
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Variance[[input$plot]]$restrict)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      if (plotSpec$TypeFlight == 'F') {
        FigFooter <<- sprintf("%s rf%02dF %s %s-%s UTC,", Project, 
                              plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                              strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                              strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                       format="%H:%M:%S", tz='UTC'))        
      } else {
        FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                            plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                            strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                            strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                     format="%H:%M:%S", tz='UTC'))
      }
    )
    FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    AddFooter <<- function() {
      isolate (
        mtext(paste(FigFooter,'generated by Ranadu plot ', input$plot,
                    FigDatestr),1,outer=T,cex=0.75)
      )
    }
    fnew <- "./R-toXanadu.nc"
    unlink(fnew)
    DataR <<- DataR
    Z <- makeNetCDF (DataR, fnew)
    if (Trace) {print ('varplot:return from makeNetCDF:');print(Z)}
    ## don't know why this is needed, but makeNetCDF modifies DataR;
    ## need to fix that routine
    # DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    Data2 <<- Data
    wlow <- 0.0001; whigh <- 10.
    # spec$var <- 'WIC
    ts <- formatTime(plotSpec$Times[1])
    te <- formatTime(plotSpec$Times[2])
    ts <- as.integer(gsub (':', '', ts))
    te <- as.integer (gsub (':', '', te))
    if (input$spectype == 'MEM') {
      unlink ("MEMPlot.png")
    }
    if (input$spectype == 'fft') {
      unlink ("FFTPlot.png")
    }
    if (input$spectype == 'acv') {
      lagMax <- min(3600, nrow (data ()))
      gname <- 'SpecialGraphics/ACVPlot.png'
      unlink ('SpecialGraphics/ACVPlot.png')
      v <- plotSpec$Variance[[1]]$Definition$var 
      cv <- plotSpec$Variance[[1]]$Definition$cvar 
      vdt <- DataR[, v]
      cvdt <- DataR[, cv]
      ## remove the mean and linear trend
      if (input$acvdetrend) {
        dt <- as.numeric(difftime(DataR$Time, DataR$Time[1]))
        ff1 <- lm (vdt ~ dt)
        ff2 <- lm (cvdt ~ dt)
        vdt <- vdt-ff1$coefficients[2]*dt
        cvdt <- cvdt-ff2$coefficients[2]*dt
        vdt <- vdt - mean (vdt, na.rm=TRUE)
        cvdt <- cvdt - mean (cvdt, na.rm=TRUE)
      }
      variance <- var (vdt, na.rm=TRUE)
      if (input$acvtype == 'data') {
        png (gname)
        layout(matrix(1:2, ncol = 1), widths = c(5,5), heights = c(5,6))
        op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
        plotWAC (DataR$Time, vdt, ylab=v)
        if (input$acvdetrend) {
          title (sprintf ('<%s x %s> = %.3f', v, cv, mean(vdt*cvdt, na.rm=TRUE)), cex=0.75)
        }
        op <- par (mar=c(5,4,1,2)+0.1)
        plotWAC (DataR$Time, cvdt, ylab=cv)
        if (input$acvdetrend) {
          title('mean and trend removed')
        }
        dev.off ()
      } else if (input$acvtype == 'crosscorrelation') {
        ccfp <- ccf (vdt, cvdt, lag.max=lagMax, na.action=na.pass, 
                     plot=FALSE)
        gname <- 'SpecialGraphics/ACVPlot.png'
        png (gname)
        plot (ccfp, type='l', lwd=1.5, col='blue', xlab='lag [s]', 
              ylab=sprintf ('ccf for %s x %s', v, cv),
              main=sprintf ('zero-lag correlation is %.2f', 
                            cor (vdt, cvdt, use='pairwise.complete')))
        dev.off ()
      } else {  ## this calculation is needed for autocor or spectra
        variance1 <- var (vdt, na.rm=TRUE)
        variance2 <- var (cvdt, na.rm=TRUE)
        acfp <- acf (vdt, lag.max=lagMax, na.action=na.pass, 
                     plot=FALSE)        
        acfpc <- acf (cvdt, lag.max=lagMax, na.action=na.pass, 
                      plot=FALSE)
        ## smoothed version
        acfps <- acfp$acf
        acfpcs <- acfpc$acf
        gname <- 'SpecialGraphics/ACVPlot.png'
        png (gname)
        tau <- input$acvtau
        lacf <- min (length (acfps), tau)
        acfps[1:lacf] <- acfps[1:lacf]*(1-((0:(lacf-1))/tau))
        acfpcs[1:lacf] <- acfpcs[1:lacf]*(1-((0:(lacf-1))/tau))
        if (tau < length (acfp$acf)) {
          acfps[(tau+1):length (acfps)] <- 0
          acfpcs[(tau+1):length (acfps)] <- 0
        }
        if (input$acvtype == 'autocorrelation') {
          layout(matrix(1:2, ncol = 1), widths = c(5,5), heights = c(5,6))
          op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
          plot (acfp, type='l', lwd=1.5, col='blue', xlab='lag [s]', 
                ylab=sprintf('%s acf', plotSpec$Variance[[1]]$Definition$var))
          points (acfps, type='l', col='darkgreen', lty=2)
          legend ('topright', legend=c('ACF', 'smoothed'), col=c('blue', 'darkgreen'),
                  lwd=c(1.5,1), lty=c(1,2))
          op <- par (mar=c(5,4,1,2)+0.1)
          plot (acfpc, type='l', lwd=1.5, col='blue', xlab='lag [s]', 
                ylab=sprintf ('%s acf', plotSpec$Variance[[1]]$Definition$cvar))
          points (acfpcs, type='l', col='darkgreen', lty=2)
          legend ('topright', legend=c('ACF', 'smoothed'), col=c('blue', 'darkgreen'),
                  lwd=c(1.5,1), lty=c(1,2))
          dev.off ()
        } else {
          mx <- length(acfps)
          ps <- vector(length=mx/2)
          ps2 <- ps
          pss <- ps
          pss2 <- ps
          freq <- (1:(mx/2)) / mx
          f2pi = freq * 2 * pi
          for (i in 1:(mx/2)) {
            ps[i] <-  (1 + 2 * sum(acfp$acf[2:mx] * cos (f2pi[i] * (1:(mx-1))), na.rm=TRUE)) * variance1
            pss[i] <- (1 + 2 * sum(acfps[2:lacf] * cos (f2pi[i] * (1:(lacf-1))), na.rm=TRUE)) * variance1
            ps2[i] <-  (1 + 2 * sum(acfpc$acf[2:mx] * cos (f2pi[i] * (1:(mx-1))), na.rm=TRUE)) * variance2
            pss2[i] <- (1 + 2 * sum(acfpcs[2:lacf] * cos (f2pi[i] * (1:(lacf-1))), na.rm=TRUE)) * variance2
          }
          if (grepl('fp\\(f\\)', input$acvtype)) {
            ps <-  ps * freq
            ps2 <- ps2 * freq
            pss <- pss * freq
            pss2 <- pss2 * freq
          }
          df <- data.frame(V1=log10 (pss), V2=log10(freq))
          df2 <- data.frame(V1=log10 (pss2), V2=log10(freq))
          pf <- binStats(df, bins=input$acvavg)
          pf2 <- binStats(df2, bins=input$acvavg)
          df$V1 <- 10^df$V1
          df$V2 <- 10^df$V2
          df$V3 <- 10^df2$V1
          df$psa <- 10^pf$ybar
          df$fa <- 10^pf$xc
          df$psa2 <- 10^pf2$ybar
          df$fa2 <- 10^pf2$xc
          # df$fps <- freq*ps
          df$freq <- freq
          p <- ggplot (df, aes(x=V2, y=V1), log='xy')
          if (grepl ('var2', input$acvtype)) {
            p <- p + geom_path (aes(y=V3), colour='darkgreen', lwd=1.5) + geom_path (aes(x=fa2, y=psa2), colour='darkorange', lwd=0.8)
          } else {
            p <- p + geom_path (colour='blue', lwd=1.5) + geom_path (aes(x=fa, y=psa), colour='red', lwd=0.8)
          }
          # p <- p + geom_path (aes(x=freq, y=fps), colour='darkgreen')
          xln <- c(10^-2.5, 10^-1)
          if (grepl ('fp\\(f\\)', input$acvtype)) {
            yln <- c(1, 10^-1)
          } else {
            yln <- c(1, 10^-2.5)
          }
          dr <- data.frame (x=xln, y=yln)
          p <- p + geom_line (data=dr, aes(x=x, y=y), colour='black', lty=3, lwd=0.7)
          p <- p + xlab('frequency [Hz]')
          if (grepl ('fp\\(f\\)', input$acvtype)) {
            if (grepl ('var2', input$acvtype)) {
              p <- p + ylab (sprintf ('%s:  fp(f)', cv))
            } else {
              p <- p + ylab(sprintf ('%s:  fp(f)', v))
            }
          } else {
            if (grepl ('var2', input$acvtype)) {
              p <- p + ylab (sprintf ('%s:  p(f)', cv))
            } else {
              p <- p + ylab(sprintf ('%s:  p(f)', v))
            }
          }
          p <- p + scale_y_log10() + scale_x_log10() 
          p <- p + theme_WAC()
          print(p)
          dev.off ()
        }
      }
    } else {  ## end of acv section; others need Xanadu routine 'otto'
      isolate (plt <- input$plot)
      v <- plotSpec$Variance[[plt]]$Definition$var
      cv <- plotSpec$Variance[[plt]]$Definition$cvar
      setXanadu (fnew, ts, te, v, cv, wlow, whigh, input$spectype, isolate(input$MEMadd), 
        isolate(input$MEMcolor))
      XanaduOut <<- system2 ("Xanadu", args="otto", stdout=TRUE)
      if (length (XanaduOut) < 1) {return()}
      # save (XanaduOut, file='/srv/shiny-server/Ranadu/XanaduOut.Rdata')
      # print ('return from system2 is:')
      # print (XanaduOut)
      if (input$spectype == 'MEM') {
        while (!file.exists ("MEMPlot.png")) {Sys.sleep (1)}
        gname <- "SpecialGraphics/PSDMEM.png"
        file.rename ("MEMPlot.png", gname)
      }
      if (input$spectype == 'fft') {
        if (input$ffttype == 'fp(f)' || input$ffttype == 'p(f)') {
          while (!file.exists ("FFTPlot.png")) {Sys.sleep (1)}
          gname <- "SpecialGraphics/PSDFFT.png"
          file.rename ("FFTPlot.png", gname)
        } else {  ## cospec / quad / coherence / phase
          gname <- 'SpecialGraphics/FFTPlot.png'
          png (gname, width=600, height=600)
          writeOutput <- FALSE
          cospecOutput <- vector ('character')
          fps <- vector ('numeric')
          fpsc <- vector ('numeric')
          p <- vector ('numeric')
          q <- vector ('numeric')
          coh <- vector ('numeric')
          for (line in XanaduOut) {
            if (grepl ('end of cospec', line)) {writeOutput <- FALSE}
            line <- sub('^ ', '', line)
            if (writeOutput) {
              cospecOutput <- c(cospecOutput, line)
              a <- as.numeric (as.vector (strsplit(line, split=' ')[[1]]))
              # i <- as.integer(a[1])
              fps <- c(fps, a[2])
              fpsc <- c(fpsc, a[3])
              p <- c(p, a[4])
              q <- c(q, a[5])
              coh <- c(coh, a[6])
              
            }
            if (grepl ('start of cospec', line)) {writeOutput <- TRUE}
          }  ## end of lines in XanaduOut
          segl <- plotSpec$Variance[[1]]$Definition$fftpts
          freq <- (1:(segl/2))/segl
          layout(matrix(1:2, ncol = 1), widths = c(5,5), heights = c(5,6))
          nbins <- plotSpec$Variance[[1]]$Definition$fftavg
          if (grepl('both', input$ffttype)) {
            op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
            df <- data.frame(fxf=segl*freq*fps, V2=log10(freq))
            df2 <- data.frame(fxf=segl*freq*fpsc, V2=log10(freq))
            pf <- binStats(df, bins=nbins)
            pf2 <- binStats(df2, bins=nbins)
            plotWAC (freq, segl*freq*fps,  type='l', xlab='frequency', col='blue', 
                     log='xy', ylab=sprintf ('%s:  fp(f)', v))
            pf <- pf[!is.na(pf$ybar),]
            lines (10^pf$xc, pf$ybar, col='red', lwd=2)
            op <- par (mar=c(5,4,1,2)+0.1)
            plotWAC (freq, segl*freq*fpsc, type='l', xlab='frequency', col='blue', 
                     log='xy', ylab=sprintf ('%s:  fp(f)', cv))
            pf2 <- pf2[!is.na(pf2$ybar),]
            lines (10^pf2$xc, pf2$ybar, col='red', lwd=2)
          } else if (grepl ('data', input$ffttype)) {
            op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
            plotWAC (DataR[, c('Time', v)])
            op <- par (mar=c(5,4,1,2)+0.1)
            plotWAC (DataR[, c('Time', cv)])
          } else {
            df <- data.frame(fxpf=segl*freq*p, V2=log10(freq))
            df2 <- data.frame(fxqf=segl*freq*q, V2=log10(freq))
            pf <- binStats(df, bins=nbins)
            pf2 <- binStats(df2, bins=nbins)
            df$fxqf <- df2$fxqf
            df$freq <- freq
          }
          if (grepl('cosp', input$ffttype)) {
            op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
            plotWAC (df[, c('freq', 'fxpf')], type='l', xlab='frequency', col='blue', 
                     log='x', ylab=sprintf ('%s x %s:  f x cospectrum', v, cv))
            pf <- pf[!is.na(pf$ybar),]
            lines (10^pf$xc, pf$ybar, col='red', lwd=2)
            abline(h=0, lty=3, lwd=0.7)
            legend('topright', legend=c('f C[f]', 'bin-averages'), col=c('blue', 'red'), lwd=c(2,2))
            op <- par (mar=c(5,4,1,2)+0.1)
            plotWAC (df[, c('freq', 'fxqf')], type='l', xlab='frequency', col='blue', 
                     log='x', ylab=sprintf ('%s x %s:  f x quadrature', v, cv))
            pf2 <- pf2[!is.na(pf2$ybar),]
            lines (10^pf2$xc, pf2$ybar, col='red', lwd=2)
            abline(h=0, lty=3, lwd=0.7)
            legend('topright', legend=c('f Q[f]', 'bin-averages'), col=c('blue', 'red'), lwd=c(2,2))
          } 
          if (grepl('coher', input$ffttype)) {
            op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
            plotWAC(freq, coh, log='x', xlab='frequency', col='blue', 
                    ylab='coherence', ylim=c(0,1))
            df <- data.frame(coh=coh, V2=log10(freq))
            pf3 <- binStats(df, bins=nbins)
            pf3 <- pf3[!is.na (pf3$ybar), ]
            lines (10^pf3$xc, pf3$ybar, col='red', lwd=2)
            legend('topleft', legend=c('coherence', 'bin-averages'), col=c('blue', 'red'), lwd=c(2,2))
            phase <- atan2 (q, p) * 180/pi
            phase[phase > 180] <- phase[phase > 180] - 360
            phase[phase < -180] <- phase[phase < -180] + 360
            ## proper averaging of the phase must account for wrap-around
            ## better: average q and p, then find phase; already have these from above
            op <- par (mar=c(5,4,1,2)+0.1)
            plotWAC (freq, phase, xlab='frequency', type='l', col='blue', 
                     log='x', ylab='phase [deg.]', ylim=c(-180,180))
            avgphase <- atan2 (pf2$ybar, pf$ybar) * 180/pi
            lines (10^pf2$xc, avgphase, col='red', lwd=2)
            abline(h=0, lty=3, lwd=0.7)
            legend('topleft', legend=c('phase', 'bin-averages'), col=c('blue', 'red'), lwd=c(2,2))
          }
          dev.off ()
        }
      }
    }
    return(list(
      src = gname,
      contentType = "image/png",
      alt = "PSD"
    ))
  }, deleteFile = FALSE)
  
  
  
  output$savePDF <- downloadHandler(
    filename = function() {
      paste('Ranadu-', format(Sys.time(), '%y-%m-%d-%H-%M-%S'), '.pdf', sep='')
    },
    content = function(file) {
      pdf (file, width=10, height=6)
      if (grepl ('plot vs', input$whichTab)) {
        plotMain (input)
      }
      if (grepl ('histogram', input$whichTab)) {
        plotHist (input)
      }
      if (grepl ('scatter', input$whichTab)) {
        plotScat (input)
      }
      if (grepl ('bin-average', input$whichTab)) {
          plotBin (input)
      }
      dev.off ()
    },
    contentType='image/pdf'
  )
  
  output$savePNG <- downloadHandler(
    filename <- function() {
      paste('Ranadu-', format(Sys.time(), '%y-%m-%d-%H-%M-%S'), '.png', sep='')
    },
    content <- function(file) {
      png (file, height=640, width=1024)
      if (grepl ('plot vs', input$whichTab)) {
        plotMain (input)
      }
      if (grepl ('histogram', input$whichTab)) {
        plotHist (input)
      }
      if (grepl ('scatter', input$whichTab)) {
        plotScat (input)
      }
      if (grepl ('bin-average', input$whichTab)) {
        plotBin (input)
      }
      if (grepl ('variance', input$whichTab)) {
        img <- png::readPNG("SpecialGraphics/PSDMEM.png")
        grid.raster(img)
      }
      dev.off ()
    }
  )
  
  output$quickPlot <- renderPlot ({
    reac$quick
    if (Trace) {print ('quickPlot: entered')}
    Data <- data ()
    if (!(quickPlotVar %in% names (Data))) {
      isolate(reac$newdata <- reac$newdata + 1)
      isolate (reac$quick <- reac$quick + 1)
      return ()
    }
    Data <- Data[Data$Time >= plotSpec$Times[1] & Data$Time < plotSpec$Times[2], ]
    plotWAC(Data[, c('Time', quickPlotVar)])
  })
  
  output$track <- renderPlot ({  ## track
    reac$newtrack
    input$times
    Project <- plotSpec$Project
    if (Trace) {
      print (sprintf ('track: entry, reac$newtrack is %d', reac$newtrack))
    }
    Data <- data ()
    if (length (Data) < 2) {
      warning (sprintf ('variable error in (%s)', fname))
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
      return ()
    }
    
    if (Trace) {
      print (sprintf ('track: input$times %s %s', formatTime (input$times[1]),
                      formatTime (input$times[2])))
      print (sprintf ('track: global plotSpec$Times are %s %s',
                      formatTime (plotSpec$Times[1]), formatTime (plotSpec$Times[2])))
    }
    namesV <- names(Data)
    namesV <- namesV[namesV != "Time"]
    #       for (nm in namesV) {
    #         
    #       }
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, input$limits2)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    #       
    #       for (n in namesV) {
    #         Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
    #       }
    #       # Data <- Data[(Data$Time >= input$times[1]) & (Data$Time < input$times[2]), ]
    #       Data <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    #       if (nrow (Data) <= 1) {
    #         plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
    #         text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
    #         reac$newdisplay <- reac$newdisplay + 1
    #         reac#newdata <- reac$newdata + 1
    #         return()
    #       }
    #       ## see global.R functions:
    #       DataV <- limitData (Data, input, input$limits2)
    #       ndv <- names (DataV)
    #       
    #       SE <- getStartEnd (Data$Time)
    i <- getIndex (DataR$Time, SE[1])
    if (plotSpec$TypeFlight == 'F') {
      FigFooter=sprintf("%s rf%02dF %s %s-%s UTC,", Project, 
                        plotSpec$Flight, strftime(DataR$Time[i], format="%Y-%m-%d", tz='UTC'),
                        strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                        strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                 format="%H:%M:%S", tz='UTC'))
    } else {
      FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                      plotSpec$Flight, strftime(DataR$Time[i], format="%Y-%m-%d", tz='UTC'),
                      strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                      strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                               format="%H:%M:%S", tz='UTC'))
    }
    FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    AddFooter <<- function() {
      CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
      mtext(paste(FigFooter,'generated by Ranadu plotTrack', #CallingFunction,
                  FigDatestr),1,outer=T,cex=0.75)
    }
    
    par(oma=c(1.1,0,0,0))
    xc <- input$track.xc
    yc <- input$track.yc
    sz <- input$track.sz
    if (is.na(xc)) xc <- NULL
    if (is.na(yc)) yc <- NULL
    if (is.na(sz)) sz <- NULL
    
    if (input$limits2) {
      DataT <- DataV
    } else {
      DataT <- DataR
    }
    if (plotSpec$SRC == 'FAAM') {
      DataT$LONC <- DataT$CLNG
      DataT$LATC <- DataT$CLAT
    }
    if (input$drift) {
      xc <- NA
      #         DataT$TASX <- SmoothInterp (DataT$TASX)
      #         DataT$THDG <- SmoothInterp (DataT$THDG)
      #         DataT$SSLIP <- SmoothInterp (DataT$SSLIP)
    }
    plotTrack (DataT, 
               xc=xc, yc=yc, sz=sz,
               .Spacing=input$track.spacing, .WindFlags=input$track.WF)
    if (input$footer2) {AddFooter ()}
    
    # }
    #       si <- input$plot
    #       updateSelectInput (session, 'Rplot', selected=st[si])
    
    
  }, width=640, height=640)
  
  
  output$theight <- renderPlot ({  ## theight
    # input$typeFlight
    if (is.null(input$times[1])) {
      if (Trace) {print ('theight: input time is NULL, returning')}
      return ()
    }
    if (Trace) {
      print (sprintf ('theight: entry, reac$newdisplay is %d', reac$newdisplay))
    }
    # input$PlotVar
    Project <- input$Project
    if (reac$newdisplay) {
      # VRPlot <- VRP ()
      if (Trace) {print (sprintf ('entered theight, newdisplay is %d', reac$newdisplay))}
      # VRPlot <<- VRPlot
      Data <- data()
      if (length (Data) < 2) {
        warning (sprintf ('variable error in (%s)', fname))
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
        return ()
      }
      
      if (Trace) {
        print (sprintf ('thight: input$times %s %s', formatTime (input$times[1]),
                        formatTime (input$times[2])))
        print (sprintf ('thight: global plotSpec$Times are %s %s',
                        formatTime (plotSpec$Times[1]), formatTime (plotSpec$Times[2])))
      }
      namesV <- names(Data)
      namesV <- namesV[namesV != "Time"]
      for (n in namesV) {
        Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
      }
      # Data <- Data[(Data$Time >= input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
      if (nrow (Data) <= 0) {
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
        reac$newdisplay <- reac$newdisplay + 1
        reac$newdata <- reac$newdata + 1
        return()
      }
      ## see global.R functions:
      DataV <- limitData (Data, input, input$limits2)
      ndv <- names (DataV)
      
      SE <- getStartEnd (Data$Time)
      i <- getIndex (Data$Time, SE[1])
      if (input$typeFlight == 'F') {
        FigFooter=sprintf("%s rf%02dF %s %s-%s UTC,", Project,
                          input$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                          strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                          strftime(Data$Time[getIndex(Data$Time,SE[2])],
                                   format="%H:%M:%S", tz='UTC'))        
      } else {
        FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, input$typeFlight,
                        input$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                        strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                        strftime(Data$Time[getIndex(Data$Time,SE[2])],
                                 format="%H:%M:%S", tz='UTC'))
      }
      FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
      AddFooter <<- function() {
        CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
        mtext(paste(FigFooter,'generated by ', CallingFunction,
                    FigDatestr),1,outer=T,cex=0.75)
      }
      
      par(oma=c(1.1,0,0,0))
      if (plotSpec$SRC == 'NCAR') {
        Data$Z <- Data$GGALT
      } else {
        Data$Z <- Data$GALT
      }
      if (input$limits2) {
        plotWAC (DataV[, c('Time', 'Z')])
      } else {
        plotWAC (Data[, c('Time', 'Z')])
      }
      AddFooter ()
      
      # }
      #       si <- input$plot
      #       updateSelectInput (session, 'Rplot', selected=st[si])
      
    }
  }, width=640, height=640)  
  
  output$skewT <- renderPlot ({  ## skewT
    reac$newskewT
    # input$typeFlight
    if (is.null(input$times[1])) {
      if (Trace) {print ('skewT: input time is NULL, returning')}
      return ()
    }
    if (Trace) {
      print (sprintf ('skewT: entry, reac$newskewT is %d', reac$newskewT))
    }
    # input$PlotVar
    Project <- input$Project
    if (reac$newskewT) {
      # VRPlot <- VRP ()
      if (Trace) {print ('skewT: reac$newskewT != 0')}
      # VRPlot <<- VRPlot
      Data <- data()
      if (length (Data) < 2) {
        warning (sprintf ('variable error in (%s)', fname))
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
        return ()
      }
      
      if (Trace) {
        isolate (print (sprintf ('skewT: input$times %s %s', formatTime (input$times[1]),
                        formatTime (input$times[2]))))
        print (sprintf ('skewT: global plotSpec$Times are %s %s',
                        formatTime (plotSpec$Times[1]), formatTime (plotSpec$Times[2])))
      }
      updateSliderInput (session, 'times', value=plotSpec$Times)
      if ((input$times[1] != plotSpec$Times[1]) || (input$times[2] != plotSpec$Times[2])) {
        if (Trace) {print('skewT: skipping because times do not match')}
        # reac$newskewT <- reac$newskewT + 1
        return()
      }
      namesV <- names(Data)
      namesV <- namesV[namesV != "Time"]
      for (n in namesV) {
        Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
      }
      # Data <- Data[(Data$Time >= input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
      if (nrow (Data) <= 0) {
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
        reac$newskewT <- reac$newskewT + 1
        isolate(reac$newdata <- reac$newdata + 1)
        return()
      }
      ## see global.R functions:
      DataV <- limitData (Data, input, input$limits6)
      ndv <- names (DataV)
      
      SE <- getStartEnd (Data$Time)
      i <- getIndex (Data$Time, SE[1])
      if (plotSpec$TypeFlight == 'F') {
        FigFooter=sprintf("%s rf%02dF %s %s-%s UTC,", Project,
                          plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                          strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                          strftime(Data$Time[getIndex(Data$Time,SE[2])],
                                   format="%H:%M:%S", tz='UTC'))        
      } else {
        FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                        plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                        strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                        strftime(Data$Time[getIndex(Data$Time,SE[2])],
                                 format="%H:%M:%S", tz='UTC'))
      }
      FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
      AddFooter <<- function() {
        CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
        mtext(paste(FigFooter,'generated by ', CallingFunction,
                    FigDatestr),1,outer=T,cex=0.75)
      }
      op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
      print (ndv)
      if (plotSpec$SRC == 'NCAR') {VSKT <- c('PSXC', 'ATX', 'DPXC')}
      if (plotSpec$SRC == 'UWYO') {VSKT <- c('ps_hads_a', 'trose', 'tdp')}
      if (plotSpec$SRC == 'FAAM') {VSKT <- c('SPR', 'TTDI', 'DEWP')}
      print (VSKT)
      print (names(Data))
      if (input$limits6) {
        DF <- DataV[, VSKT]
      } else {
        DF <- Data[, VSKT]
      }
      colnames(DF) <- c("Pressure", "Temperature", "DewPoint")
      if (input$SRC == 'FAAM') {
        DF$Temperature <- DF$Temperature - 273.15
        DF$DewPoint <- DF$DewPoint - 273.15
      }
      suppressWarnings (gg <- SkewTSounding (DF, AverageInterval=5, BackgroundSpecs="skewTDiagram.Rdata")
                        + ggtitle(sprintf("%s Flight %s  %s -- %s", plotSpec$Project, plotSpec$Flight, 
                                          formatTime (plotSpec$Times[1]), formatTime (plotSpec$Times[2]))))
      if (input$cape) {
        NSND <- CAPE (Data)
        if (length (names (NSND)) > 3) {
          gg <- SkewTSounding (NSND)
          S2 <- SkewTSounding (data.frame (Pressure=NSND$Pressure, Temperature=NSND$TP, DewPoint=-120), ADD=TRUE)
          S3 <- SkewTSounding (data.frame (Pressure=NSND$Pressure, Temperature=NSND$TQ, DewPoint=-120), ADD=TRUE)
          gg <- gg + geom_path (data=S2, aes (x=AT, y=P), colour='red', lwd=1.0)
          gg <- gg + geom_path (data=S3, aes (x=AT, y=P), colour='green', lwd=1.0)
          plcl <- attr (NSND, 'LCLp'); tlcl <- attr (NSND, 'LCLt')
          maxLWC <- attr (NSND, 'MaxLWC'); pmaxLWC <- attr (NSND, 'pMaxLWC')
          SP <- SkewTSounding (data.frame(Pressure=plcl, Temperature=tlcl, DewPoint=-120), ADD=TRUE)
          gg <- gg + geom_point (data=SP, aes(x=AT, y=P), pch=19, colour='darkorange', size=4)
          labelText <<- paste(sprintf('orange dot: LCL %.1f hPa %.2f degC', plcl, tlcl), 
                              'red line: pseudo-adiabatic ascent', 
                              'bright green line: wet-adiabatic ascent',
                              sprintf ('max LWC: %.2f g/m3 at %.1f hPa', maxLWC, pmaxLWC),
                              sprintf ('cape=%.0f J/kg (adiabatic cape=%.0f)', 
                                       attr(NSND, 'CAPE'), attr (NSND, 'CAPEW')),
                              sprintf ('conv. inh. %.0f J/kg, LFC=%.0f hPa', 
                                       attr(NSND, 'CIN'), attr(NSND, 'LFC')), sep='\n')
          if (Trace) {
            print (sprintf ('skewT: labelText is %s', labelText))
          }
          gg <- gg + geom_label (aes(x=0, y=2.85, label=labelText), size=4.5, fill='ivory', hjust='left')
        } else {
          gg <- gg + geom_label (aes (x=0, y=2.85, label='no region of positive buoyancy'), size=5,
                                 fill='ivory', hjust='left')
        }
      }
      suppressWarnings (print (gg))
      if (input$hodograph) {
        
        circle.draw <- function (radius) {
          x <- vector (length=361); y <- vector (leng=361)
          for (i in 1:361) {
            x[i] <- radius * sin (i*pi/180) 
            y[i] <- radius * cos (i*pi/180)
          }
          return (data.frame (x=x, y=y))
        }
        
        if (input$limits6) {
          DH <- DataV[, c('WSC', 'WDC', 'PSXC')]
        } else {
          DH <- Data[, c('WSC', 'WDC', 'PSXC')]
        }
        WS <- binStats (DH[, c('WSC', 'PSXC')], xlow=150, xhigh=1050,bins=9)
        WD <- binStats (DH[, c('WDC', 'PSXC')], xlow=150, xhigh=1050, bins=9)
        WE <- -WS$ybar * sin (WD$ybar *pi/180) 
        WN <- -WS$ybar * cos (WD$ybar *pi/180) 
        DF2 <- data.frame ('WE'=WE, 'WN'=WN, 'P'=WS$xc)
        q <- ggplot (DF, aes (x=WE, y=WN))
        q <- q + geom_path (data=DF2, aes (x=WE, y=WN), col='blue', lwd=2) 
        q <- q + geom_point (data=DF2, aes (x=WE, y=WN), col='blue', size=4) 
        q <- q + geom_path (data=circle.draw(20), aes (x=x, y=y))
        q <- q + geom_path (data=circle.draw(40), aes (x=x, y=y))
        q <- q + geom_path (data=circle.draw(60), aes (x=x, y=y))
        q <- q + geom_text (aes (x=0, y=43, label='40 m/s'), size=5)
        for (ang in c(0, 30, 60, 90, 120, 150)) {
          xl <- c(60*sin(ang*pi/180), -60*sin(ang*pi/180))
          yl <- c(60*cos(ang*pi/180), -60*cos(ang*pi/180))
          df <- data.frame (xl=xl, yl=yl)
          q <- q + geom_line (data=df, aes (x=xl, y=yl))
        }
        q <- q + geom_text (data=DF2, aes (x=WE, y=WN, label=P), size=3, nudge_y=5)
        q <- q + xlab('') + ylab('') + theme_base()
        q <- q + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),
                       axis.text.y = element_blank())
        vp <- viewport(width = 0.4, height = 0.4, x = 0.75, y = 0.75)
        print (q, vp=vp)
      }
      
      if (input$footer6) {
        plot.new()
        vpbase <- viewport(width = 1, height = 0.95, x = 0.5, y = 0.5)
        print (gg, vp=vpbase)
        if (input$hodograph) {
          print (q, vp=vp)
        }
        popViewport (0)
        AddFooter ()
      }
    }
  }, width=780, height=640) 
  
  output$paluch <- renderPlot ({
    Data <-  limitData (data (), input, input$limits9)
    if (!plotSpec$paluchLWC %in% names(Data)) {
      showModal(modalDialog(title='Needed Variable Not Found', 
        sprintf ('The Paluch diagram needs a measurement of LWC, but the specified variable (%s) is not present',
          plotSpec$paluchLWC), easyClose=TRUE))
      if (input$paluchBetts == 'Paluch') {return()}
    }
    input$paluchStart
    input$paluchEnd
    input$paluchCStart
    input$paluchCEnd
    if (Trace) {print (sprintf ('Paluch: paluchStart/End %s %s', plotSpec$PaluchTimes[1], plotSpec$PaluchTimes[2]))}
    getMixingData <- function (Data, inp) {
      EWX <- Data$EWX
      EWS <- MurphyKoop (Data$ATX)
      t <- EWX > EWS
      ## Assume sounding measurements with supersaturation are erronous;
      ## replace with equilibrium humidity
      t[is.na (t)] <- FALSE
      # t[is.na (Data$THETAQ)] <- FALSE
      EWX[t] <- EWS[t]
      Data$THETAQ[t] <- WetEquivalentPotentialTemperature(Data$PSXC[t], Data$ATX[t], EWX[t], Data[t, plotSpec$paluchLWC])
      R <- 0.622 * EWX / (Data$PSXC - EWX)
      LWC <- Data[, plotSpec$paluchLWC]
      EbyP <- with (Data, EWX / PSXC)
      Ra <- SpecificHeats (EbyP)[, 3]
      Tk <- Data$ATX + 273.15
      Rtot <-  R + Ra * Tk * LWC * 1.e-3 / (100 * Data$PSXC)
      Qtot <- Rtot / (1 + Rtot)
      return (c(R, Rtot, Qtot, Tk, EWX, Data$THETAQ))
    }
    MD <- getMixingData (Data, input)
    dim(MD) <- c(length(MD)/6, 6)
    R <- MD[,1]; Rtot <- MD[,2]; Qtot <- MD[,3]; Tk <- MD[,4]; EWX <- MD[,5]
    Data$Rtot <- Rtot
    Data$Qtot <- Qtot
    Data$THETAQ <- MD[,6]
    Data$EWX <- EWX
    Data$Tk <- Tk
    Data$R <- R 
    ## restrict data to period of specified environmental sounding
    DataS <- Data[(Data$Time >= plotSpec$PaluchTimes[1]) & (Data$Time < plotSpec$PaluchTimes[2]), ]
    if (Trace) {
      print (sprintf ('Paluch: times are %s %s; names in DataS are:', plotSpec$PaluchTimes[1], plotSpec$PaluchTimes[2]))
      print (sort(names(DataS)))
      DataS <<- DataS  ## save for inspection
    }
    ## get the saturation point, if plot type is Betts:
    if (grepl ('Betts', input$paluchBetts)) {
      load (file=paste(path.package ("Ranadu"), 'satptDiagram.Rdata', sep='/'))
      # load (file='inst/satptDiagram.Rdata')  ## this also loads rminBetts, etc, for xygraph
      cpt <- with(DataS, SpecificHeats ()[, 1] * (1 - Qtot) + StandardConstant('Rw') * Qtot)
      alhv <- 2.501e6
      spt <-  with (DataS, cpt * log (Tk/273.15) - (1-Qtot) * SpecificHeats()[, 3] * log ((PSXC-EWX) / 1000.) + alhv * R / ((1+R)*Tk))
      ## transformation function
      xygraph <- function (r, s) { ## returns pairs of x,y coordinates to plot
        return (c(calpha * (r-rminBetts)/(rmaxBetts-rminBetts) + salpha * (s-sminBetts)/(smaxBetts-sminBetts),
                  -salpha * (r-rminBetts)/(rmaxBetts-rminBetts) + calpha * (s-sminBetts)/(smaxBetts-sminBetts)))
      }
      g <- satptDiagram
      R <- SmoothInterp (DataS$R)
      spt <- SmoothInterp (spt)
      XP <- xygraph (R*1000, spt)
      dim(XP) <- c(length(XP)/2, 2)
      DS <- data.frame (X=XP[,1], Y=XP[,2], spt=spt)
      g <- g + geom_path (data=DS, aes(x=X, y=Y), colour='chocolate', lwd=1)
    }
    if (grepl ('Paluch', input$paluchBetts)) {
      DataS$Rtot <- 1000 * DataS$Rtot
      RT <- binStats (DataS[, c('Rtot', 'PSXC')], xlow=125, xhigh=1025,bins=input$nbsa)
      TQ <- binStats (DataS[, c('THETAQ', 'PSXC')], xlow=125, xhigh=1025, bins=input$nbsa)
      DF2 <- data.frame (RT=RT$ybar, TQ=TQ$ybar, P=RT$xc)
      # if (Trace) {print (c('skewT: DF2 is', DF2))}
      if (Trace) {
        print (summary(DF2$RT))
        print (summary(DF2$TQ))
        print (summary(DF2$P))
      }
      g <- ggplot (DF2, aes (x=TQ, y=RT))
      g <- g + geom_path (col='blue', lwd=2) 
      g <- g + geom_point (col='blue', size=4) 
      ry <- range (RT$ybar, na.rm=TRUE)
      ny <- (ry[2]-ry[1]) * 0.03
      g <- g + geom_text (aes (label=P), size=4, nudge_y=ny)
      g <- g + ylim(rev(range(RT)))
      g <- g + xlab('wet-equivalent potential temperature [K]') + ylab('total water mixing ratio [g/kg]') 
      g <- g + theme_WAC()
      DataS$Rtot <- DataS$Rtot * 0.001
      gtest <<- g
    }
    if (grepl ('stab', input$paluchBetts)) {
      ## need THETA, GGALT, WDC, WSC, EWX, PSXC
      needVariables <- c ('THETA', 'GGALT', 'WSC', 'WDC', 'EWX', 'PSXC')
      ## not requiring THETAV or THETAP; will calculate them from the above
      if (!all (needVariables %in% names (DataS))) {
        if (Trace) {print ('skewT: returning to data () for new variables')}
        if (Trace) {print ('skewT: reset newdata')}
        addedVariables <<- needVariables
        isolate (reac$newdata <- reac$newdata + 1)
        return ()
      }
      ## get profiles of THETAV, THETAE, u, v
      DataS$THETAV <- VirtualPotentialTemperature (VirtualTemperature (DataS$ATX, DataS$R), DataS$PSXC, DataS$EWX)
      DataS$THETAP <- EquivalentPotentialTemperature (DataS$PSXC, DataS$ATX, DataS$EWX)
      DataS$U <- -DataS$WSC * sin (DataS$WDC * pi/180)
      DataS$V <- -DataS$WSC * cos (DataS$WDC * pi/180)
      RI1 <- binStats (DataS[, c('THETAV', 'GGALT')], bins=input$nbsa)
      RI2 <- binStats (DataS[, c('THETAP', 'GGALT')], bins=input$nbsa)
      RI3 <- binStats (DataS[, c('U', 'GGALT')], bins=input$nbsa)
      RI4 <- binStats (DataS[, c('V', 'GGALT')], bins=input$nbsa)
      layout(matrix(1:4, ncol = 2), widths = c(5,5), heights = c(8,8))
      op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))      
      plotWAC (RI1$ybar, RI1$xc, xlab='virtual potential temperature', ylab='altitude [m]', type='b', pch=20)
      SmoothDeriv <- function (.timeSeries, .maxGap=10, .Length=61, .order=3) {
        ## skip if there are fewer than 10 measurements
        if (length (.timeSeries[!is.na(.timeSeries)]) < 10) {return (.timeSeries)}
        d <- zoo::na.approx (as.vector(.timeSeries), maxgap=.maxGap, na.rm = FALSE)
        if (!(.Length %% 2)) {.Length <- .Length + 1}
        d[is.na(d)] <- 0
        return (as.vector (signal::filter(signal::sgolay(.order, .Length, m=1), d)))
      }
      
      RI1S <- SmoothDeriv (RI1$ybar, .Length=input$nbss)
      RI2S <- SmoothDeriv (RI2$ybar, .Length=input$nbss)
      lines (RI2$ybar, RI2$xc, type='b', col='darkgreen')
      legend ('topleft', legend=c('THETAV', 'THETAE'), col=c('blue', 'darkgreen'), lwd=2)
      wmin <- min (c(RI3$ybar, RI4$ybar), na.rm=TRUE)
      wmax <- max (c(RI3$ybar, RI4$ybar), na.rm=TRUE)
      plotWAC (RI3$ybar, RI3$xc, xlab='wind component [m/s]', ylab='altitude [m]', type='b', col='blue', xlim=c(wmin, wmax))
      RI3S <- SmoothDeriv (RI3$ybar, .Length=input$nbss)
      lines (RI4$ybar, RI4$xc, type='b', col='darkgreen')
      legend ('top', legend=c('U', 'V'), col=c('blue', 'darkgreen'), lwd=2)
      RI4S <- SmoothDeriv (RI4$ybar, .Length=input$nbss)
      #       plotWAC (RI1S, RI1$xc, xlab='smoothed THETAV derivative', type='b', col='darkorange')
      #       plotWAC (RI3S, RI3$xc, xlab='smoothed dUdz', type='b', col='darkorange')
      ## now calculate the Ri and N numbers:
      binNorm <- (max (DataS$GGALT, na.rm=TRUE) - min (Data$GGALT, na.rm=TRUE)) / 50
      if (grepl ('V', input$tvORthetap)) {
        Ri <- (StandardConstant ('g_standard') * (RI1S / binNorm) / RI1$ybar) /
          ((RI3S/binNorm)^2 + (RI4S/binNorm)^2)
      } else {
        Ri <- (StandardConstant ('g_standard') * (RI2S / binNorm) / RI2$ybar) /
          ((RI3S/binNorm)^2 + (RI4S/binNorm)^2)
      }
      plotWAC (Ri, RI3$xc, xlab='Richardson Number', type='b', col='blue', xlim=c(-2,5))
      lines (c(0,0,NA,0.25,0.25), c(0,15000,NA,0,15000), col='skyblue', lwd=0.8)
      if (grepl ('V', input$tvORthetap)) {
        Nbv2 <- StandardConstant ('g_standard') * (RI1S  / binNorm) / RI1$ybar
      } else {
        Nbv2 <- StandardConstant ('g_standard') * (RI2S  / binNorm) / RI2$ybar
      }
      Nbv2[is.na (Nbv2) | (Nbv2 < 0)] <- 0
      Nbv <- sqrt (Nbv2)
      bvmax <- max (Nbv, na.rm=TRUE)
      plotWAC (Nbv, RI3$xc, xlab='Brunt-Vaisala number', type='b', col='blue', xlim=c(0,bvmax))
      lines (c(0,0), c(0,15000), col='skyblue', lwd=0.8)
      title (sprintf ('based on %s', input$tvORthetap))
    }
    ## now add in-cloud points
    CDPconc <- names(Data)[which (grepl ('CONCD_', names(Data)))]
    if (any(CDPconc > 5)) {
      DIC <- Data[Data[, CDPconc] > 5, ]
      
      DIC <- DIC[(DIC$Time >= plotSpec$PaluchCTimes[1]) & (DIC$Time < plotSpec$PaluchCTimes[2]), ]
      if (grepl ('Betts', input$paluchBetts)) {
        cpt <- with(DIC, SpecificHeats ()[, 1] * (1 - Qtot) + StandardConstant('Rw') * Qtot)
        alhv <- 2.501e6
        spt <- with (DIC, cpt * log (Tk/273.15) - (1-Qtot) * SpecificHeats()[, 3] * log ((PSXC-EWX) / 1000.) + alhv * R / ((1+R)*Tk))
        XP <- xygraph (DIC$Rtot*1000, spt)
        dim(XP) <- c(length(XP)/2, 2)
        DSC <- data.frame (X=XP[,1], Y=XP[,2], spt=spt)
        g <- g + geom_point (data=DSC, aes(x=X, y=Y), colour='red', pch=20, size=1)
      }
      if (grepl ('Paluch', input$paluchBetts)) {
        DIC$Rtot <- DIC$Rtot * 1000
        g <- g + geom_point (data=DIC, aes(x=THETAQ, y=Rtot), colour='red', pch=20)
      }
    }
    if (!grepl ('stab', input$paluchBetts)) {
      vp <- viewport()
      suppressWarnings (print (g, vp=vp))
    }
    
    #     with (Data, plotWAC (THETAQ, Rtot, type='l', xlab='wet-equivalent potential temperature',
    #                          ylim=rev(range(Rtot, na.rm=TRUE)),
    #                          ylab='total water mixing ratio [g/kg]', col='blue', cex.lab=2))
  }, width=640, height=640)
  
  
  output$stats <- renderDataTable ({    ## stats
    if (Trace) {print ('stats: entered')}
    input$times
    input$panels
    Ds <- limitData (data(), input)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    plotV <- vector ()
    for (i in 1:plotSpec$Plot[[input$plot]]$panels) {
      plotV <- c(plotV, plotSpec$Plot[[input$plot]]$panel[[i]]$var)
    }
    plotV <- unique (plotV)
    Ds <- Ds[, c('Time', plotV)]
    Ds <- Ds[(Ds$Time >= plotSpec$Times[1]) & (Ds$Time < plotSpec$Times[2]), ]
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
    if (Trace) {
      print ('stats: output is')
      print (str(Dstats))
    }
    Dstats
  }, options=list(paging=FALSE, searching=FALSE))
  
  output$statistics <- renderDataTable ({    ## statistics
    if (Trace) {print ('statistics: entered')}
    input$times
    reac$newstats
    ## check if any requested variables not present in Data:
    if (any (!(plotSpec$StatVar %in% VarList))) {
      VarList <<- unique (c(VarList, plotSpec$StatVar))
      isolate (reac$newdata <- reac$newdata + 1)
      isolate (reac$newstats <- reac$newstats + 1)
      return()
    }
    Ds <- limitData (data(), input, input$limits2a)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    #     plotV <- vector ()
    #     for (i in 1:plotSpec$Plot[[input$plot]]$panels) {
    #       plotV <- c(plotV, plotSpec$Plot[[input$plot]]$panel[[i]]$var)
    #     }
    #     plotV <- unique (plotV)
    Ds <- Ds[, c('Time', sort(plotSpec$StatVar))]
    Ds <- Ds[(Ds$Time >= plotSpec$Times[1]) & (Ds$Time < plotSpec$Times[2]), ]
    if (grepl ('list', input$statslist)) {
      ## average in NAVE-second intervals
      NAVE <- input$avgsec
      if (NAVE <= 1) {
        DsA <- Ds
      } else {
        NC <- ncol (Ds)
        isq <- seq (1, nrow(Ds), by=NAVE)
        DsA <- Ds[isq, ]
        for (j in 1:length(isq)) {
          DsA[j, 2:NC] <- apply (Ds[isq[j]:(isq[j]+NAVE-1), 2:NC], 2, function (x) mean (x, na.rm=TRUE)) 
        }
        ## assign factor to Ds: -- commented because this was slower
        # Ds$FCTR <- findInterval (Ds$Time, Ds$Time[1]+seq (0, nrow(Ds), by=NAVE), all.inside=TRUE)
        # for (ifctr in 1:max(Ds$FCTR)) {
        #   DsA[ifctr, 2:NC] <- apply(Ds[Ds$FCTR == ifctr, 2:NC], 2, function (x) mean (x, na.rm=TRUE))
        # }
      }
      DsA$Time <- formatTime(DsA$Time)
      options(digits=5)
      return (DsA)
    } else {
      Dstats <- data.frame ()
      Dstats['Time', 1] <- '  Time'
      Dstats['Time', 2] <- NA
      Dstats['Time', 3] <- NA
      Dstats['Time', 4] <- formatTime (Ds$Time[1])
      Dstats['Time', 5] <- formatTime (Ds$Time[nrow(Ds)])
      nms <- names(Ds)
      nms <- nms[nms != 'Time']
      for (nm in nms) {
        Dstats[nm, 1] <- nm
        Dstats[nm, 2] <- mean (Ds[, nm], na.rm=TRUE)
        Dstats[nm, 3]   <- sd   (Ds[, nm], na.rm=TRUE)
        Dstats[nm, 4]  <- min  (Ds[, nm], na.rm=TRUE)
        Dstats[nm, 5]  <- max  (Ds[, nm], na.rm=TRUE)
      }
      names(Dstats) <- c('variable', 'mean', 'sd', 'min', 'max')
      ## alphabetical order:
      Dstats <- Dstats[do.call (order, Dstats), ] 
      # Dstats[2:nrow(Dstats), 2:5] <- format(Dstats[2:nrow(Dstats),2:5], digits=5, scientific=FALSE)
      for (k in 2:5) {
        Dstats[2:nrow(Dstats), k] <- sprintf('%.3f', as.numeric(Dstats[2:nrow(Dstats), k]))
      }  
      if (Trace) {
        print ('statistics: output is')
        print (str(Dstats))
      }
      return (Dstats)
    }
  }, options=list(paging=TRUE, searching=TRUE))
  
  output$hist <- renderPlot ({  ## hist
    input$panels
    input$times
    layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(8,8,8))
    op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))
    if (Trace) {print ('hist: entered')}
    Ds <- limitData (data(), input)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    plotV <- vector ()
    for (i in 1:plotSpec$Plot[[input$plot]]$panels) {
      plotV <- c(plotV, plotSpec$Plot[[input$plot]]$panel[[i]]$var)
    }
    plotV <- unique (plotV)
    Ds <- Ds[, c('Time', plotV)]
    Ds <- Ds[(Ds$Time >= plotSpec$Times[1]) & (Ds$Time < plotSpec$Times[2]), ]
    kount <- 0
    for (nm in names (Ds)) {
      if (nm == 'Time') {next}
      kount <- kount + 1
      if (kount > 6) {break}
      hist (Ds[ ,nm], freq=FALSE, breaks=50, xlab=nm, 
            ylab='probability density', main=NA)
    }
  }, width=780, height=640)
  
  plotHist <- function (input) {  ## plotHist
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, input$limits3)
    plt <- input$plot
    if (Trace) {
      print (sprintf ('plotHist: entered,  plt=%d', plt))
    }
    spec <- plotSpec$Hist[[plt]]
    nrws <- ceiling (spec$panels / spec$columns)
    nmx <- nrws * spec$columns
    fill.colors <- gray.colors(6, .9, .5, alpha=0.5)
    op <- par (mar=c(5,4,1,2)+0.1, oma=c(1.1,0,0,0))
    gp <- list()
    var <- list()
    colrs <- list()
    lws <- list()
    lts <- list()
    for (pnl in 1:spec$panels) {
      var[[pnl]] <- spec$panel[[pnl]]$var
      colrs[[pnl]] <- spec$panel[[pnl]]$col
      lws[[pnl]] <- spec$panel[[pnl]]$lw
      lts[[pnl]] <- spec$panel[[pnl]]$lt
      yl <- NULL
      if (spec$panel[[pnl]]$fixed) {yl <- spec$panel[[pnl]]$ylim}
      vr <- var[[pnl]]
      Var1 <- spec$panel[[pnl]]$var[1]
      if (input$limits3) {
        DataX <- DataV[, vr]
      } else {
        DataX <- DataR[, vr]
      }
      g <- ggplot (data=DataX)
      for (i in 1:length(vr)) {
        v <- sprintf ('var1[%d]', i)
        if (input$densityH) {
          b <- sprintf ("aes (x=%s, ..density.., colour='%s', size='%s', fill='%s', lty='%s')", 
                      vr[i], vr[i], vr[i], vr[i], vr[i])
        } else {
          b <- sprintf ("aes (x=%s, colour='%s', size='%s', fill='%s', lty='%s')", 
                        vr[i], vr[i], vr[i], vr[i], vr[i])
        }
        g <- g + geom_histogram (eval(parse(text=b)),
                                 bins=plotSpec$Hist[[plt]]$panel[[pnl]]$bins, na.rm=TRUE)          
        
      }
      if (!is.null (yl)) {
        g <- g + xlim (yl)
      }
      g <- g + scale_colour_manual(name='bar',
                                   labels = var[[pnl]],
                                   values = colrs[[pnl]])
      g <- g + scale_linetype_manual ("bar", labels=var[[pnl]], 
                                      values = spec$panel[[pnl]]$lt)
      g <- g + scale_size_manual ('bar', labels=var[[pnl]],
                                  values=lws[[pnl]])
      g <- g + scale_fill_manual ("bar", labels=var[[pnl]], 
                                  values = fill.colors[1:length(var[[pnl]])])
      g <- g + xlab(vr[1]) + theme_WAC()
      ## add cumulative distribution
      if (input$cdf) {
        a <- ggplot_build(g)
        yrange <- a$layout$panel_ranges[[1]]$y.range
        xrange <- a$layout$panel_ranges[[1]]$x.range
        for (j in 1:length (vr)) {
          yc <- cumsum (a$data[[j]]$density) * (a$data[[j]]$x[3] - a$data[[j]]$x[2])
          yc <- yc * yrange[2]
          dt <- data.frame (x=a$data[[1]]$x, y=yc)
          # dt[nrow(dt), ] <- NA
          g <- g + geom_line (data=dt, aes(x,y), colour=colrs[[pnl]][j], lty=lts[[pnl]][j], lwd=0.6, na.rm=TRUE)
        }
        gsave <<- g
        ## now adjust to add right axis for cdf
        xr <- xrange[2] + 0.05 * (xrange[2] - xrange[1])
        g3 <- ggplot (data=DataX) +
          stat_ecdf(aes_string(Var1), geom = "step", colour=NA, na.rm=TRUE) +
          #           g3 <- g3 + geom_text (aes (label='cumulative', x=xrange[2], y=0.5), colour='blue', 
          #                                      angle=90, size=6) +
          theme_WAC() %+replace%
          theme(panel.background = element_rect(fill = NA)) %+replace%
          theme(panel.grid.major = element_line(color = "white", linetype=2, size=0.5)) %+replace%
          theme(axis.text.y = element_text (margin=margin(0,20,0,20)))
        # extract gtable
        t1 <- ggplot_gtable (ggplot_build (g))
        t2 <- ggplot_gtable (ggplot_build (g3))
        t2 <<- t2
        
        # overlap the panel of 2nd plot on that of 1st plot
        pp <- c(subset(t1$layout, name == "panel", se = t:r))
        g <- gtable_add_grob(t1, t2$grobs[[which(t2$layout$name == "panel")]], pp$t,
                             pp$r, pp$b, pp$l)
        
        # axis tweaks
        ia <- which(t2$layout$name == "axis-l")
        ga <- t2$grobs[[ia]]
        ax <- ga$children[[2]]
        ax$widths <- rev(ax$widths)
        ax$grobs <- rev(ax$grobs)
        ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(-0.5, "npc") + unit(0.15, "cm")
        g <- gtable_add_cols(g, t2$widths[t2$layout[ia, ]$l], length(g$widths) - 1)
        g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
        gp[[pnl]] <- g
      } else {
        gp[[pnl]] <- ggplot_gtable(ggplot_build(g))
      }
    }
    # multiplot (plotlist=gp, cols=spec$columns)
    if (plotSpec$Hist[[plt]]$panels == 1) {
      grid.draw (g)
    } else {
      grid.newpage()
      vp <- list()
      rsize <- 1/nrws
      csize <- 1/spec$columns
      yv <- 1 - rsize / 2
      xv <- csize / 2
      for (k in 1:spec$panels) {
        vp[[k]] <- viewport (xv, yv, width=csize, height=rsize)
        pushViewport (vp[[k]])
        grid.draw (gp[[k]])
        popViewport()
        yv <- yv - rsize
        if (yv < 0) {
          yv <- 1 - rsize/2
          xv <- xv + csize
        }
      }
    }
    gp <<- gp
  }
  
  output$histogram <- renderPlot ({  ## histogram
    input$hpanels
    input$hcols
    # input$times
    reac$newhistogram
    Project <- plotSpec$Project
    if (Trace) {print ('histogram: entered')}
    if (Trace) {
      print (sprintf ('histogram: newhistogram is %d', reac$newhistogram))
      print (sprintf ('histogram: global plotSpec$Times are %s %s',
                      formatTime (plotSpec$Times[1]), formatTime (plotSpec$Times[2])))
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newhistogram <- reac$newhistogram + 1 
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('histogram: exiting for new data')}
      return()
    }
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= plotSpec$Times[1]) & (Data$Time < plotSpec$Times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, input$limits3)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      if (plotSpec$TypeFlight == 'F') {
        FigFooter <<- sprintf("%s rf%02dF %s %s-%s UTC,", Project, 
                              plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                              strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                              strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                       format="%H:%M:%S", tz='UTC'))        
      } else {
        FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                            plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                            strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                            strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                     format="%H:%M:%S", tz='UTC'))
      }
    )
    FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    AddFooter <<- function() {
      isolate (
        mtext(paste(FigFooter,'generated by Ranadu hist ', input$plot,
                    FigDatestr),1,outer=T,cex=0.75)
      )
    }
    plotHist (input)    ## isolated in function to be able to save via PDF/PNG
    if (input$footer) {AddFooter ()}
    if (Trace) {print ('histogram: finished generation')}
  }, width=780, height=640)
  
  output$barWvsZ <- renderPlot ({
    if (Trace) {print ('barWvsZ: entered')}
    input$times
    input$panels
    layout (matrix(1:6, ncol=3), widths=c(5,5,5), heights=c(8,8))
    op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))
    Ds <- limitData (data(), input)
    if ('GGALT' %in% names(Ds)) {
      Ds$Z <- Ds$GGALT
    } else if ('GALT' %in% names(Ds)) {
      Ds$Z <- Ds$GALT
    } else if ('ALT' %in% names(Ds)) {
      Ds$Z <- Ds$ALT
    }
    plotV <- vector ()
    for (i in 1:plotSpec$Plot[[input$plot]]$panels) {
      plotV <- c(plotV, plotSpec$Plot[[input$plot]]$panel[[i]]$var)
    }
    plotV <- unique (plotV)
    Ds <- Ds[, c('Time', plotV, 'Z')]
    Ds <- Ds[(Ds$Time >= plotSpec$Times[1]) & (Ds$Time < plotSpec$Times[2]), ]
    Ds <- Ds[!is.na (Ds$Z), ]
    kount <- 0
    for (nm in names (Ds)) {
      if (nm == 'Time') {next}
      if (nm == 'Z') {next}
      kount <- kount + 1
      if (kount > 6) {break}
      DB <- data.frame ('Z1'=Ds[, nm])
      DB[Ds$Z > 1000, 'Z1'] <- NA
      for (j in 2:15) {
        zmax <- j*1000
        zmin <- zmax-1000
        V <- sprintf ('Z%d', j)
        DB[,V] <- Ds[, nm]
        DB[(Ds$Z < zmin) | (Ds$Z > zmax), V] <- NA
      }
      boxplot (DB, horizontal=TRUE, outline=TRUE, 
               xlab=nm, ylab='altitude [km]', names=NULL)
    }
  }, width=780, height=640)
  
  output$checkV <- renderDataTable ({
    if (Trace) {print ('checkV: entered')}
    input$times
    input$plot_click
    input$RefT
    input$panels
    Ds <- limitData (data(), input)
    plotV <- vector ()
    for (i in 1:plotSpec$Plot[[input$plot]]$panels) {
      plotV <- c(plotV, plotSpec$Plot[[input$plot]]$panel[[i]]$var)
    }
    plotV <- unique (plotV)
    Ds <- Ds[, c('Time', plotV)]
    # Ds <- Ds[(Ds$Time >= (checkTime - 5)) & (Ds$Time <= checkTime + 5), ]
    Ds <- Ds[(Ds$Time >= (checkTime-30)) & (Ds$Time <= (checkTime+30)), ]
    Dr <- Ds[Ds$Time == checkTime, ]
    for (nn in plotV) {Dr[1,nn] <- mean (Ds[, nn], na.rm=TRUE)}
    Dr$Time <- formatTime(Dr$Time)
    Dr
  }, options=list(paging=FALSE, searching=FALSE))
  
  output$listing <- renderDataTable ({
    if (Trace) {print ('listing: entered')}
    input$times
    input$panels
    Ds <- limitData (data(), input)
    plotV <- vector ()
    for (i in 1:plotSpec$Plot[[input$plot]]$panels) {
      plotV <- c(plotV, plotSpec$Plot[[input$plot]]$panel[[i]]$var)
    }
    plotV <- unique (plotV)
    Ds <- Ds[, c('Time', plotV)]
    Ds <- Ds[(Ds$Time >= plotSpec$Times[1]) & (Ds$Time < plotSpec$Times[2]), ]
    Ds$Time <- formatTime(Ds$Time)
    Ds
  }, options=list(paging=TRUE, searching=TRUE))
  
  output$image2d <- renderPlot ({
    reac$new2d
    mode <- 'page'
    mode <- 'record'
    if (!exists ('cfile')) {
      # plotSpec$fname2d <<- '/Data/DC3/20120526_191349_rf05.2d'
      if (!('fname2d' %in% names (plotSpec))) {
        print ('must select a 2D file to get the 2D display')
        return ()
      }
      cfile <<- file(plotSpec$fname2d, 'rb')
      updateTextInput (session, 'fnametext', value=plotSpec$fname2d)
      xmlinfo <- readBin(cfile, character(), 1)
      probe <<- sub('.*\\n', '', xmlinfo)
      xmlinfo <- sub(sprintf ('\\n%s$',probe), '\n', xmlinfo)
      xmlD <- xmlParse (xmlinfo)
      probeXML <- toString.XMLNode(xmlRoot(xmlD)[['probe']])
      p1 <- sub ('.*resolution=\"', '', probeXML)
      resltion <<- as.numeric(sub('\".*', '', p1))
      ## there may be multiple probes...  need fix here
      p2 <- sub ('.*nDiodes=\"', '', probeXML)
      nDiodes <<- as.numeric(sub('\".*', '', p2))
      hour <- as.integer (readBin(cfile, raw(), 1))
      a <- readBin(cfile, integer(), 8, size=2, signed=FALSE, endian='swap')
      minute <- a[1]
      second <- a[2]
      year <<- a[3]
      month <<- a[4]
      day <<- a[5]
      tas <- a[6]
      msec <- a[7]
      overld <- a[8]
      end2d <<- c(hour, minute, second)
      nextday <<- ifelse (end2d[1] >= 12, TRUE, FALSE)
      #     
      #     print (sprintf ('date %d-%02d-%02d time %d:%02d:%02d.%03d probe %s resltion %d diodes %d tas %d overld %d',
      #                     year, month, day, hour, minute, second, msec, probe, resltion, nDiodes, tas, overld))
      image <- readBin(cfile, raw(), 4096, endian='swap')
    }
    np <- 0
    i <- 0
    
    ## function used for bit masking 
    bits <- function(i,m) {
      ifelse (bitwAnd (i,m), 1, 0.05)
    }
    mask <- c(0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x01)
    
    while (TRUE) {  ## this uses breaks to end after a record or a page
      i <- i + 1
      if (TRUE) {
        # ptm <- proc.time ()
        img <- as.integer (readRecord (cfile))
        if (length(img) < 10) {break}
        l2d <- last2d[1]*3600 + last2d[2]*60 + last2d[3]
        s2d <- start2d[1]*3600 + start2d[2]*60 + start2d[3]
        ## bug here: this doesn't work right for max2d!=0 and the 'prev2d' button
        if ((input$max2d != 0) && (s2d-l2d < input$max2d)) {next}
        ## convert input$times[1] to HHMMSS format for comparison to the 2D time
        Ttest <- as.integer (gsub(':', '', formatTime(input$times[1])))
        if (Ttest < 120000 && nextday) {Ttest <- Ttest + 240000}
        stest <- start2d
        if ((stest[1] < 12) && nextday) stest[1] <- stest[1] + 24
        if (Ttest >= (10000*stest[1] + 100*stest[2] + stest[3])) {next}
        last2d <<- start2d
        # print (proc.time () - ptm)
        
        ## this is the image processor -- bits is a function, defined above
        m <- unlist (lapply(img, bits, mask))
        dim(m) <- c(64,512)
        
        # print (proc.time () - ptm)
        if (input$mode2d == 'page') {
          if (np == 0) {
            op <- par(bg = "thistle")
            op <- par (bg = 'grey70')
            # op <- par (bg = 'lightyellow')
            plot (c(1,512), c(1,512), type='n', xaxt='n', yaxt='n', xlab='', ylab='', asp=0.5)
            pageStart <- start2d
          }
          xi <- 1+(np %% 2) * 256
          yi <- 512 - 64 + 1 - (np %/% 2) * 64
          rasterImage (m, xi, yi, xi+255, yi+63)
          np <- (np + 1) %% 16
          if (np == 0) {
            title (sprintf ('%d-%02d-%-2d probe %s resolution %d diodes %d %d:%02d:%02d -- %d:%02d:%02d', 
                            year, month, day, probe, resltion, nDiodes, 
                            pageStart[1], pageStart[2], pageStart[3], end2d[1], end2d[2], end2d[3]), 
                   col.main='blue',cex.main=0.8)
            break
          }
          # print (c('p', prot.time() - ptm))
        } else {
          op <- par(bg = "thistle")
          op <- par (bg = 'grey70')
          # op <- par (bg = 'lightyellow')
          plot(c(1,256),c(1,256), type='n', xaxt='n', yaxt='n', xlab='', ylab='', asp=0.5)
          #       m[,64] <- FALSE
          #       m[,128] <- FALSE
          rasterImage (m[, 1:256], 1, 131, 256, 256)
          rasterImage (m[, 257:512], 1, 1, 256, 126)
          title (sprintf ('%d-%02d-%-2d probe %s resolution %dum diodes %d %d:%02d:%02d -- %d:%02d:%02d', 
                          year, month, day, probe, resltion, nDiodes, 
                          start2d[1], start2d[2], start2d[3], end2d[1], end2d[2], end2d[3]), 
                 col.main='blue',cex.main=0.8)
          break
        }
      } else {
        readRecord (cfile)
      }
    }
    #     if (mode == 'page') {
    #       title (sprintf ('%d-%02d-%-2d probe %s resolution %d diodes %d %d:%02d:%02d -- %d:%02d:%02d', 
    #                       year, month, day, probe, resltion, nDiodes, 
    #                       pageStart[1], pageStart[2], pageStart[3], end2d[1], end2d[2], end2d[3]), 
    #              col.main='blue',cex.main=0.8)
    #     }
    
    # plot(c(1,512),c(1,256), type='n', xaxt='n', yaxt='n', xlab='', ylab='', asp=1)
    # ptm <- proc.time()
    # img <- readRecord (cfile, i, resltion, nDiodes)
    # writeBin(img, 'twod.image.gray')
    # unlink ('twod.image.png')
    # system ('convert -size 64x512 -depth 1 -rotate -90 twod.image.gray twod.image.png', wait=TRUE)
    # img.native <- readPNG('twod.image.png', native=TRUE)
    # rasterImage(img.native, 0,0,512,64)
    # proc.time() - ptm
    
    
    
    ## /* Values currently in use for the 'id' field. */
    #define PMS2D_C1         0x4331		// First PMS-2DC
    #define PMS2D_C2         0x4332		// Second PMS-2DC
    #define PMS2D_C4         0x4334         // RAF 64 diode 25 um Fast 2DC
    #define PMS2D_C6         0x4336         // RAF 64 diode 10 um Fast 2DC
    #define PMS2D_G1         0x4731		// First 2D Greyscale; unused to date
    #define PMS2D_H1         0x4831		// SPEC HVPS
    #define PMS2D_P1         0x5031		// First PMS-2DP
    #define PMS2D_P2         0x5032		// Second PMS-2DP
    
    #   struct P2d_rec {
    #     short id;                             /* 'P1','C1','P2','C2', H1, etc */
    #       short hour;
    #     short minute;
    #     short second;
    #     short year;                           /* starting in 2007 w/ PACDEX */
    #       short month;                          /* starting in 2007 w/ PACDEX */
    #       short day;                            /* starting in 2007 w/ PACDEX */
    #       short tas;                            /* true air speed	*/
    #       short msec;                           /* msec of this record	*/
    #       short overld;                         /* overload time, msec	*/
    #       unsigned char data[4096];		/* image buffer		*/
    #   };
    # typedef struct P2d_rec P2d_rec;
    
  })
  
  outputOptions (output, 'display', priority=-110)
  outputOptions (output, 'stats', priority=-20)
  outputOptions (output, 'listing', priority=-20)
  outputOptions (output, 'hist', priority=-20)
  outputOptions (output, 'barWvsZ', priority=-20)
  outputOptions (output, 'statistics', priority=-20)
  outputOptions (output, 'histogram', priority=-20)
  outputOptions (output, 'skewT', priority=-20)
})


require (shiny)

shinyServer(function(input, output, session) {
  
  
  ################ OBSERVERS ########################
  
  ## use an observer for each item in the plot definition:
  ##    -> write to global plotSpec or other global location
  ##    -> set appropriate reac$ variable (e.g., for new data, new display)
  
  observe ({
    
  }, priority=0)
  
  exprProject <- quote ({ 
    plotSpec$Project <<- input$Project
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print ('reset newdata 1')}
  })
  obsProject <- observe (exprProject, quoted=TRUE)
  
  exprFlight <- quote ({
    plotSpec$Flight <<- input$Flight
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print ('reset newdata 2')}
  })
  obsFlight <- observe (exprFlight, quoted=TRUE)
  
  exprTypeFlight <- quote ({
    plotSpec$TypeFlight <<- input$typeFlight
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print ('reset newdata 3')}
  })
  obsTypeFlight <- observe (exprTypeFlight, quoted=TRUE)
  
  exprTime <- quote ({
    times <<- input$times
    updateTextInput (session, 'tstart', value=formatTime(times[1]))
    updateTextInput (session, 'tend', value=formatTime(times[2]))
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newstats <- reac$newstats + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newdisplay 20')}
  })
  obsTime <- observe (exprTime, quoted=TRUE)
  
  exprTstart <- quote ({
    txt <- input$tstart
    hhmmss <- as.integer (gsub (':', '', txt))
    i1 <- getIndex (Data, hhmmss)
    if (i1 < 1) {i1 <- 1}
    tms <- times
    tms[1] <- Data$Time[i1]
    updateSliderInput (session, 'times', value=tms)
    times <<- tms
    #     isolate (reac$newdisplay <- reac$newdisplay + 1)
    #     isolate (reac$newhistogram <- reac$newhistogram + 1)
    #     isolate (reac$newstats <- reac$newstats + 1)
    #     isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset new times 25')}
  })
  obsTstart <- observe (exprTstart, quoted=TRUE)
  
  exprTend <- quote ({
    txt <- input$tend
    hhmmss <- as.integer (gsub (':', '', txt))
    i2 <- getIndex (Data, hhmmss)
    if (i2 < 1) {i2 <- nrow (Data)}
    tms <- times
    tms[2] <- Data$Time[i2]
    if (Trace) {print (c('updating time to', formatTime(tms[1]), formatTime(tms[2])))}
    updateSliderInput (session, 'times', value=tms)
    times <<- tms
    #     isolate (reac$newdisplay <- reac$newdisplay + 1)
    #     isolate (reac$newhistogram <- reac$newhistogram + 1)
    #     isolate (reac$newstats <- reac$newstats + 1)
    #     isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset new times 26')}
  })
  obsTend <- observe (exprTend, quoted=TRUE)
  
  exprPanels <- quote ({
    plotSpec$Plot[[input$plot]]$panels <<- input$panels
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 4')}
  })
  obsPanels <- observe (exprPanels, quoted=TRUE)
  
  exprCols <- quote ({
    plotSpec$Plot[[input$plot]]$columns <<- input$cols
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 5')}
  })
  obsCols <- observe (exprCols, quoted=TRUE)
  
  exprlogY <- quote ({
    plotSpec$Plot[[input$plot]]$panel[[input$panel]]$logY <<- input$logY
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 6')}
  })
  obsLogY <- observe (exprlogY, quoted=TRUE)
  
  exprFixed <- quote ({
    plotSpec$Plot[[input$plot]]$panel[[input$panel]]$fixed <<- input$fixed
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 7')}
  })
  obsFixed <- observe (exprFixed, quoted=TRUE)
  
  exprPanelMin <- quote ({
    plotSpec$Plot[[input$plot]]$panel[[input$panel]]$ylim[1] <<- input$panelMin
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 8')}
  })
  obsPanelMin <- observe (exprPanelMin, quoted=TRUE)
  
  exprPanelMax <- quote ({
    plotSpec$Plot[[input$plot]]$panel[[input$panel]]$ylim[2] <<- input$panelMax
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 9')}
  })
  obsPanelMax <- observe (exprPanelMax, quoted=TRUE)
  
  exprRestrict <- quote ({
    plotSpec$Plot[[input$plot]]$restrict <<- input$restrict
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 10')}
  })
  obsRestrict <- observe (exprRestrict, quoted=TRUE)
  
  exprhPanels <- quote ({
    plotSpec$Hist[[input$plot]]$panels <<- input$hpanels
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 30')}
  })
  obshPanels <- observe (exprhPanels, quoted=TRUE)
  
  exprhCols <- quote ({
    plotSpec$Hist[[input$plot]]$columns <<- input$hcols
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 31')}
  })
  obshCols <- observe (exprhCols, quoted=TRUE)
  
  exprhPanel <- quote ({
    plt <- input$plot
    pnl <- input$hpanel
    updateCheckboxInput (session, 'hlogY', value=plotSpec$Hist[[plt]]$panel[[pnl]]$logY)
    updateCheckboxInput (session, 'hfixed', value=plotSpec$Hist[[plt]]$panel[[pnl]]$fixed)
    updateNumericInput (session, 'hlineV', value=1)
    updateSelectInput (session, 'haddVarP', selected=plotSpec$Hist[[plt]]$panel[[pnl]]$var[1])
    updateSelectInput (session, 'hvarColor', selected=plotSpec$Hist[[plt]]$panel[[pnl]]$col[1])
    updateNumericInput (session, 'hlineW', value=plotSpec$Hist[[plt]]$panel[[pnl]]$lw[1])
    updateRadioButtons (session, 'hlineStyle', selected=ltyps[plotSpec$Hist[[plt]]$panel[[pnl]]$lt[1]])
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 12')}
  })
  obshPanel <- observe (exprhPanel, priority=8, quoted=TRUE)    
  
  exprhlogY <- quote ({
    plotSpec$Hist[[input$plot]]$panel[[input$hpanel]]$logY <<- input$hlogY
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 6')}
  })
  obshLogY <- observe (exprhlogY, priority=2, quoted=TRUE)
  
  exprhFixed <- quote ({
    plotSpec$Hist[[input$plot]]$panel[[input$hpanel]]$fixed <<- input$hfixed
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 7')}
  })
  obshFixed <- observe (exprhFixed, priority=2, quoted=TRUE)
  
  exprhPanelMin <- quote ({
    plotSpec$Hist[[input$plot]]$panel[[input$hpanel]]$ylim[1] <<- input$hpanelMin
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 8')}
  })
  obshPanelMin <- observe (exprhPanelMin, priority=2, quoted=TRUE)
  
  exprhPanelMax <- quote ({
    plotSpec$Hist[[input$plot]]$panel[[input$hpanel]]$ylim[2] <<- input$hpanelMax
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 9')}
  })
  obshPanelMax <- observe (exprhPanelMax, priority=2, quoted=TRUE)
  
  exprhRestrict <- quote ({
    plotSpec$Hist[[input$plot]]$restrict <<- input$limits3
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 10')}
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
      if (Trace) {print (c('var set to ', v))}
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
    if (Trace) {print ('processed hlineV [13]')}
    # isolate (reac$newhistogram <- reac$newhistogram + 1)
    # if (Trace) {print ('reset newhistogram 13')}
  })
  obshlineV <- observe (exprhlineV, priority=5, quoted=TRUE)
  
  ## observers for scatterplots
  exprsPanels <- quote ({
    plotSpec$Scat[[input$plot]]$panels <<- input$spanels
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 30')}
  })
  obssPanels <- observe (exprsPanels, quoted=TRUE)
  
  exprsCols <- quote ({
    plotSpec$Scat[[input$plot]]$columns <<- input$scols
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 31')}
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
    updateRadioButtons (session, 'symbol', selected=ltyps[plotSpec$Scat[[plt]]$panel[[pnl]]$symbol[1]])
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 12')}
  })
  obssPanel <- observe (exprsPanel, priority=8, quoted=TRUE)    
  
  exprslogX <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$logX <<- input$slogX
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 6')}
  })
  obssLogX <- observe (exprslogX, priority=2, quoted=TRUE)  
  
  exprslogY <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$logY <<- input$slogY
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 6')}
  })
  obssLogY <- observe (exprslogY, priority=2, quoted=TRUE)
  
  exprsFixed <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$fixed <<- input$sfixed
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 7')}
  })
  obssFixed <- observe (exprsFixed, priority=2, quoted=TRUE)
  
  exprsPanelMinx <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$xlim[1] <<- input$spanelMinx
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 8')}
  })
  obssPanelMinx <- observe (exprsPanelMinx, priority=2, quoted=TRUE)
  
  exprsPanelMaxx <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$xlim[2] <<- input$spanelMaxx
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 9')}
  })
  obssPanelMaxx <- observe (exprsPanelMaxx, priority=2, quoted=TRUE)
  
  exprsPanelMiny <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$ylim[1] <<- input$spanelMiny
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 8')}
  })
  obssPanelMiny <- observe (exprsPanelMiny, priority=2, quoted=TRUE)
  
  exprsPanelMaxy <- quote ({
    plotSpec$Scat[[input$plot]]$panel[[input$spanel]]$ylim[2] <<- input$spanelMaxy
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 9')}
  })
  obssPanelMaxy <- observe (exprsPanelMaxy, priority=2, quoted=TRUE)
  
  exprsRestrict <- quote ({
    plotSpec$Scat[[input$plot]]$restrict <<- input$limits4
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 10')}
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
      updateRadioButtons (session, 'symbol', selected=ltyps[plotSpec$Scat[[plt]]$panel[[pnl]]$symbol[lv]])
    } else {
      vy <- plotSpec$Scat[[plt]]$panel[[pnl]]$vary
      vy <- c(vy, vy[length(vy)])
      plotSpec$Scat[[plt]]$panel[[pnl]]$vary <<- vy
      ## varx remains the same
      if (Trace) {print (c('vary set to ', vy))}
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
    if (Trace) {print ('processed slineV [13]')}
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
      print ('reset newscat 41')
      print (c('plt,pnl,lv,symbol:',plt,pnl,lv,input$symbol))
    }
  })
  obsssymbol <- observe (exprssymbol, quoted=TRUE)
  
  exprssize <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$spanel)
    lv <- input$slineV
    plotSpec$Scat[[plt]]$panel[[pnl]]$size[lv] <<- input$ssize
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 40')}
  })
  obsssize <- observe (exprssize, quoted=TRUE)
  
  ## observers for binned plots
  exprbPanels <- quote ({
    plotSpec$Bin[[input$plot]]$panels <<- input$bpanels
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 30')}
  })
  obsbPanels <- observe (exprbPanels, quoted=TRUE)
  
  exprbCols <- quote ({
    plotSpec$Bin[[input$plot]]$columns <<- input$bcols
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 31')}
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
    updateRadioButtons (session, 'bsymbol', selected=ltyps[plotSpec$Bin[[plt]]$panel[[pnl]]$symbol[1]])
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 12')}
  })
  obsbPanel <- observe (exprbPanel, priority=8, quoted=TRUE)    
  
  exprblogX <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$logX <<- input$blogX
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 6')}
  })
  obsbLogX <- observe (exprblogX, priority=2, quoted=TRUE)  
  
  exprblogY <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$logY <<- input$blogY
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 6')}
  })
  obsbLogY <- observe (exprblogY, priority=2, quoted=TRUE)
  
  exprbFixed <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$fixed <<- input$bfixed
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 7')}
  })
  obsbFixed <- observe (exprbFixed, priority=2, quoted=TRUE)
  
  exprbPanelMinx <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$xlim[1] <<- input$bpanelMinx
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 8')}
  })
  obsbPanelMinx <- observe (exprbPanelMinx, priority=2, quoted=TRUE)
  
  exprbPanelMaxx <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$xlim[2] <<- input$bpanelMaxx
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 9')}
  })
  obsbPanelMaxx <- observe (exprbPanelMaxx, priority=2, quoted=TRUE)
  
  exprbPanelMiny <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$ylim[1] <<- input$bpanelMiny
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 8')}
  })
  obsbPanelMiny <- observe (exprbPanelMiny, priority=2, quoted=TRUE)
  
  exprbPanelMaxy <- quote ({
    plotSpec$Bin[[input$plot]]$panel[[input$bpanel]]$ylim[2] <<- input$bpanelMaxy
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 9')}
  })
  obsbPanelMaxy <- observe (exprbPanelMaxy, priority=2, quoted=TRUE)
  
  exprbRestrict <- quote ({
    plotSpec$Bin[[input$plot]]$restrict <<- input$limits4
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 10')}
  })
  obsbRestrict <- observe (exprbRestrict, priority=2, quoted=TRUE)
  
  exprblineV <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$bpanel)
    lv <- input$blineV
    if (lv <= length (plotSpec$Bin[[plt]]$panel[[pnl]]$vary)) {
      updateSelectInput (session, 'baddVarP1', selected=plotSpec$Bin[[plt]]$panel[[pnl]]$varx)
      updateSelectInput (session, 'baddVarP2', selected=plotSpec$Bin[[plt]]$panel[[pnl]]$vary[lv])
      updateSelectInput (session, 'bvarColor', selected=plotSpec$Bin[[plt]]$panel[[pnl]]$col[lv])
      updateNumericInput (session, 'bsize', value=plotSpec$Bin[[plt]]$panel[[pnl]]$size[lv])
      updateRadioButtons (session, 'bsymbol', selected=ltyps[plotSpec$Bin[[plt]]$panel[[pnl]]$symbol[lv]])
    } else {
      vy <- plotSpec$Bin[[plt]]$panel[[pnl]]$vary
      vy <- c(vy, vy[length(vy)])
      plotSpec$Bin[[plt]]$panel[[pnl]]$vary <<- vy
      ## varx remains the same
      if (Trace) {print (c('vary set to ', vy))}
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
    if (Trace) {print ('processed blineV [13]')}
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
      print ('reset newbin 41')
      print (c('plt,pnl,lv,symbol:',plt,pnl,lv,input$bsymbol))
    }
  })
  obsbsymbol <- observe (exprbsymbol, quoted=TRUE)
  
  exprbsize <- quote ({
    plt <- isolate(input$plot)
    pnl <- isolate(input$bpanel)
    lv <- input$blineV
    plotSpec$Bin[[plt]]$panel[[pnl]]$size[lv] <<- input$bsize
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 40')}
  })
  obsbsize <- observe (exprbsize, quoted=TRUE)
  
  exprPlot <- quote ({
    plt <- input$plot
    updateNumericInput (session, 'panels', value=plotSpec$Plot[[plt]]$panels)
    updateNumericInput (session, 'cols', value=plotSpec$Plot[[plt]]$cols)
    updateNumericInput (session, 'panel', value=1)
    updateCheckboxInput (session, 'logY', value=plotSpec$Plot[[plt]]$panel[[1]]$logY)
    updateCheckboxInput (session, 'fixed', value=plotSpec$Plot[[plt]]$panel[[1]]$fixed)
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
    updateSelectInput (session, 'haddVarP', selected=plotSpec$Hist[[plt]]$panel[[1]]$var[1])
    updateSelectInput (session, 'hvarColor', selected=plotSpec$Hist[[plt]]$panel[[1]]$col[1])
    updateNumericInput (session, 'hlineW', value=plotSpec$Hist[[plt]]$panel[[1]]$lw[1])
    updateRadioButtons (session, 'hlineStyle', selected=ltyps[plotSpec$Hist[[plt]]$panel[[1]]$lt[1]])
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newdisplay 11')}
  })
  obsPlot <- observe (exprPlot, quoted=TRUE)
  
  exprPanel <- quote ({
    plt <- input$plot
    pnl <- input$panel
    updateCheckboxInput (session, 'logY', value=plotSpec$Plot[[plt]]$panel[[pnl]]$logY)
    updateCheckboxInput (session, 'fixed', value=plotSpec$Plot[[plt]]$panel[[pnl]]$fixed)
    updateNumericInput (session, 'lineV', value=1)
    updateSelectInput (session, 'addVarP', selected=plotSpec$Plot[[plt]]$panel[[pnl]]$var[1])
    updateSelectInput (session, 'varColor', selected=plotSpec$Plot[[plt]]$panel[[pnl]]$col[1])
    updateNumericInput (session, 'lineW', value=plotSpec$Plot[[plt]]$panel[[pnl]]$lw[1])
    updateRadioButtons (session, 'lineStyle', selected=ltyps[plotSpec$Plot[[plt]]$panel[[pnl]]$lt[1]])
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 12')}
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
      updateRadioButtons (session, 'lineStyle', selected=ltyps[plotSpec$Plot[[plt]]$panel[[pnl]]$lt[lv]])
    } else {
      v <- plotSpec$Plot[[plt]]$panel[[pnl]]$var
      v <- c(v, v[length(v)])
      plotSpec$Plot[[plt]]$panel[[pnl]]$var <<- v
      if (Trace) {print (c('var set to ', v))}
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
      updateSelectInput (session, 'addVarP', selected=v[length(v)])
      updateSelectInput (session, 'varColor', selected=cl[length(cl)])
      updateNumericInput (session, 'lineW', value=lw[length(lw)])
      updateNumericInput (session, 'lineStyle', value=lt[length(lt)])
    }
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 13')}
  })
  obsLineV <- observe (exprLineV, quoted=TRUE)
  
  
  exprHistVar <- quote ({
    
    plt <- isolate (input$plot)
    pnl <- isolate (input$hpanel)
    lv <- isolate (input$hlineV)
    
    if (input$haddVarP != 'omit') {
      if (input$haddVarP == 'select') {
      } else {
        plotSpec$Hist[[plt]]$panel[[pnl]]$var[lv] <<- input$haddVarP
        if (((ld <- length(data ())) < 2) || (!(input$haddVarP %in% (nms <- names (data ()))))) {
          if (exists ('specialData') && (input$haddVarP %in% names (specialData))) {
          } else {
            print ('need new data to include new variable - 1')
            if (Trace) {print (c('haddVarP is ', input$haddVarP))}
            if (Trace) {print (c('length of data is ', ld))}
            if (Trace) {print (c('names in data are', nms))}
            reac$newdata <- reac$newdata + 1
          }
        }
      }
    } else {
      v <- plotSpec$Hist[[plt]]$panel[[pnl]]$var
      v <- v[-lv]
      if (Trace) {print (sprintf ('new var list is %s', v))}
      plotSpec$Hist[[plt]]$panel[[pnl]]$var <<- v
      # nms <- names (data ())  ## just a data ref to get reset
      updateSelectInput (session, 'haddVarP', selected='select')  
    }
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 14')}
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
        if (((ld <- length(data ())) < 2) || (!(input$saddVarP1 %in% (nms <- names (data ()))))) {
          if (exists ('specialData') && (input$saddVarP1 %in% names (specialData))) {
          } else {
            print ('need new data to include new variable - 2')
            if (Trace) {print (c('saddVarP1 is ', input$saddVarP1))}
            if (Trace) {print (c('length of data is ', ld))}
            if (Trace) {print (c('names in data are', nms))}
            reac$newdata <- reac$newdata + 1
          }
        }
      }
    } 
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 14x')}
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
        if (((ld <- length(data ())) < 2) || (!(input$saddVarP2 %in% (nms <- names (data ()))))) {
          if (exists ('specialData') && (input$saddVarP2 %in% names (specialData))) {
          } else {
            print ('need new data to include new variable - 3')
            if (Trace) {print (c('saddVarP2 is ', input$saddVarP2))}
            if (Trace) {print (c('length of data is ', ld))}
            if (Trace) {print (c('names in data are', nms))}
            reac$newdata <- reac$newdata + 1
          }
        }
      }
    } else {
      v <- plotSpec$Scat[[plt]]$panel[[pnl]]$vary
      v <- v[-lv]
      if (Trace) {print (sprintf ('new var list is %s', v))}
      plotSpec$Scat[[plt]]$panel[[pnl]]$vary <<- v
      # nms <- names (data ())  ## just a data ref to get reset
      updateSelectInput (session, 'saddVarP2', selected='select')  
    } 
    isolate (reac$newscat <- reac$newscat + 1)
    if (Trace) {print ('reset newscat 14y')}
  })
  obsScatVar2 <- observe (exprScatVar2, priority=10, quoted=TRUE)
  
  exprPlotVar <- quote ({
    
    plt <- isolate (input$plot)
    pnl <- isolate (input$panel)
    lv <- isolate (input$lineV)
    
    if (input$addVarP != 'omit') {
      if (input$addVarP == 'select') {
      } else {
        plotSpec$Plot[[plt]]$panel[[pnl]]$var[lv] <<- input$addVarP
        if ((length(data ()) < 2) || (!(input$addVarP %in% names (data ())))) {
          if (exists ('specialData') && (input$addVarP %in% names (specialData))) {
          } else {
            print ('need new data to include new variable - 4')
            reac$newdata <- reac$newdata + 1
          }
        }
      }
    } else {
      v <- plotSpec$Plot[[plt]]$panel[[pnl]]$var
      v <- v[-lv]
      if (Trace) {print (sprintf ('new var list is %s', v))}
      plotSpec$Plot[[plt]]$panel[[pnl]]$var <<- v
      # nms <- names (data ())  ## just a data ref to get reset
      updateSelectInput (session, 'addVarP', selected='select')  
    }
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 14')}
  })
  obsPlotVar <- observe (exprPlotVar, quoted=TRUE)
  
  exprbPlotVarX <- quote ({
    
    plt <- isolate (input$plot)
    pnl <- isolate (input$bpanel)
    lv <- isolate (input$blineV)
    plotSpec$Bin[[plt]]$panel[[pnl]]$varx <<- input$baddVarP1
    if (((ld <- length(data ())) < 2) || (!(input$baddVarP1 %in% (nms <- names (data ()))))) {
      if (exists ('specialData') && (input$baddVarP1 %in% names (specialData))) {
      } else {
        print ('need new data to include new variable - 5')
        reac$newdata <- reac$newdata + 1
        if (Trace) {print (c('baddVarP1 is ', input$baddVarP1))}
        if (Trace) {print (c('length of data is ', ld))}
        if (Trace) {print (c('names in data are', nms))}
      }
    }
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 14')}
  })
  obsbPlotVarX <- observe (exprbPlotVarX, quoted=TRUE)
  
  exprbPlotVar <- quote ({
    
    plt <- isolate (input$plot)
    pnl <- isolate (input$bpanel)
    lv <- isolate (input$blineV)
    
    if (input$baddVarP2 != 'omit') {
      if (input$baddVarP2 == 'select') {
      } else {
        plotSpec$Bin[[plt]]$panel[[pnl]]$vary[lv] <<- input$baddVarP2
        if ((length(data ()) < 2) || (!(input$baddVarP2 %in% names (data ())))) {
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
      if (Trace) {print (sprintf ('new vary list is %s', v))}
      plotSpec$Bin[[plt]]$panel[[pnl]]$vary <<- v
      # nms <- names (data ())  ## just a data ref to get reset
      # updateSelectInput (session, 'saddVarP2', selected='select')  
    }
    isolate (reac$newbin <- reac$newbin + 1)
    if (Trace) {print ('reset newbin 14')}
  })
  obsbPlotVar <- observe (exprbPlotVar, quoted=TRUE)
  
  exprLineColor <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate (input$panel)
    lv <- isolate (input$lineV)
    plotSpec$Plot[[plt]]$panel[[pnl]]$col[lv] <<- input$varColor
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 15')}
  })
  obsLineColor <- observe (exprLineColor, quoted=TRUE)
  
  exprLineWidth <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate (input$panel)
    lv <- isolate (input$lineV)
    plotSpec$Plot[[plt]]$panel[[pnl]]$lw[lv] <<- input$lineW
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 16')}
  })
  obsLineWidth <- observe (exprLineWidth, quoted=TRUE)
  
  exprLineStyle <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate (input$panel)
    lv <- isolate (input$lineV)
    plotSpec$Plot[[plt]]$panel[[pnl]]$lt[lv] <<- which (input$lineStyle == ltyps)
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 17')}
  })
  obsLineStyle <- observe (exprLineStyle, quoted=TRUE)
  
  exprhLineColor <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate(input$hpanel)
    lv <- isolate (input$hlineV)
    plotSpec$Hist[[plt]]$panel[[pnl]]$col[lv] <<- input$hvarColor
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 15')}
  })
  obshLineColor <- observe (exprhLineColor, quoted=TRUE)
  
  exprhLineWidth <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate(input$hpanel)
    lv <- isolate (input$hlineV)
    plotSpec$Hist[[plt]]$panel[[pnl]]$lw[lv] <<- input$hlineW
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 16')}
  })
  obshLineWidth <- observe (exprhLineWidth, quoted=TRUE)
  
  exprhLineStyle <- quote ({
    plt <- isolate (input$plot)
    pnl <- isolate(input$hpanel)
    lv <- isolate (input$hlineV)
    if (Trace) {print (c('lineStyle is ', input$hlineStyle))}
    plotSpec$Hist[[plt]]$panel[[pnl]]$lt[lv] <<- which (input$hlineStyle == ltyps)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    if (Trace) {print ('reset newhistogram 17')}
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
    if (Trace) {print ('reset newdisplay 21')}
  })
  obsRvar <- observe (exprRvar, quoted=TRUE)
  
  
  observeEvent (input$specSave, saveConfig (input))
  observeEvent (input$specRead, 
                {loadConfig (input)
                  updateSelectInput (session, 'Project', selected=plotSpec$Project)
                  updateNumericInput (session, 'Flight', value=plotSpec$Flight)
                  updateRadioButtons (session, 'typeFlight', selected=plotSpec$TypeFlight)
                  updateNumericInput (session, 'plot', value=1)
                  updateNumericInput (session, 'panels', value=plotSpec$Plot[[1]]$panels)
                  updateNumericInput (session, 'cols', value=plotSpec$Plot[[1]]$columns)
                  updateNumericInput (session, 'panel', value=1)
                  updateCheckboxInput (session, 'logY', value=plotSpec$Plot[[1]]$panel[[1]]$logY)
                  updateCheckboxInput (session, 'fixed', value=plotSpec$Plot[[1]]$panel[[1]]$fixed)
                  updateNumericInput (session, 'panelMin', value=plotSpec$Plot[[1]]$panel[[1]]$ylim[1])
                  updateNumericInput (session, 'panelMax', value=plotSpec$Plot[[1]]$panel[[1]]$ylim[2])
                  updateNumericInput (session, 'lineV', value=1)
                  updateSelectInput (session, 'addVarP', selected=plotSpec$Plot[[1]]$panel[[1]]$var[1])
                  updateSelectInput (session, 'varColor', selected=plotSpec$Plot[[1]]$panel[[1]]$col[1])
                  updateNumericInput (session, 'lineW', value=plotSpec$Plot[[1]]$panel[[1]]$lw[1])
                  updateRadioButtons (session, 'lineStyle', selected=plotSpec$Plot[[1]]$panel[[1]]$lt[1])
                  
                  updateNumericInput (session, 'hpanels', value=plotSpec$Hist[[1]]$panels)
                  updateNumericInput (session, 'hcols', value=plotSpec$Hist[[1]]$columns)
                  updateNumericInput (session, 'hpanel', value=1)
                  updateCheckboxInput (session, 'hlogY', value=plotSpec$Hist[[1]]$panel[[1]]$logY)
                  updateCheckboxInput (session, 'hfixed', value=plotSpec$Hist[[1]]$panel[[1]]$fixed)
                  updateNumericInput (session, 'hpanelMin', value=plotSpec$Hist[[1]]$panel[[1]]$ylim[1])
                  updateNumericInput (session, 'hpanelMax', value=plotSpec$Hist[[1]]$panel[[1]]$ylim[2])
                  updateNumericInput (session, 'hlineV', value=1)
                  updateSelectInput (session, 'haddVarP', selected=plotSpec$Hist[[1]]$panel[[1]]$var[1])
                  updateSelectInput (session, 'hvarColor', selected=plotSpec$Hist[[1]]$panel[[1]]$col[1])
                  updateNumericInput (session, 'hlineW', value=plotSpec$Hist[[1]]$panel[[1]]$lw[1])
                  updateRadioButtons (session, 'hlineStyle', selected=plotSpec$Hist[[1]]$panel[[1]]$lt[1])
                  isolate (reac$newdisplay <- reac$newdisplay + 1)
                  isolate (reac$newhistogram <- reac$newhistogram + 1)
                } )
  #   observeEvent (input$savePDF,
  #                 savePDF (Data=data(), inp=input))
  #   observeEvent (input$savePNG,
  #                 savePNG (Data=data(), inp=input))
  observeEvent (input$saveRdata,
                saveRdata (Data=data(), inp=input))
  observeEvent (input$nextT, {
    dt <- difftime (times[2], times[1])
    times[1] <<- times[1] + dt
    times[2] <<- times[2] + dt
    updateSliderInput (session, 'times', value=times)
    updateTextInput (session, 'tstart', value=formatTime (times[1]))
    updateTextInput (session, 'tend',   value=formatTime (times[2]))
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newstats <- reac$newstats + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    isolate (reac$newbin <- reac$newbin + 1)
  } )
  observeEvent (input$prevT, {
    dt <- difftime (times[2], times[1])
    times[1] <<- times[1] - dt
    times[2] <<- times[2] - dt
    updateSliderInput (session, 'times', value=times)
    updateTextInput (session, 'tstart', value=formatTime (times[1]))
    updateTextInput (session, 'tend',   value=formatTime (times[2]))
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    isolate (reac$newhistogram <- reac$newhistogram + 1)
    isolate (reac$newstats <- reac$newstats + 1)
    isolate (reac$newscat <- reac$newscat + 1)
    isolate (reac$newbin <- reac$newbin + 1)
  } )
  observeEvent (input$createV, {
    TX <- input$formla
    m <- gregexpr('[[:alnum:]]+', TX)
    V <- regmatches(TX, m)[[1]]
    V <- V[grepl('[[:upper:]]', V)]
    ## if a requested variable is not present, get new data:
    nms <- names (data ())
    for (VV in V) {
      if (!(VV %in% nms)) {
        addedVariables <<- c(addedVariables, VV)
        isolate (reac$newdata <- reac$newdata + 1)
      }
    } 
    nv <- input$newvar
    assign (nv, with (data (), eval (parse (text=input$formla))))
    print (summary (eval(parse(text=input$newvar))))
    if (!exists ('specialData')) {
      specialData <<- data.frame ('Time'=data()$Time)
    }
    specialData[, nv] <<- eval(parse(text=nv))
    FI$Variables <<- c(FI$Variables, nv)
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
    updateSelectInput (session, 'specvar', choices=choices)
    updateSelectInput (session, 'speccovar', choices=choices)
    updateSelectInput (session, 'rvar', choices=choices,
                       selected=plotSpec$Restrictions$RVAR[rlv])
    ## force re-read to get this variable added to data:
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print (str(specialData))}
  })
  # observeEvent (input$prev, Repeat (-1))
  observeEvent (input$statVariables, {
    chooseVar (fname, inp=input)
    ## check if any requested variables not present in Data:
    if (any (!(sVarList %in% VarList))) {
      VarList <<- unique (c(VarList, sVarList))
      print (c(VarList, sVarList))
      isolate (reac$newdata <- reac$newdata + 1)
    }
    isolate (reac$newstats <- reac$newstats + 1)
  })
  observeEvent (input$ncplot, OpenInProgram (data(), warnOverwrite=FALSE))
  observeEvent (input$Xanadu, OpenInProgram (data(), 'Xanadu', warnOverwrite=FALSE))
  observeEvent (input$maneuvers, SeekManeuvers (data ()))
  observeEvent (input$manual, seeManual ())
  
  
  
  ################ REACTIVES ########################
  
  reac <- reactiveValues (newdata=0, newdisplay=0, newtrack=0, 
                          newstats=0, newhistogram=0, newscat=0, 
                          newbin=0, newvarp=0)
  
  flightType <- reactive ({              ## typeFlight
    ## reset typeFlight to rf
    updateRadioButtons (session, 'typeFlight', label=NULL, selected='rf')
    'rf'
  })
  
  data <- reactive({                     ## data
    if (Trace) {
      print (c('entered data, newdata is ', reac$newdata))
    }
    # Project <<- Project <- input$Project
    reac$newdata
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    ## these would be needed for translation to new cal coefficients
    ## VarList <- c(VarList, "RTH1", "RTH2", "RTF1")
    
    VarList <<- makeVarList ()  ## saved as global for possible inspection
    if (grepl ('HIPPO', plotSpec$Project)) {
      fname <<- sprintf ('%sHIPPO/%s%s%02d.nc', DataDirectory (), plotSpec$Project,
                         plotSpec$TypeFlight, plotSpec$Flight)
    } else {
      fname <<- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), plotSpec$Project,
                         plotSpec$Project, plotSpec$TypeFlight, plotSpec$Flight)
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
    # reac$newdisplay <- reac$newdisplay + 1
    if (file.exists(fname)) {
      D <- getNetCDF (fname, VarList)
      # times <<- c(D$Time[1], D$Time[nrow(D)])
      step <- 60
      minT <- D$Time[1]
      minT <- minT - as.integer (minT) %% step + step
      maxT <- D$Time[nrow(D)]
      maxT <- maxT - as.integer (maxT) %% step
      times <<- c(minT, maxT)
      updateSliderInput (session, 'times', value=c(minT,maxT), min=minT, max=maxT)
      if (exists ('specialData')) {
        SD <- specialData
        if ('Time' %in% names (SD)) {
          SD$Time <- NULL
        }
        D <- cbind (D, SD)
      }
      if (length (D) > 1) {
        fname.last <<- fname
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
      fn <- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), plotSpec$Project,
                     plotSpec$Project, 'tf', plotSpec$Flight)
      if (file.exists (fn)) {
        warning (sprintf ('switched to tf%02d because rf%02d does not exist',
                          plotSpec$Flight, plotSpec$Flight))
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
  
  plotMain <- function (input) {
    
    DataR <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Plot[[input$plot]]$restrict)
    plt <- isolate (input$plot)
    spec <- plotSpec$Plot[[plt]]
    nrws <- ceiling (spec$panels / spec$columns)
    nmx <- nrws * spec$columns
    layout(matrix(1:nmx, ncol = spec$columns), widths = 1, 
           heights = c(rep(5,spec$panels-1),6))
    op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
    for (pnl in 1:spec$panels) {
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
      if (spec$panel[[pnl]]$fixed) {yl <- spec$panel[[pnl]]$ylim}
      if (plotSpec$Plot[[input$plot]]$restrict) {
        if (is.null (yl)) {
          plotWAC (DataV[, c('Time', spec$panel[[pnl]]$var)], log=logY,
                   col=spec$panel[[pnl]]$col,
                   lwd=spec$panel[[pnl]]$lw,
                   lty=spec$panel[[pnl]]$lt)  
        } else {
          plotWAC (DataV[, c('Time', spec$panel[[pnl]]$var)], ylim=yl, log=logY,
                   col=spec$panel[[pnl]]$col,
                   lwd=spec$panel[[pnl]]$lw,
                   lty=spec$panel[[pnl]]$lt)  
        }
      } else {
        if (is.null (yl)) {
          plotWAC (DataR[, c('Time', spec$panel[[pnl]]$var)], log=logY,
                   col=spec$panel[[pnl]]$col,
                   lwd=spec$panel[[pnl]]$lw,
                   lty=spec$panel[[pnl]]$lt) 
        } else {
          plotWAC (DataR[, c('Time', spec$panel[[pnl]]$var)], ylim=yl, log=logY,
                   col=spec$panel[[pnl]]$col,
                   lwd=spec$panel[[pnl]]$lw,
                   lty=spec$panel[[pnl]]$lt) 
        }
      }
    }
  }
  
  output$display <- renderPlot ({  ## display
    reac$newdisplay
    Project <- plotSpec$Project
    if (Trace) {
      print ('entered display')
      # Sys.sleep(5)
    }
    if (Trace) {
      print (c('newdisplay is', reac$newdisplay))
      print (sprintf ('global times are %s %s',
                      formatTime (times[1]), formatTime (times[2])))
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newdisplay <- reac$newdisplay + 1 #<- TRUE
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('exiting display for new data')}
      return()
    }
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Plot[[input$plot]]$restrict)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                            plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                            strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                            strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                     format="%H:%M:%S", tz='UTC'))
    )
    FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    AddFooter <<- function() {
      isolate (
        mtext(paste(FigFooter,'generated by Ranadu plot ', input$plot,
                    FigDatestr),1,outer=T,cex=0.75)
      )
    }
    plotMain (input)    ## isolated in function to be able to save via PDF/PNG
    
    if (input$footer) {AddFooter ()}
    if (Trace) {
      print ('finished plot generation')
    }
  }, width=920, height=640)
  
  plotScat <- function (input) {
    
    DataR <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
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
        plotWAC (DataX[, c(spec$panel[[pnl]]$varx, spec$panel[[pnl]]$vary)], 
                 log=logV, col=spec$panel[[pnl]]$col, type='p', 
                 xlab=spec$panel[[pnl]]$varx, 
                 pch=spec$panel[[pnl]]$symbol, cex=spec$panel[[pnl]]$size,
                 legend.position='top')
        if (Trace) {print (c('symbol used ', spec$panel[[pnl]]$symbol))}
      } else {
        plotWAC (DataX[, c(spec$panel[[pnl]]$varx, spec$panel[[pnl]]$vary)], 
                 log=logV, col=spec$panel[[pnl]]$col, type='p', xlim=xl,
                 ylim=yl, xlab=spec$panel[[pnl]]$varx, 
                 pch=spec$panel[[pnl]]$symbol, cex=spec$panel[[pnl]]$size,
                 legend.position='top')
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
    
    DataR <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
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
      if (Trace) {print (c('binPlot xl=', xl))}
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
      print ('entered scatterplot')
      # Sys.sleep(5)
    }
    if (Trace) {
      print (c('newscat is', reac$newscat))
      print (sprintf ('global times are %s %s',
                      formatTime (times[1]), formatTime (times[2])))
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newdisplay <- reac$newdisplay + 1 #<- TRUE
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('exiting scatterplot for new data')}
      return()
    }
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Bin[[input$plot]]$restrict)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                            plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                            strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                            strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                     format="%H:%M:%S", tz='UTC'))
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
      print ('finished scatterplot generation')
    }
  }, width=920, height=640)
  
  output$binplot <- renderPlot ({  ## binplot
    reac$newbin
    Project <- plotSpec$Project
    if (Trace) {
      print ('entered binplot')
      # Sys.sleep(5)
    }
    if (Trace) {
      print (c('newbin is', reac$newbin))
      print (sprintf ('global times are %s %s',
                      formatTime (times[1]), formatTime (times[2])))
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newbin <- reac$newbin + 1 #<- TRUE
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('exiting binplot for new data')}
      return()
    }
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Scat[[input$plot]]$restrict)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                            plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                            strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                            strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                     format="%H:%M:%S", tz='UTC'))
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
      print ('finished binplot generation')
    }
  }, width=920, height=640)
  
  output$sdplot <- renderPlot ({  ## CDP
    Project <- plotSpec$Project
    Flight <- plotSpec$Flight
    tf <- plotSpec$TypeFlight
    input$times    ## make sensitive to time changes
    op <- par (mar=c(5,6,1,1)+0.1,oma=c(1.1,0,0,0))
    if (!is.na (fname) && file.exists (fname)) {
      if (is.null (netCDFfile) || is.na (netCDFfile)) {
        netCDFfile <<- nc_open (fname)
      }
      namesCDF <- names (netCDFfile$var)
      Time <- ncvar_get (netCDFfile, "Time")
      TASX <<- TASX <- ncvar_get (netCDFfile, "TASX")
      time_units <- ncatt_get (netCDFfile, "Time", "units")
      tref <<- sub ('seconds since ', '', time_units$value)
      Time <<- Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
      idx1 <- getIndex (Time, as.integer (gsub(':', '', formatTime(times[1]))))
      if (idx1 < 1) {idx1 <- 1}
      idx2 <- getIndex (Time, as.integer (gsub(':', '', formatTime(times[2]))))
      if (idx1 < 1) {idx2 <- length(Time)}
      if ('UHSAS' %in% input$probe) {
        nm3 <- namesCDF[grepl("CUHSAS_", namesCDF)]
        if (length (nm3) > 0) {
          if (is.null (CUHSAS) || (is.na (CUHSAS))) {
            CUHSAS <- ncvar_get (netCDFfile, nm3)
            CellSizes <<- ncatt_get (netCDFfile, nm3, "CellSizes")
            CellLimitsU <- CellSizes$value
            attr (CUHSAS, 'CellLimits') <- CellLimitsU
            CUHSAS <<- CUHSAS
          } else {
            CellLimitsU <- attr (CUHSAS, 'CellLimits')
          }
          if (idx2-idx1 <= 1) {
            UHSAS <- CUHSAS[, idx1]
          } else {
            UHSAS <- rowMeans(CUHSAS[, idx1:(idx2-1)], na.rm=TRUE)
          }
          UHSAStot <- 0
          nbU <- length (UHSAS)
          for (m in 2:nbU) {
            UHSAStot <- UHSAStot + UHSAS[m]
            UHSAS[m] <- UHSAS[m] / (CellLimitsU[m] - CellLimitsU[m-1])
          }
          UHSAS[UHSAS <= 0] <- 1e-6
        }
      }
      if ('PCASP' %in% input$probe) {
        nm4 <- namesCDF[grepl("CPCASP_", namesCDF)]
        if (length (nm4) > 0) {
          if (is.null (CPCASP) || (is.na (CPCASP))) {
            CPCASP <- ncvar_get (netCDFfile, nm4)
            CellSizes <<- ncatt_get (netCDFfile, nm4, "CellSizes")
            CellLimitsP <- CellSizes$value
            attr (CPCASP, 'CellLimits') <- CellLimitsP
            CPCASP <<- CPCASP
          } else {
            CellLimitsP <- attr (CPCASP, 'CellLimits')
          }
          if (idx2-idx1 <= 1) {
            PCASP <- CPCASP[, idx1]
          } else {
            PCASP <- rowMeans(CPCASP[, idx1:(idx2-1)], na.rm=TRUE)
          }
          PCASPtot <- 0
          nbP <- length (PCASP)
          for (m in 2:nbP) {
            PCASPtot <- PCASPtot + PCASP[m]
            PCASP[m] <- PCASP[m] / (CellLimitsP[m] - CellLimitsP[m-1])
          }
          PCASP[PCASP <= 0] <- 1e-6
        }
      }
      if ('CDP' %in% input$probe) {
        nm1 <- namesCDF[grepl("CCDP_", namesCDF)]
        if (length (nm1) > 0) {
          if (is.null (CCDP) || (is.na (CCDP))) {
            CCDP <- ncvar_get (netCDFfile, nm1)
            CellSizes <<- ncatt_get (netCDFfile, nm1, "CellSizes")
            CellLimitsD <- CellSizes$value
            attr (CCDP, 'CellLimits') <- CellLimitsD
            CCDP <<- CCDP
          } else {
            CellLimitsD <- attr (CCDP, 'CellLimits')
          }
          if (idx2-idx1 <= 1) {
            CDP <- CCDP[, idx1]
          } else {
            CDP <- rowMeans(CCDP[, idx1:(idx2-1)], na.rm=TRUE)
          }
          CDPtot <- 0
          nbC <- length (CDP)
          for (m in 2:nbC) {
            CDPtot <- CDPtot + CDP[m]
            CDP[m] <- CDP[m] / (CellLimitsD[m] - CellLimitsD[m-1])
          }
          CDP[CDP <= 0] <- 1e-6
        }
      }
      if ('FSSP' %in% input$probe) {
        nm2 <- namesCDF[grepl("CS100_", namesCDF)]
        if (length (nm2) > 0) {
          if (is.null (CFSSP) || (is.na (CFSSP))) {
            CFSSP <- ncvar_get (netCDFfile, nm2)
            CellSizes <<- ncatt_get (netCDFfile, nm2, "CellSizes")
            CellLimitsF <- CellSizes$value
            attr (CFSSP, 'CellLimits') <- CellLimitsF
            CFSSP <<- CFSSP
          } else {
            CellLimitsF <- attr (CFSSP, 'CellLimits')
          }
          if (idx2-idx1 <= 1) {
            FSSP <- CFSSP[, idx1]
          } else {
            FSSP <- rowMeans(CFSSP[, idx1:(idx2-1)], na.rm=TRUE)
          }
          FSSPtot <- 0
          nbF <- length (FSSP)
          for (m in 2:nbF) {
            FSSPtot <- FSSPtot + FSSP[m]
            FSSP[m] <- FSSP[m] / (CellLimitsF[m] - CellLimitsF[m-1])
          }
          FSSP[FSSP <= 0] <- 1e-6
        }
      }
      if ('2DC' %in% input$probe) {
        nm5 <- namesCDF[grepl("^C1DC_", namesCDF)]
        if (length (nm5) > 0) {
          if (is.null (C1DC) || (is.na (C1DC))) {
            C1DC <- ncvar_get (netCDFfile, nm5)
            CellSizes <<- ncatt_get (netCDFfile, nm5, "CellSizes")
            CellLimits2 <- CellSizes$value
            attr (C1DC, 'CellLimits') <- CellLimits2
            C1DC <<- C1DC
          } else {
            CellLimits2 <- attr (C1DC, 'CellLimits')
          }
          if (idx2-idx1 <= 1) {
            S1DC <- C1DC[, idx1]
          } else {
            S1DC <- rowMeans(C1DC[, idx1:(idx2-1)], na.rm=TRUE)
          }
          ## convert to concentration usits of cm^3
          S1DC <- S1DC * 1.e-3
          S1DCtot <- 0
          nb2 <- length (S1DC)
          for (m in 2:nb2) {
            S1DCtot <- S1DCtot + S1DC[m]
            S1DC[m] <- S1DC[m] / (CellLimits2[m] - CellLimits2[m-1])
          }
          S1DC[S1DC <= 0] <- 1e-9
        }
      }
      
      ## now have size distributions; construct plots
      dmin <- 1e10
      dmax <- 0
      cmin=1e10
      cmax=0
      if ('CDP' %in% input$probe && (length(nm1) > 0)) {
        dmin <- min (c(dmin, CellLimitsD[2]), na.rm=TRUE)
        dmax <- max (c(dmax, CellLimitsD[nbC]), na.rm=TRUE)
        cmin <- min (c(cmin, CDP), na.rm=TRUE)
        cmax <- max (c(cmax, CDP), na.rm=TRUE)
      }
      if ('FSSP' %in% input$probe && (length(nm2) > 0)) {
        dmin <- min (c(dmin, CellLimitsF[2]), na.rm=TRUE)
        dmax <- max (c(dmax, CellLimitsF[nbF]), na.rm=TRUE)
        cmin <- min (c(cmin, FSSP), na.rm=TRUE)
        cmax <- max (c(cmax, FSSP), na.rm=TRUE)
      }
      if ('UHSAS' %in% input$probe && (length(nm3) > 0)) {
        dmin <- min (c(dmin, CellLimitsU[2]), na.rm=TRUE)
        dmax <- max (c(dmax, CellLimitsU[nbU]), na.rm=TRUE)
        cmin <- min (c(cmin, UHSAS), na.rm=TRUE)
        cmax <- max (c(cmax, UHSAS), na.rm=TRUE)
      }
      if ('PCASP' %in% input$probe && (length(nm4) > 0)) {
        dmin <- min (c(dmin, CellLimitsP[2]), na.rm=TRUE)
        dmax <- max (c(dmax, CellLimitsP[nbP]), na.rm=TRUE)
        cmin <- min (c(cmin, PCASP), na.rm=TRUE)
        cmax <- max (c(cmax, PCASP), na.rm=TRUE)
      }
      if ('2DC' %in% input$probe && (length(nm5) > 0)) {
        dmin <- min (c(dmin, CellLimits2[2]), na.rm=TRUE)
        dmax <- max (c(dmax, CellLimits2[nb2]), na.rm=TRUE)
        cmin <- min (c(cmin, S1DC), na.rm=TRUE)
        cmax <- max (c(cmax, S1DC), na.rm=TRUE)
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
      ttl <- sprintf ("Time=%s--%s ", strftime (Time[idx1], format="%H:%M:%S", tz='UTC'), 
                      strftime (Time[idx2], format="%H:%M:%S", tz='UTC'))
      legend.names <- vector()
      legend.colors <- vector()
      if ('UHSAS' %in% input$probe && (length (nm3) > 0)) {
        points (CellLimitsU[2:nbU], UHSAS[2:nbU], type='s', 
                col='darkgreen', lwd=2)
        legend.names <- c(legend.names, 'UHSAS')
        legend.colors <- c(legend.colors, 'darkgreen')
        ttl <- paste (ttl, sprintf("CONCU=%.1f", UHSAStot), sep=' ')
      }
      if ('PCASP' %in% input$probe && (length (nm4) > 0)) {
        points (CellLimitsP[2:nbP], PCASP[2:nbP], type='s', 
                col='darkorange', lwd=2)
        legend.names <- c(legend.names, 'PCASP')
        legend.colors <- c(legend.colors, 'darkorange')
        ttl <- paste (ttl, sprintf("CONCP=%.1f", PCASPtot), sep=' ')
      }
      if ('CDP' %in% input$probe && (length (nm1) > 0)) {
        points (CellLimitsD[2:nbC], CDP[2:nbC], type='s', 
              col='blue', lwd=2)
        legend.names <- c(legend.names, 'CDP')
        legend.colors <- c(legend.colors, 'blue')
        ttl <- paste (ttl, sprintf("CONCD=%.1f", CDPtot), sep=' ')
      }
      if ('FSSP' %in% input$probe && (length (nm2) > 0)) {
        points (CellLimitsF[2:nbF], FSSP[2:nbF], type='s', 
                col='violet', lwd=2)
        legend.names <- c(legend.names, 'FSSP')
        legend.colors <- c(legend.colors, 'violet')
        ttl <- paste (ttl, sprintf("CONCF=%.1f", FSSPtot), sep=' ')
      }
      if ('2DC' %in% input$probe && (length (nm5) > 0)) {
        points (CellLimits2[7:nb2], S1DC[7:nb2], type='s', 
                col='red', lwd=2)
        legend.names <- c(legend.names, '2DC')
        legend.colors <- c(legend.colors, 'red')
        ttl <- paste (ttl, sprintf("CONC1DC=%.4f", S1DCtot), sep=' ')
      }

      if (length (input$probe) > 0) {
        title (ttl)
        legend ("topright", legend=legend.names, col=legend.colors,
                lwd=c(2,1), cex=0.75)
      }
    }  ## end of CDP section
  }, width=800, height=640)
  
  output$varplot <- renderImage ({  ## varplot
    reac$newvarp
    Project <- plotSpec$Project
    # spec <- plotSpec$Var[[input$plot]]
    if (Trace) {
      print ('entered varplot')
      # Sys.sleep(5)
    }
    if (Trace) {
      print (c('newvarp is', reac$newvarp))
      print (sprintf ('global times are %s %s',
                      formatTime (times[1]), formatTime (times[2])))
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newvarp <- reac$newvarp + 1 #<- TRUE
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('exiting varplot for new data')}
      return()
    }
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Scat[[input$plot]]$restrict)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                            plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                            strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                            strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                     format="%H:%M:%S", tz='UTC'))
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
    Z <- makeNetCDF (Data, fnew)
    ## the following function writes some configuration for interaction with
    ## 'Xanadu' where the spectral analysis is performed.
    setXanadu <- function (fnew, start, end, var, wlow, whigh) {
      ## edit the .def files for the Xanadu call
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
        if (substr (line, 1, 4) == "WLOW") {
          line <- gsub (" .*", sprintf (" %f", wlow), line)
        }
        if (substr (line, 1, 5) == "WHIGH") {
          line <- gsub (" .*", sprintf (" %f", whigh), line)
        }
        newlines[length (newlines) + 1] <- line
      }
      writeLines (newlines, "otto.def")
      return()
    }
    wlow <- 0.0001; whigh <- 0.1
    # spec$var <- 'WIC'
    ts <- formatTime(times[1])
    te <- formatTime(times[2])
    ts <- as.integer(gsub (':', '', ts))
    te <- as.integer (gsub (':', '', te))
    setXanadu (fnew, ts, te, 'WIC', wlow, whigh)
    unlink ("MEMPlot.png")
    XanaduOut <- system ("Xanadu otto", intern=TRUE)
    while (!file.exists ("MEMPlot.png")) {Sys.sleep (1)}
    file.rename ("MEMPlot.png", "SpecialGraphics/PSD1.png")
    
    #     if (is.null(input$picture))
    #       return(NULL)
    
    # if (input$picture == "face") {
    return(list(
      src = "SpecialGraphics/PSD1.png",
      contentType = "image/png",
      alt = "PSD"
    ))
    
  }, deleteFile = FALSE)
  
  
  
  output$savePDF <- downloadHandler(
    filename = function() {
      paste('Figures/Ranadu.', Sys.time(), '.pdf', sep='')
    },
    content = function(file) {
      pdf (file)
      print (plotMain (input))
      dev.off ()
    },
    contentType='image/pdf'
  )
  
  output$savePNG <- downloadHandler(
    filename <- function() {
      paste('Figures/Ranadu.', Sys.time(), '.png', sep='')
    },
    content <- function(file) {
      png (file)
      print (plotMain (input))
      dev.off ()
      print ('in PNG download handler')
    },
    contentType='image/png'
  )
  
  
  output$track <- renderPlot ({  ## track
    reac$newtrack
    Project <- plotSpec$Project
    if (Trace) {
      print (c('track entry, reac$newtrack is:', reac$newtrack))
    }
    Data <- data ()
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
    #       for (nm in namesV) {
    #         
    #       }
    DataR <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, input$limits2)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    #       
    #       for (n in namesV) {
    #         Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
    #       }
    #       # Data <- Data[(Data$Time >= input$times[1]) & (Data$Time < input$times[2]), ]
    #       Data <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
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
    FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                      plotSpec$Flight, strftime(DataR$Time[i], format="%Y-%m-%d", tz='UTC'),
                      strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                      strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                               format="%H:%M:%S", tz='UTC'))
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
      # Data <- Data[(Data$Time >= input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
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
      # Data <- Data[(Data$Time >= input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
      if (nrow (Data) <= 0) {
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
        reac$newdisplay <- reac$newdisplay + 1
        reac#newdata <- reac$newdata + 1
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
                              +ggtitle(sprintf("Flight %s", Flight))))
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
      
      if (input$footer6) {AddFooter ()}
      
      # }
      #       si <- input$plot
      #       updateSelectInput (session, 'Rplot', selected=st[si])
      
    }
  }, width=780, height=640) 
  
  
  output$stats <- renderDataTable ({    ## stats
    if (Trace) {print ('entered stats')}
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
    Ds <- Ds[(Ds$Time >= times[1]) & (Ds$Time < times[2]), ]
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
  
  output$statistics <- renderDataTable ({    ## statistics
    if (Trace) {print ('entered statistics')}
    input$times
    reac$stats
    ## check if any requested variables not present in Data:
    if (any (!(sVarList %in% VarList))) {
      VarList <<- unique (c(VarList, sVarList))
      isolate (reac$newdata <- reac$newdata + 1)
    }
    Ds <- limitData (data(), input, input$limits2a)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    #     plotV <- vector ()
    #     for (i in 1:plotSpec$Plot[[input$plot]]$panels) {
    #       plotV <- c(plotV, plotSpec$Plot[[input$plot]]$panel[[i]]$var)
    #     }
    #     plotV <- unique (plotV)
    Ds <- Ds[, c('Time', sVarList)]
    Ds <- Ds[(Ds$Time >= times[1]) & (Ds$Time < times[2]), ]
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
  
  output$hist <- renderPlot ({  ## hist
    input$panels
    input$times
    layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(8,8,8))
    op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))
    if (Trace) {print ('entered hist')}
    Ds <- limitData (data(), input)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    plotV <- vector ()
    for (i in 1:plotSpec$Plot[[input$plot]]$panels) {
      plotV <- c(plotV, plotSpec$Plot[[input$plot]]$panel[[i]]$var)
    }
    plotV <- unique (plotV)
    Ds <- Ds[, c('Time', plotV)]
    Ds <- Ds[(Ds$Time >= times[1]) & (Ds$Time < times[2]), ]
    kount <- 0
    for (nm in names (Ds)) {
      if (nm == 'Time') {next}
      kount <- kount + 1
      if (kount > 6) {break}
      hist (Ds[ ,nm], freq=FALSE, breaks=50, xlab=nm, 
            ylab='probability density', main=NA)
    }
  }, width=780, height=640)
  
  plotHist <- function (inp) {  ## plotHist
    DataR <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, inp, inp$limits3)
    plt <- inp$plot
    if (Trace) {
      print (sprintf ('entered plotHist, plt=%d', plt))
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
      if (inp$limits3) {
        DataX <- DataV[, vr]
      } else {
        DataX <- DataR[, vr]
      }
      g <- ggplot (data=DataX)
      for (i in 1:length(vr)) {
        v <- sprintf ('var1[%d]', i)
        b <- sprintf ("aes (x=%s, colour='%s', size='%s', fill='%s', lty='%s')", 
                      vr[i], vr[i], vr[i], vr[i], vr[i])
        g <- g + geom_histogram (eval(parse(text=b)),
                                 bins=50, na.rm=TRUE)          
        
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
      if (inp$cdf) {
        a <- ggplot_build(g)
        yrange <- a$panel$ranges[[1]]$y.range
        xrange <- a$panel$ranges[[1]]$x.range
        for (j in 1:length (vr)) {
          yc <- cumsum (a$data[[j]]$density) * (a$data[[j]]$x[3] - a$data[[j]]$x[2])
          yc <- yc * yrange[2]
          dt <- data.frame (x=a$data[[1]]$x[-1], y=yc[-1])
          dt[nrow(dt), ] <- NA
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
    input$times
    reac$newhistogram
    Project <- plotSpec$Project
    if (Trace) {print ('entered histogram')}
    if (Trace) {
      print (c('newhistogram is', reac$newhistogram))
      print (sprintf ('global times are %s %s',
                      formatTime (times[1]), formatTime (times[2])))
    }
    Data <- data()
    if (nrow (Data) <= 1) {
      plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
      reac$newhistogram <- reac$newhistogram + 1 
      reac$newdata <- reac$newdata + 1
      if (Trace) {print ('exiting histogram for new data')}
      return()
    }
    namesV <- names(Data)  
    namesV <- namesV[namesV != "Time"]
    DataR <- Data[(Data$Time >= times[1]) & (Data$Time < times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, input$limits3)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    i <- getIndex (DataR$Time, SE[1])
    isolate (
      FigFooter <<- sprintf("%s %s%02d %s %s-%s UTC,", Project, plotSpec$TypeFlight,
                            plotSpec$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                            strftime(DataR$Time[i], format="%H:%M:%S", tz='UTC'),
                            strftime(DataR$Time[getIndex(DataR$Time,SE[2])],
                                     format="%H:%M:%S", tz='UTC'))
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
    if (Trace) {print ('finished hist generation')}
  }, width=780, height=640)
  
  output$barWvsZ <- renderPlot ({
    if (Trace) {print ('entered barXvsZ')}
    input$times
    input$panels
    layout (matrix(1:6, ncol=3), widths=c(5,5,5), heights=c(8,8))
    op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))
    Ds <- limitData (data(), input)
    plotV <- vector ()
    for (i in 1:plotSpec$Plot[[input$plot]]$panels) {
      plotV <- c(plotV, plotSpec$Plot[[input$plot]]$panel[[i]]$var)
    }
    plotV <- unique (plotV)
    Ds <- Ds[, c('Time', plotV)]
    Ds <- Ds[(Ds$Time >= times[1]) & (Ds$Time < times[2]), ]
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
  }, width=780, height=640)
  
  output$listing <- renderDataTable ({
    if (Trace) {print ('entered listing')}
    input$times
    input$panels
    Ds <- limitData (data(), input)
    plotV <- vector ()
    for (i in 1:plotSpec$Plot[[input$plot]]$panels) {
      plotV <- c(plotV, plotSpec$Plot[[input$plot]]$panel[[i]]$var)
    }
    plotV <- unique (plotV)
    Ds <- Ds[, c('Time', plotV)]
    Ds <- Ds[(Ds$Time >= times[1]) & (Ds$Time < times[2]), ]
    Ds$Time <- formatTime(Ds$Time)
    Ds
  }, options=list(paging=TRUE, searching=TRUE))
  
  outputOptions (output, 'display', priority=-10)
  outputOptions (output, 'stats', priority=-10)
  outputOptions (output, 'listing', priority=-10)
  outputOptions (output, 'hist', priority=-10)
  outputOptions (output, 'barWvsZ', priority=-10)
  outputOptions (output, 'statistics', -10)
  outputOptions (output, 'histogram', -10)
})


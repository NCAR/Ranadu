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
    if (Trace) {print ('reset newdisplay 1')}
  })
  obsProject <- observe (exprProject, quoted=TRUE)
  
  exprFlight <- quote ({
    plotSpec$Flight <<- input$Flight
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print ('reset newdisplay 2')}
  })
  obsFlight <- observe (exprFlight, quoted=TRUE)
  
  exprTypeFlight <- quote ({
    plotSpec$TypeFlight <<- input$typeFlight
    isolate (reac$newdata <- reac$newdata + 1)
    if (Trace) {print ('reset newdisplay 3')}
  })
  obsTypeFlight <- observe (exprTypeFlight, quoted=TRUE)
  
  exprTime <- quote ({
    times <<- input$times
    updateTextInput (session, 'tstart', value=formatTime(times[1]))
    updateTextInput (session, 'tend', value=formatTime(times[2]))
    isolate (reac$newdisplay <- reac$newdisplay + 1)
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
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 25')}
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
    isolate (reac$newdisplay <- reac$newdisplay + 1)
    if (Trace) {print ('reset newdisplay 26')}
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
        if ((length(data ()) < 2) || (!(input$haddVarP %in% names (data ())))) {
          print ('need new data to include new variable')
          reac$newdata <- reac$newdata + 1
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
  
  exprPlotVar <- quote ({
    
    plt <- isolate (input$plot)
    pnl <- isolate (input$panel)
    lv <- isolate (input$lineV)
    
    if (input$addVarP != 'omit') {
      if (input$addVarP == 'select') {
      } else {
        plotSpec$Plot[[plt]]$panel[[pnl]]$var[lv] <<- input$addVarP
        if ((length(data ()) < 2) || (!(input$addVarP %in% names (data ())))) {
          print ('need new data to include new variable')
          reac$newdata <- reac$newdata + 1
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
  
  #   observe ({  ## change plot number or number of panels
  #     input$specRead
  #     updateNumericInput (session, 'panels', value=plotSpec$Plot[[input$plot]]$panels)
  #   }, priority=100)
  #   observe ({
  #     plotSpec$Plot[[input$plot]]$panels <<- input$panels 
  #     plotSpec$Plot[[input$plot]]$columns <<- input$cols
  #   })
  #   observe ({  ## addVarP
  #     if (Trace) {print (sprintf ('entered addVarP observer, var=%s', input$addVarP))}
  #     #     if (is.null(input$addVarP) || length (input$addVarP) < 2) {return ()}
  #     #     if (is.null(input$varColor) || length (input$varColor) < 2) {return ()}
  #     npt <- isolate (input$plot)
  #     npl <- isolate (input$panel)
  #     if (Trace) {print (sprintf ('lineV is %d', input$lineV))}
  #     if (input$lineV > length (plotSpec$Plot[[npt]]$panel[[npl]]$var)) {
  #       v <- plotSpec$Plot[[npt]]$panel[[npl]]$var
  #       v <- c(v, v[length(v)])
  #       plotSpec$Plot[[npt]]$panel[[npl]]$var <<- v
  #       if (Trace) {print (c('var set to ', v))}
  #       lbl <- plotSpec$Plot[[npt]]$panel[[npl]]$lab
  #       lbl <- c(lbl, lbl[length(lbl)])
  #       plotSpec$Plot[[npt]]$panel[[npl]]$lab <<- lbl
  #       cl <- plotSpec$Plot[[npt]]$panel[[npl]]$col
  #       cl <- c(cl, cl[length(cl)])
  #       plotSpec$Plot[[npt]]$panel[[npl]]$col <<- cl
  #       lw <- plotSpec$Plot[[npt]]$panel[[npl]]$lw
  #       lw <- c(lw, lw[length(lw)])
  #       plotSpec$Plot[[npt]]$panel[[npl]]$lw <<- lw
  #       lt <- plotSpec$Plot[[npt]]$panel[[npl]]$lt
  #       lt <- c(lt, lt[length(lt)])
  #       plotSpec$Plot[[npt]]$panel[[npl]]$lt <<- lt
  #       updateSelectInput (session, 'addVarP', selected=v[length(v)])
  #       updateSelectInput (session, 'varColor', selected=cl[length(cl)])
  #       updateNumericInput (session, 'lineW', value=lw[length(lw)])
  #       updateNumericInput (session, 'lineStyle', value=lt[length(lt)])
  #     }
  #     if (input$addVarP != 'omit') {
  #       if (input$addVarP == 'select') {
  #         reac$newdisplay <- TRUE
  #       } else {
  #         plotSpec$Plot[[input$plot]]$panel[[input$panel]]$var[input$lineV] <<- input$addVarP
  #         reac$newdisplay <- TRUE
  #         reac$newspec <- TRUE
  #         if ((length(data ()) < 2) || (!(input$addVarP %in% names (data ())))) {
  #           print ('need new data')
  #           reac$newdata <- TRUE
  #         }
  #       }
  #     } else {
  #       v <- plotSpec$Plot[[input$plot]]$panel[[input$panel]]$var
  #       v <- v[-input$lineV]
  #       if (Trace) {print (sprintf ('new var list is %s', v))}
  #       plotSpec$Plot[[input$plot]]$panel[[input$panel]]$var <<- v
  #       reac$newdisplay <- TRUE
  #       reac$newspec <- TRUE
  #       nms <- names (data ())  ## just a data ref to get reset
  #       updateSelectInput (session, 'addVarP', selected='select')  
  #     }
  #   })
  #   
  #   observe ({
  #     l <- input$lineV
  #     if (l <= length (plotSpec$Plot[[input$plot]]$panel[[input$panel]]$var)) {
  #       v <- plotSpec$Plot[[input$plot]]$panel[[input$panel]]$var[l]
  #       updateSelectInput (session, 'addVarP', selected=v)
  #       updateSelectInput (session, 'varColor', selected=
  #                            plotSpec$Plot[[input$plot]]$panel[[input$panel]]$col[l])
  #       updateNumericInput (session, 'lineW', value=
  #                             plotSpec$Plot[[input$plot]]$panel[[input$panel]]$lw[l])
  #       updateRadioButtons (session, 'lineStyle', selected=
  #                             plotSpec$Plot[[input$plot]]$panel[[input$panel]]$lt[l])
  #       
  #     }
  #   })
  #   
  #   observe ({
  #     l <- input$lineV
  #     plotSpec$Plot[[input$plot]]$panel[[input$panel]]$col[l] <<- input$varColor
  #     plotSpec$Plot[[input$plot]]$panel[[input$panel]]$lw[l] <<- input$lineW
  #     plotSpec$Plot[[input$plot]]$panel[[input$panel]]$lt[l] <<- input$lineStyle
  #     reac$newdisplay <- TRUE
  #     reac$newspec <- TRUE
  #   })
  #   
  #   observe ({
  #     plotSpec$Plot[[input$plot]]$panel[[input$panel]]$logY <<- input$logY
  #     reac$newdisplay <- TRUE
  #     reac$newspec <- TRUE
  #   })
  #   
  #   observe ({ 
  #     input$specRead
  #     reac$newdisplay <- TRUE
  #     reac$newspec <- TRUE
  #   })
  #   
  #   observe ({
  #     if (input$rvNumber > nrow (Restrictions)) {
  #       newRow <- data.frame (RVAR=isolate (input$rvar), 
  #                             apply=isolate (input$apply),
  #                             min=isolate (input$rmin),
  #                             max=isolate (input$rmax))
  #       Restrictions <<- rbind (Restrictions, newRow)
  #     } else {
  #       updateSelectInput(session, 'rvar',  
  #                         selected=Restrictions$RVAR[input$rvNumber])
  #       updateCheckboxInput (session, 'apply',
  #                            value=Restrictions$apply[input$rvNumber])
  #       updateNumericInput(session, 'rmin', label=NULL, 
  #                          value=Restrictions$min[input$rvNumber])
  #       updateNumericInput(session, 'rmax', label=NULL, 
  #                          value=Restrictions$max[input$rvNumber])
  #     }
  #     reac$newdisplay <- TRUE
  #     reac$newspec <- TRUE
  #   })
  #   
  #   observe ({
  #     Restrictions$RVAR[input$rvNumber] <<- input$rvar
  #     reac$newdisplay <- TRUE
  #     reac$newspec <- TRUE
  #   })
  #   observe ({
  #     Restrictions$apply[input$rvNumber] <<- input$apply
  #     reac$newdisplay <- TRUE
  #     reac$newspec <- TRUE
  #   })
  #   observe ({
  #     Restrictions$min[input$rvNumber] <<- input$rmin
  #     reac$newdisplay <- TRUE
  #     reac$newspec <- TRUE
  #   })
  #   observe ({
  #     Restrictions$max[input$rvNumber] <<- input$rmax
  #     reac$newdisplay <- TRUE
  #     reac$newspec <- TRUE
  #   })
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
  } )
  observeEvent (input$prevT, {
    dt <- difftime (times[2], times[1])
    times[1] <<- times[1] - dt
    times[2] <<- times[2] - dt
    updateSliderInput (session, 'times', value=times)
    updateTextInput (session, 'tstart', value=formatTime (times[1]))
    updateTextInput (session, 'tend',   value=formatTime (times[2]))
  } )
  observeEvent (input$prev, Repeat (-1))
  observeEvent (input$statVariables, {
    chooseVar (fname, inp=input)
    ## check if any requested variables not present in Data:
    if (any (!(sVarList %in% VarList))) {
      VarList <<- unique (c(VarList, sVarList))
      print (c(VarList, sVarList))
      isolate (reac$newdata <- reac$newdata + 1)
    }
    isolate (reac$stats <- reac$stats + 1)
  })
  observeEvent (input$ncplot, OpenInProgram (data(), warnOverwrite=FALSE))
  observeEvent (input$Xanadu, OpenInProgram (data(), 'Xanadu', warnOverwrite=FALSE))
  observeEvent (input$maneuvers, SeekManeuvers (data ()))
  observeEvent (input$manual, seeManual ())
  
  #   observe ({                              ## typeFlight
  #     if (Trace) {print (sprintf ('entered typeFlight observer with value %s', input$typeFlight))}
  #     typeFlight <<- input$typeFlight
  #     reac$newdata <- TRUE
  #   })
  
  
  #   
  #   observe ({                             ## time
  #     if (Trace) {print ('setting time')}
  #     if (Trace) {print (sprintf ('Project and Flight: %s %s%02d',
  #                                 isolate(input$Project),
  #                                 isolate (input$typeFlight),
  #                                 isolate (input$Flight)))}
  #     #     reac$newdisplay
  #     #     reac$newdisplay <- TRUE
  #     Data <- data ()
  #     if (length (Data) < 2) {
  #       reac$newdata <- TRUE
  #       if (Trace) {print ('error, need data first')}
  #       return ()
  #     }
  #     step <- 60
  #     minT <- Data$Time[1]
  #     minT <- minT - as.integer (minT) %% step
  #     maxT <- Data$Time[nrow(Data)]
  #     maxT <- maxT - as.integer (maxT) %% step + step
  #     if (Trace) {print (sprintf ('slider values %s %s', formatTime (minT),
  #                                 formatTime (maxT)))}
  #     updateSliderInput(session, inputId='times', label=NULL,
  #                       value=c(minT, maxT),
  #                       min=minT, max=maxT)
  #     times <<- c(minT, maxT)
  #   }, priority=0)
  #   
  
  ################ REACTIVES ########################
  
  reac <- reactiveValues (newdata=0, newdisplay=0, newtrack=0, 
                          newstats=0, newhistogram=0)
  
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
    
    DataR <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, plotSpec$Plot[[input$plot]]$restrict)
    plt <- isolate (input$plot)
    spec <- plotSpec$Plot[[plt]]
    nrws <- ceiling (spec$panels / spec$columns)
    nmx <- nrws * spec$columns
    layout(matrix(1:nmx, ncol = spec$columns), widths = 1, 
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
      # }
      #       si <- input$plot
      #       updateSelectInput (session, 'Rplot', selected=st[si])
      
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
    DataR <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
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
    #     plt <- isolate (input$plot)
    #     spec <- plotSpec$Plot[[plt]]
    #     nrws <- ceiling (spec$panels / spec$columns)
    #     nmx <- nrws * spec$columns
    #     layout(matrix(1:nmx, ncol = spec$columns), widths = 1, 
    #            heights = c(rep(5,spec$panels-1),6))
    #     op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
    #     # ylb <- expression (paste ("temperature  ATy  [", degree, "C]"))
    #     #   g1 <- ggplotWAC (data[, c("Time", spec$panel[[1]]$var)],
    #     #            ylab=spec$panel[[1]]$var[1], lty=c(1,1,1,2,3), lwd=c(1,1.5,1,2,1),
    #     #            legend.position='bottomleft')+xlab(NULL)
    #     #   g2 <- ggplotWAC (data[, c("Time", spec$panel[[2]]$var)],
    #     #                    ylab=spec$panel[[2]]$var[1], lty=c(1,1,1,2,3), lwd=c(1,1.5,1,2,1),
    #     #                    legend.position='bottomleft')
    #     #   # suppressWarnings (print (g))
    #     #   multiplot (g1,g2)
    #     for (pnl in 1:spec$panels) {
    #       if ((pnl == spec$panels) || (pnl %% nrws == 0)) {
    #         op <- par (mar=c(5,4,1,2)+0.1)
    #       } else {
    #         op <- par (mar=c(2,4,1,2)+0.1)
    #       }
    #       if (spec$panel[[pnl]]$logY) {
    #         logY <- 'y'
    #         v <- spec$panel[[pnl]]$var
    #         for (vv in v) {
    #           DataR <- DataR[!is.na (DataR[, vv]), ]
    #           DataV <- DataV[!is.na(DataV[,vv]), ]
    #         }
    #       } else {
    #         logY <- ''
    #       }
    #       yl <- NULL
    #       if (spec$panel[[pnl]]$fixed) {yl <- spec$panel[[pnl]]$ylim}
    #       if (plotSpec$Plot[[input$plot]]$restrict) {
    #         if (is.null (yl)) {
    #           plotWAC (DataV[, c('Time', spec$panel[[pnl]]$var)], log=logY,
    #                    col=spec$panel[[pnl]]$col,
    #                    lwd=spec$panel[[pnl]]$lw,
    #                    lty=spec$panel[[pnl]]$lt)  
    #         } else {
    #           plotWAC (DataV[, c('Time', spec$panel[[pnl]]$var)], ylim=yl, log=logY,
    #                    col=spec$panel[[pnl]]$col,
    #                    lwd=spec$panel[[pnl]]$lw,
    #                    lty=spec$panel[[pnl]]$lt)  
    #         }
    #       } else {
    #         if (is.null (yl)) {
    #           plotWAC (DataR[, c('Time', spec$panel[[pnl]]$var)], log=logY,
    #                    col=spec$panel[[pnl]]$col,
    #                    lwd=spec$panel[[pnl]]$lw,
    #                    lty=spec$panel[[pnl]]$lt) 
    #         } else {
    #           plotWAC (DataR[, c('Time', spec$panel[[pnl]]$var)], ylim=yl, log=logY,
    #                    col=spec$panel[[pnl]]$col,
    #                    lwd=spec$panel[[pnl]]$lw,
    #                    lty=spec$panel[[pnl]]$lt) 
    #         }
    #       }
    #       # }
    #       #       si <- input$plot
    #       #       updateSelectInput (session, 'Rplot', selected=st[si])
    #       
    #     }
    if (input$footer) {AddFooter ()}
    if (Trace) {
      print ('finished plot generation')
    }
  }, width=920, height=640)
  
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
    DataR <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
    ## see global.R functions:
    DataV <- limitData (DataR, input, input$limits2)
    ndv <- names (DataV)
    SE <- getStartEnd (DataR$Time)
    #       
    #       for (n in namesV) {
    #         Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
    #       }
    #       # Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
    #       Data <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
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
      # Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
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
      # Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
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
                              +ggtitle(sprintf("Flight %s, whole-flight-average", Flight))))
      
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
    Ds <- Ds[(Ds$Time > times[1]) & (Ds$Time < times[2]), ]
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
    DataR <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
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
    DataR <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
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
    Ds <- Ds[(Ds$Time >= times[1]) & (Ds$Time <= times[2]), ]
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


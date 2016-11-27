# ui.R

## This is the user interface for the Ranadu shinyApp.


step <- 60
shinyUI(
  fluidPage (theme='www/bootstrap.css',    ##navbarPage('Explorer',  ##fluidPage(
    # titlePanel("Explorer"),
    # tabPanel ('plot vs time',
    # tags$style (type='text/css', '.well {max-height:140px;}'),
    navlistPanel (tabPanel (strong('data & plot'), fluidRow (
      column (4, wellPanel (
        fluidRow (
          column (6, selectInput (inputId='Project', label=NULL,
                       choices=PJ, selected=plotSpec$Project, width='100px')),
          column (6, actionButton (inputId='manual', label = 'see\nmanual'))
        ),

        fluidRow (
          column (4, downloadButton ('savePDF', label='PDF')), # icon=icon('file-pdf-o'))),
          column (4, downloadButton (outputId='savePNG', label='PNG')), # icon=icon('file-image-o'))),
          column (4, actionButton (inputId='saveRdata', label='R', icon=icon('file-archive-o')))
        )
      )),
      column (2, wellPanel (
        fluidRow (
          column (8, numericInput (inputId='Flight', label='Flight', value=1,
                                   min=1, max=99, step=1, width='80px')),
          column (4, radioButtons ('typeFlight', label=NULL, choices=c('rf', 'tf', 'ff', 'F'),
                                   width='70px', inline=FALSE))))),
      column (1, 
              numericInput (inputId='plot', label='plot', value=1,
                            min=1, max=5, step=1),
              actionButton ('check', 'quick')),
      column(5, wellPanel (
        fluidRow (
          column (6, selectInput ('restore', label=NULL, choices=defFiles, selected='plotSpec.def')),
          column (6, textInput ('save', label=NULL, value='plotSpec.def'))
        ), 
        fluidRow (
          column (6, actionButton (inputId='specRead', label='Read Specs')),
          column (6, actionButton (inputId='specSave', label='Save Specs'))
        )
      )))),
      
      #       tags$head(
      #         tags$style(type="text/css", ".irs {max-height:55px;}"),
      #         tags$style (type='text/css', '.well {max-height:140px;}')
      #       ),  
      #       tags$head (
      #         tags$style(type='text/css', 'irs {max-height:55px;')
      #         # tags$style(type='text/css', 'textinput {max-height:55px;'),
      #         # tags$style(type='text/css', 'button-text {font-size:12px; height:20px;}'),
      #       ),
      
      tabPanel (strong('restrictions'), 
                column(6, 
                       tags$style(type='text/css', '.well {max-height:120px;top:0px'),
                       wellPanel(
                         tags$style(type='text/css', 'irs {max-height:53px;'),
                         sliderInput("times", label=NA, min=plotSpec$Times[1], max=plotSpec$Times[2],
                                     value=plotSpec$Times,
                                     # animate=TRUE,
                                     step=step,  
                                     timeFormat='%T', dragRange=TRUE,
                                     timezone='+0000'),
                         fluidRow (
                           #                     column (4, tags$textarea(id = "tstart", rows = "1", cols = "11", value=formatTime(time[1]))),  # text input with 10 rows aka lines
                           #                     column (4, tags$textarea(id = "t-end", rows = "1", cols = "11")),
                           column (4, 
                                   # tags$style(type="text/css", "input.shiny-bound-input {font-size:12px; height:15px;}"),
                                   textInput ('tstart', label=NULL, value=formatTime(plotSpec$Times[1])), 
                                   tags$style(type='text/css', "#tstart {width: 100px; height: 22px; }")),
                           column (4, 
                                   # tags$style(type="text/css", "input.shiny-bound-input {font-size:12px; height:15px;}"),
                                   textInput ('tend', label=NULL, value=formatTime(plotSpec$Times[2])), 
                                   tags$style(type='text/css', "#tend {width: 100px; height: 22px; }")),
                           column (2, shinyBS::bsButton ('resetT', label='reset', size='extra-small')),
                           column (1, shinyBS::bsButton ('prevT', label=NULL, icon=icon('angle-left'), size='extra-small')),
                           # tags$style(type='text/css', "#button { vertical-align: middle; height: 15px; width: 100%; font-size: 12px;}")
                           
                           column (1, shinyBS::bsButton ('nextT', label=NULL, icon=icon('angle-right'), size='extra-small'))
                           # tags$style(type='text/css', "#button { vertical-align: middle; height: 15px; width: 100%; font-size: 12px;}")
                         )
                       )),
                column (6, wellPanel ( fluidRow (
                  # tags$style(type="text/css", "input.shiny-bound-input {font-size:14px; height:30px;}"),
                  column (2, numericInput ('rvNumber', 'R#', 1)),
                  column (3, selectInput ('rvar', 'var', choices=c(sort(FI$Variables)), 
                                          selected=plotSpec$Restrictions$RVAR[1])),
                  column (1, '?', checkboxInput ('apply', label=NULL, 
                                                 value=plotSpec$Restrictions$apply[1])),
                  column (3, numericInput ('rmin', 'min', plotSpec$Restrictions$min[1])),
                  column (3, numericInput ('rmax', 'max', plotSpec$Restrictions$max[1]))
                ) 
                
                ))), widths=c(2,10)),
    bsAlert("pleasewait"),
    bsModal("quickCheck", title=NULL, trigger='check', size = "large", plotOutput("quickPlot")),
    tabsetPanel (id='whichTab', type='pills',
                 tabPanel ('plot vs time',
                           sidebarLayout(
                             sidebarPanel(h4('define plot'), 
                                          fluidRow (
                                            column (6, numericInput ('panels', 'panels', 
                                                                     plotSpec$Plot[[1]]$panels, width='60px')),
                                            column (6, numericInput ('cols', 'cols', plotSpec$Plot[[1]]$columns, min=1, width='60px'))),
                                          fluidRow (
                                            column (6, numericInput ('panel', 'panel', 1, min=1, max=6,width='50px')),
                                            column (6, checkboxInput ('logY', 'log?'),
                                                    checkboxInput ('fixed', 'set ylim?', 
                                                                   value=plotSpec$Plot[[1]]$panel[[1]]$fixed)
                                            )),
                                          fluidRow (
                                            column (6, numericInput ('panelMin', 'min', plotSpec$Plot[[1]]$panel[[1]]$ylim[1])),
                                            column (6, numericInput ('panelMax', 'max', plotSpec$Plot[[1]]$panel[[1]]$ylim[2]))),
                                          fluidRow (
                                            column (7, checkboxInput ('restrict','apply restrictions', plotSpec$Plot[[1]]$restrict)),
                                            column (5, checkboxInput ('footer','footer?'))),
                                          wellPanel (
                                            fluidRow (
                                              column (2, 'line:'),
                                              column (4, numericInput ('lineV', NULL, 1, width='70px')),
                                              column (2, 'lwd:'),
                                              column (4, numericInput ('lineW', NULL, 1, width='70px'))),
                                            
                                            fluidRow (
                                              column (1, 'v:'),
                                              column (11, selectInput ('addVarP', label=NULL,
                                                         choices=c('select', 'omit', sort(FI$Variables)), 
                                                         selected=plotSpec$Plot[[1]]$panel[[1]]$var[1]))),
                                            fluidRow(
                                              column (1, 'l:'),
                                              column (11, textInput ('ylbl', label=NULL, value=plotSpec$Plot[[1]]$panel[[1]]$var[1]))),
                                            selectInput ('varColor', NULL, c('blue', 'darkgreen', 'red',
                                                                             'cyan', 'violet', 'darkorange',
                                                                             'brown', 'black')),
#                                             fluidRow (
#                                               column (6, 'width:'),
#                                               column (6, numericInput ('lineW', NULL, 1, width='90px'))),
                                            fluidRow (
                                                column (6, radioButtons ('lineStyle', label='line type', choices=ltyps, inline=TRUE, 
                                                          selected=ltyps[plotSpec$Plot[[1]]$panel[[1]]$lt[1]],
                                                          width='90px')),
                                                column (6, checkboxInput ('smooth','smooth?', plotSpec$Plot[[1]]$panel[[1]]$smooth[1]),
                                                        numericInput ('SGpoints', 'SG pts', plotSpec$Plot[[1]]$panel[[1]]$SGlength[1])
                                                        ))),
                                          width=3),
                             
                             mainPanel( tabsetPanel (tabPanel ('plots', plotOutput (outputId='display', click=clickOpts(id='plot_click'), 
                                                                                    brush=brushOpts(id='plot_brush', delay=3000, delayType='debounce', resetOnNew=TRUE))),
                                                     tabPanel ('stats', dataTableOutput ('stats')),
                                                     tabPanel ('histograms', plotOutput (outputId='hist')),
                                                     tabPanel ('soundings', plotOutput (outputId='barWvsZ')),
                                                     tabPanel ('listing', dataTableOutput ('listing')), 
                                                     tabPanel ('checkV', h2('1-min averages centered on listed time'),
                                                               textInput ('RefT', label=NULL, value=formatTime(checkTime)), 
                                                               dataTableOutput ('checkV')), 
                                                     id='display')
                              ))
                           
                           # ),
                           # tabPanel ('track')
                 ), 
                 tabPanel ('track',
                           sidebarLayout(
                             sidebarPanel(h4('track definition'), 
                                          checkboxInput ('limits2','apply restrictions'), 
                                          checkboxInput ('footer2', 'footer?'),
                                          checkboxInput ('drift', 'drifting?'),
                                          numericInput ('track.xc', 'center long.', value=NULL),
                                          numericInput ('track.yc', 'center lat.', value=NULL),
                                          numericInput ('track.sz', 'size (deg.', value=NULL),
                                          numericInput ('track.spacing', 'time between labels (min)', 15),
                                          numericInput ('track.WF', 'scale for wind flags', 5),
                                          width=2),
                             
                             mainPanel( tabsetPanel (tabPanel ('plan view', plotOutput (outputId='track')),
                                                     tabPanel ('time-height', plotOutput ('theight'))
                             )))
                 ),
                 tabPanel ('stats/listing',
                           sidebarLayout(
                             sidebarPanel(h4('stats/listing definition'), 
                                          checkboxInput ('limits2a','apply restrictions'), 
                                          actionButton ('statVariables', 'select variables'),
                                          radioButtons ('statslist', label='type of output', choices=c('stats', 'listing')),
                                          numericInput ('avgsec', label='for listing, ave. seconds:', value=1, min=1),
                                          width=2),
                             mainPanel(dataTableOutput ('statistics')))
                 ),
                 
                 navbarMenu ('more plots', 
                             tabPanel ('histogram',
                                       sidebarLayout(
                                         sidebarPanel(h4('histogram definition'), 
                                                      fluidRow (
                                                        column (6, checkboxInput ('limits3','apply restrictions')),
                                                        column (6, checkboxInput ('densityH', 'plot density?'))
                                                      ),
                                                      fluidRow (
                                                        column (6, checkboxInput ('cdf', 'include CDF')),
                                                        column (6, checkboxInput ('hfooter','footer? (NA)'))),
                                                      fluidRow (
                                                        column (6, numericInput ('hpanels', 'panels', 
                                                                                 plotSpec$Hist[[1]]$panels, width='60px')),
                                                        column (6, numericInput ('hcols', 'cols', plotSpec$Hist[[1]]$columns, min=1, width='60px'))),
                                                      fluidRow (
                                                        column (4, numericInput ('hpanel', 'panel', 1, min=1, max=5,width='50px')),
                                                        column (4, numericInput ('hbins', 'bins', 50, min=10)),
                                                        column (4, checkboxInput ('hlogY', 'log? (NA)'),
                                                                checkboxInput ('hfixed', 'set xlim?', 
                                                                               value=plotSpec$Hist[[1]]$panel[[1]]$fixed)
                                                        )),
                                                      fluidRow (
                                                        column (6, numericInput ('hpanelMin', 'xmin', plotSpec$Hist[[1]]$panel[[1]]$ylim[1])),
                                                        column (6, numericInput ('hpanelMax', 'xmax', plotSpec$Hist[[1]]$panel[[1]]$ylim[2]))),
                                                      wellPanel (
                                                        fluidRow (
                                                          column (6, h4('line:')),
                                                          column (6, numericInput ('hlineV', NULL, 1, width='90px'))),
                                                        selectInput ('haddVarP', label=NULL,
                                                                     choices=c('select', 'omit',sort(FI$Variables)), 
                                                                     selected=plotSpec$Hist[[1]]$panel[[1]]$var[1]),
                                                        selectInput ('hvarColor', NULL, c('blue', 'darkgreen', 'red',
                                                                                          'cyan', 'violet', 'darkorange',
                                                                                          'brown', 'black')),
                                                        fluidRow (
                                                          column (6, 'width:'),
                                                          column (6, numericInput ('hlineW', NULL, 1, width='90px'))),
                                                        radioButtons ('hlineStyle', label='line type', choices=ltyps, inline=TRUE, 
                                                                      selected=ltyps[plotSpec$Hist[[1]]$panel[[1]]$lt[1]],
                                                                      width='90px')),
                                                      width=3),
                                         
                                         mainPanel(plotOutput (outputId='histogram')))
                             ),
                             tabPanel ('scatterplot',
                                       sidebarLayout(
                                         sidebarPanel(h4('scatterplot'),  
                                                      fluidRow (
                                                        column (7, checkboxInput ('limits4','restrictions')),
                                                        column (5, checkboxInput ('sfooter','footer? (NA)'))),
                                                      fluidRow (
                                                        column (6, numericInput ('spanels', 'panels', 
                                                                                 plotSpec$Scat[[1]]$panels, width='60px')),
                                                        column (6, numericInput ('scols', 'cols', plotSpec$Scat[[1]]$columns, min=1, width='60px'))),
                                                      fluidRow (
                                                        column (6, numericInput ('spanel', 'panel', 1, min=1, max=5,width='50px')),
                                                        column (6, checkboxInput ('slogX', 'log x?'),
                                                                checkboxInput ('slogY', 'log y?'),
                                                                checkboxInput ('sfixed', 'fixed?', 
                                                                               value=plotSpec$Scat[[1]]$panel[[1]]$fixed)
                                                        )),
                                                      
                                                      wellPanel (
                                                        fluidRow (
                                                          column (5, h4('pair:')),
                                                          column (7, numericInput ('slineV', NULL, 1, width='90px'))),
                                                        fluidRow (
                                                          column (2, 'x'),
                                                          column (10, selectInput ('saddVarP1', label=NULL,
                                                                                   choices=c('select', 'omit',sort(FI$Variables)), 
                                                                                   selected=plotSpec$Scat[[1]]$panel[[1]]$varx))),
                                                        fluidRow (
                                                          column (2, 'y'),
                                                          column (10, selectInput ('saddVarP2', label=NULL,
                                                                                   choices=c('select', 'omit',sort(FI$Variables)), 
                                                                                   selected=plotSpec$Scat[[1]]$panel[[1]]$vary[1]))),
                                                        selectInput ('svarColor', NULL, c('blue', 'darkgreen', 'red',
                                                                                          'cyan', 'violet', 'darkorange',
                                                                                          'brown', 'black')),
                                                        fluidRow (
                                                          column (6, 'symbol size:'),
                                                          column (6, numericInput ('ssize', NULL, 1, width='90px'))),
                                                        numericInput ('symbol', 'symbol', plotSpec$Scat[[1]]$panel[[1]]$symbol[1], width='90px'),
                                                        fluidRow (
                                                          column (6, numericInput ('spanelMinx', 'xmin', plotSpec$Scat[[1]]$panel[[1]]$xlim[1])),
                                                          column (6, numericInput ('spanelMaxx', 'xmax', plotSpec$Scat[[1]]$panel[[1]]$lim[2]))),
                                                        fluidRow (
                                                          column (6, numericInput ('spanelMiny', 'ymin', plotSpec$Scat[[1]]$panel[[1]]$ylim[1])),
                                                          column (6, numericInput ('spanelMaxy', 'ymax', plotSpec$Scat[[1]]$panel[[1]]$ylim[2])))),
                                                      width=3),
                                         
                                         mainPanel(plotOutput (outputId='scatterplot')))
                             ),
                             tabPanel ('bin-average plots',    
                                       sidebarLayout(
                                         sidebarPanel(h4('bin averages'),
                                                      checkboxInput ('limits5','apply restrictions'),
                                                      fluidRow (
                                                        column (5, checkboxInput ('bfooter','footer? (NA)'))),
                                                      fluidRow (
                                                        column (6, numericInput ('bpanels', 'panels', 
                                                                                 plotSpec$Bin[[1]]$panels, width='60px')),
                                                        column (6, numericInput ('bcols', 'cols', plotSpec$Bin[[1]]$columns, min=1, width='60px'))),
                                                      fluidRow (
                                                        column (6, numericInput ('bpanel', 'panel', 1, min=1, max=5,width='50px'),
                                                                checkboxInput ('bhoriz', 'horizontal?')),
                                                        
                                                        column (6, checkboxInput ('blogX', 'log x?'),
                                                                checkboxInput ('blogY', 'log y?'),
                                                                checkboxInput ('bfixed', 'fixed?', 
                                                                               value=plotSpec$Bin[[1]]$panel[[1]]$fixed)
                                                        )),
                                                      
                                                      wellPanel (
                                                        fluidRow (
                                                          column (5, h4('pair:')),
                                                          column (7, numericInput ('blineV', NULL, 1, width='90px'))),
                                                        fluidRow (
                                                          column (2, 'x'),
                                                          column (10, selectInput ('baddVarP1', label=NULL,
                                                                                   choices=c('select', 'omit',sort(FI$Variables)), 
                                                                                   selected=plotSpec$Bin[[1]]$panel[[1]]$varx))),
                                                        fluidRow (
                                                          column (2, 'y'),
                                                          column (10, selectInput ('baddVarP2', label=NULL,
                                                                                   choices=c('select', 'omit',sort(FI$Variables)), 
                                                                                   selected=plotSpec$Bin[[1]]$panel[[1]]$vary[1]))),
                                                        selectInput ('bvarColor', NULL, c('blue', 'darkgreen', 'red',
                                                                                          'cyan', 'violet', 'darkorange',
                                                                                          'brown', 'black')),
                                                        fluidRow (
                                                          column (6, 'symbol size:'),
                                                          column (6, numericInput ('bsize', NULL, 1, width='90px'))),
                                                        numericInput ('bsymbol', 'symbol', plotSpec$Bin[[1]]$panel[[1]]$symbol[1], width='90px'),
                                                        fluidRow (
                                                          column (6, numericInput ('bpanelMinx', 'xmin', plotSpec$Bin[[1]]$panel[[1]]$xlim[1])),
                                                          column (6, numericInput ('bpanelMaxx', 'xmax', plotSpec$Bin[[1]]$panel[[1]]$lim[2]))),
                                                        fluidRow (
                                                          column (6, numericInput ('bpanelMiny', 'ymin', plotSpec$Bin[[1]]$panel[[1]]$ylim[1])),
                                                          column (6, numericInput ('bpanelMaxy', 'ymax', plotSpec$Bin[[1]]$panel[[1]]$ylim[2])))),
                                                      width=3),
                                         
                                         mainPanel(plotOutput (outputId='binplot')))
                             )),
                 
                 navbarMenu ('particles',
                             tabPanel ('size distributions',
                                       sidebarLayout(
                                         sidebarPanel(h4('define size distributions'), 
                                                      checkboxInput ('limits7','apply restrictions'), 
                                                      selectInput ('sdtype', 'type of axes',
                                                                   choices=c('linear', 'log-y', 'log-x', 'both log'),
                                                                   selected='both log'),
                                                      checkboxGroupInput ('probe', 'probe?', c('CDP', 'FSSP', 'UHSAS', 'PCASP', '2DC'),
                                                                          selected='CDP'),
                                                      width=2),
                                         
                                         mainPanel(plotOutput (outputId='sdplot')))
                             ),
                             tabPanel ('hydrometeor images',
                                       sidebarLayout (
                                         sidebarPanel (h4('2D probe images'), 
                                                       actionButton ('fname2d', 'select input file'),
                                                       textInput ('fnametext', 'file selected:', value=plotSpec$fname2d),
                                                       radioButtons ('mode2d', 'type of display:',
                                                                     choices=c('record', 'page', 'second (max)', 'image')),
                                                       fluidRow (
                                                         column (7, 'min. seconds between displayed records:'),
                                                         column (5, numericInput ('max2d', label=NULL, value=0, min=0, width='200px'))),
                                                       shinyBS::bsButton ('prev2d', label=NULL, icon=icon('angle-left'), size='small'),
                                                       shinyBS::bsButton ('next2d', label=NULL, icon=icon('angle-right'), size='small'),
                                                       width=3),
                                         mainPanel(plotOutput (outputId='image2d'))))),
                 navbarMenu ('thermo. diagrams',
                             tabPanel ('skewT diagram',
                           sidebarLayout(
                             sidebarPanel(h4('skew-T definition'), 
                                          checkboxInput ('limits6', 'apply restrictions'), 
                                          numericInput ('sndbins', 'bins for sounding', value=50, min=20, max=1000),
                                          checkboxInput ('hodograph', 'show hodograph?'),
                                          checkboxInput ('cape', 'CAPE / LCL / adiabats?'),
                                          checkboxInput ('footer6', 'footer?'),
                                          width=2),
                             
                             mainPanel(plotOutput (outputId='skewT')))
                 ),
                 tabPanel ('mixing and stability plots',
                                     sidebarLayout(
                                       sidebarPanel (h4('Paluch / sat.-point, stability diagrams'),
                                                     checkboxInput ('limits9', 'apply restrictions'),
                                                     selectInput ('paluchBetts', label=NULL, choices=c('Paluch', 'Betts sat. point', 'stability profiles')),
                                                     h4('sounding times:'),
                                                     fluidRow (
                                                       column (6, textInput ('paluchStart', label=NULL, value=formatTime (plotSpec$PaluchTimes[1]))),
                                                       column (6, textInput ('paluchEnd', label=NULL, value=formatTime (plotSpec$PaluchTimes[2])))
                                                     ),
                                                     fluidRow (
                                                       column (2, 'bins:'),
                                                       column (4, numericInput ('nbsa', label=NULL, value=18, min=5, width='70px')),
                                                       column (2, 'avg:'),
                                                       column (4, numericInput ('nbss', label=NULL, value=7, min=5, width='70px'))
                                                     ),
                                                     selectInput('paluchLWC', 'LWC variable', choices=c(sort(FI$Variables)), 
                                                                 selected=plotSpec$paluchLWC),
                                                     h4('in-cloud times for mixing diagrams:'),
                                                     fluidRow (
                                                       column (6, textInput ('paluchCStart', label=NULL, value=formatTime (plotSpec$PaluchCTimes[1]))),
                                                       column (6, textInput ('paluchCEnd', label=NULL, value=formatTime (plotSpec$PaluchCTimes[2])))
                                                     ),
                                                     h4 ('variable for stability profiles:'),
                                                     radioButtons ('tvORthetap', label=NULL, choices=c('THETAV', 'THETAP'), inline=TRUE),
                                                     width=4),
                                     mainPanel (plotOutput (outputId='paluch')))
                                     )
                 ),
                 tabPanel ('variance spectra',
                           sidebarLayout(
                             sidebarPanel(h4('spectral analysis'), 
                                          checkboxInput ('limits8','apply restrictions'),
                                          selectInput('specvar', 'variable', choices=c(sort(FI$Variables)), 
                                                      selected=plotSpec$Variance[[1]]$Definition$var),
                                          selectInput('speccovar', 'co-variable', choices=c(sort(FI$Variables)), 
                                                      selected=plotSpec$Variance[[1]]$Definition$cvar),
                                          tabsetPanel (type='pills',
                                                       tabPanel ('fft',
                                                                 numericInput ('fftpts', 'segment length', plotSpec$Variance[[1]]$Definition$fftpts),
                                                                 selectInput ('fftwindow', label=NULL, choices=c('Parzen', 'square', 'Welch', 'Hanning')),
                                                                 numericInput ('fftavg', 'log avg intervals', plotSpec$Variance[[1]]$Definition$fftavg),
                                                                 checkboxInput ('ffterrbar', 'error bars?'),
                                                                 selectInput ('ffttype', label=NULL, choices=c('fp(f)', 'p(f)', 'both fp(f)',
                                                                                                               'cospec. / quad.', 'coherence / phase', 'data'))),
                                                       tabPanel ('acv',
                                                                 checkboxInput ('acvdetrend', 'remove trend?', value=TRUE),
                                                                 numericInput ('acvtau', 'smoothing time', plotSpec$Variance[[1]]$Definition$acvtau),
                                                                 numericInput ('acvavg', 'log avg intervals', plotSpec$Variance[[1]]$Definition$acvavg),
                                                                 selectInput ('acvtype', label=NULL, choices=c('fp(f)', 'p(f)', 'var2 fp(f)', 'var2 p(f)', 
                                                                                                               'autocorrelation', 'crosscorrelation', 'data'))),
                                                       tabPanel ('MEM',
                                                                 selectInput ('MEMtype', label=NULL, choices=c('fp(f)', 'p(f)', 'co-variable fp(f)',
                                                                                                               'cospectrum', 'quadrature', 'coherence', 'phase')),
                                                                 numericInput ('MEMpoles', 'poles', plotSpec$Variance[[1]]$Definition$MEMpoles),
                                                                 numericInput ('MEMres', 'resoln', plotSpec$Variance[[1]]$Definition$MEMres),
                                                                 selectInput ('MEMcolor', 'color', choices=c('blue', 'darkgreen', 'red', 'cyan', 'darkorange', 'brown', 'magenta', 'black')),
                                                                 numericInput ('MEMavg', 'log avg intervals', plotSpec$Variance[[1]]$Definition$MEMavg),
                                                                 checkboxInput ('MEMadd', 'add to prev. plot', value=FALSE)
                                                       ), id='spectype'),
                                          width=3),
                             
                             mainPanel(plotOutput (outputId='varplot')))
                 ),
                 navbarMenu ('utilities',
                             tabPanel ('run other programs',
                                       selectInput ('otherprogram', label=NULL, 
                                                    actionButton ('xfrVariables', 'select variables'),
                                                    choices=c('ncplot', 'Xanadu', 'python', 
                                                              'NCL', 'IDL', 'IDV')),
                                       
                                       checkboxInput ('limits10','apply restrictions'), 
                                       actionButton ('xfrVariables', 'select variables'),
                                       actionButton ('ncplot', 'start program')),
                             tabPanel ('fits to data',
                                       sidebarLayout (
                                         sidebarPanel (h4('linear fits'),
                                                       checkboxInput ('limitsFit', 'apply restrictions?', value=TRUE),
                                                       selectInput ('response', label='response variable', 
                                                                    choices=sort(VarList)),
                                                       helpText(h4('formula use:'),
                                                                'the fit formula can take several forms:',
                                                                tags$ul(
                                                                  tags$li('a single variable'),
                                                                  tags$li('multiple variables separated by + signs (e.g., A+B)'),
                                                                  tags$li('additional expressions enclosed in isolating expressions ',
                                                                          tags$strong('+I()'),', where the enclosed formula may',
                                                                          'use operatioins like *, /, etc., or ^ for powers (e.g., A^2 for the square of A)'),
                                                                  tags$li('you can also use new variables previously defined by the utility \"create new variable\"')
                                                                )),
                                                       textInput ('fformula', 'fit formula (example shown)', placeholder='I(ADIFR/QCF)+I(MACHX^3)'),
                                                       actionButton ('lfit', 'show linear fit'),
                                                       actionButton ('dfit', 'show Deming fit')
                                         ), 
                                         mainPanel (plotOutput (outputId='fitplot'),
                                                    htmlOutput (outputId='fittext'),
                                                    tableOutput (outputId='coeftable'))
                                       )),
                             tabPanel ('create new variable',
                                       actionButton ('createV', 'create new variable'),
                                       helpText (h4('Usage:'), 
                                                 'Provide a name for the new variable,',
                                                 'then give a formula for calculating it. The',
                                                 'formula can use names of existing variables',
                                                 'if they are separated by spaces from other',
                                                 'text or numbers, and it can use standard',
                                                 'math operations like +-*/ or math functions',
                                                 'like sin(). An example is shown. Click the',
                                                 'top button once the formula is ready.', 
                                                 h4('Warning:'), 'If the',
                                                 'variable already exists it will be replaced.'),
                                       textInput ('newvar', 'name of new variable', value='AOAREF'),
                                       textInput ('formla', 'formula for new variable', 
                                                  value='PITCH - GGVSPD / TASX * 180 / pi')))
    )
  )
)





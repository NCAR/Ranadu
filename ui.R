# ui.R


step <- 60
shinyUI(
  fluidPage (    ##navbarPage('Explorer',  ##fluidPage(
    # titlePanel("Explorer"),
    # tabPanel ('plot vs time',
    # tags$style (type='text/css', '.well {max-height:140px;}'),
    navlistPanel (tabPanel (strong('data & plot'), fluidRow (
      column (4, wellPanel (
        selectInput (inputId='Project', label=NULL,
                     choices=PJ, width='100px'),
        fluidRow (
          column (4, actionButton (inputId='savePDF', label='PDF', icon=icon('file-pdf-o'))),
          column (4, actionButton (inputId='savePNG', label='PNG', icon=icon('file-image-o'))),
          column (4, actionButton (inputId='saveRdata', label='R', icon=icon('file-archive-o')))
        )
      )),
      column (2, wellPanel (
        fluidRow (
          column (8, numericInput (inputId='Flight', label='Flight', value=1,
                                   min=1, max=99, step=1, width='80px')),
          column (4, radioButtons ('typeFlight', label=NULL, choices=c('rf', 'tf', 'ff'),
                                   width='70px', inline=TRUE))))),
      column (1, 
              numericInput (inputId='plot', label='plot', value=1,
                            min=1, max=49, step=1)),
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
                  tags$style(type='text/css', 'irs {max-height:55px;'),
                  sliderInput("times", label=NA, min=times[1], max=times[2],
                              value=times,
                              # animate=TRUE,
                              step=step,  
                              timeFormat='%T', dragRange=TRUE,
                              timezone='+0000'),
                  fluidRow (
#                     column (4, tags$textarea(id = "tstart", rows = "1", cols = "11", value=formatTime(time[1]))),  # text input with 10 rows aka lines
#                     column (4, tags$textarea(id = "t-end", rows = "1", cols = "11")),
                    column (4, 
                            # tags$style(type="text/css", "input.shiny-bound-input {font-size:12px; height:20px;}"),
                            textInput ('tstart', label=NULL, value=formatTime(times[1])), class='input-small',
                            tags$style(type='text/css', "#tstart { height: 26px; }")),
                    column (4, 
                            # tags$style(type="text/css", "input.shiny-bound-input {font-size:12px; height:20px;}"),
                            textInput ('t-end', label=NULL, value=formatTime(times[2])), class='input-small',
                            tags$style(type='text/css', "#t-end { height: 26px; }")),
                    column (2, shinyBS::bsButton ('prev', label=NULL, icon=icon('angle-left'), size='extra-small')),
                            # tags$style(type='text/css', "#button { vertical-align: middle; height: 15px; width: 100%; font-size: 12px;}")
                          
                    column (2, shinyBS::bsButton ('next', label=NULL, icon=icon('angle-right'), size='extra-small'))
                            # tags$style(type='text/css', "#button { vertical-align: middle; height: 15px; width: 100%; font-size: 12px;}")
                  )
                )),
                column (6, wellPanel ( fluidRow (
                  # tags$style(type="text/css", "input.shiny-bound-input {font-size:14px; height:30px;}"),
                  column (2, numericInput ('rvNumber', 'R#', 1)),
                  column (3, selectInput ('rvar', 'var', choices=c(sort(FI$Variables)), 
                                          selected=Restrictions$RVAR[1])),
                  column (1, '?', checkboxInput ('apply', label=NULL, 
                                                 value=Restrictions$apply[1])),
                  column (3, numericInput ('rmin', 'min', Restrictions$min[1])),
                  column (3, numericInput ('rmax', 'max', Restrictions$max[1]))
                ) 
                
                ))), widths=c(2,10)),

    tabsetPanel (type='pills',
                 tabPanel ('plot vs time',
                           sidebarLayout(
                             sidebarPanel(h4('define plot'), 
                                          fluidRow (
                                            column (6, numericInput ('panels', 'panels', 
                                                                     plotSpec$Plot[[1]]$panels, width='60px')),
                                            column (6, numericInput ('cols', 'cols', plotSpec$Plot[[1]]$columns, min=1, width='60px'))),
                                          fluidRow (
                                            column (6, numericInput ('panel', 'panel', 1, min=1, max=5,width='50px')),
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
                                              column (6, h4('line:')),
                                              column (6, numericInput ('lineV', NULL, 1, width='90px'))),
                                            selectInput ('addVarP', label=NULL,
                                                         choices=c('select', 'omit',sort(FI$Variables)), 
                                                         selected=plotSpec$Plot[[1]]$panel[[1]]$var[1]),
                                            selectInput ('varColor', NULL, c('blue', 'darkgreen', 'red',
                                                                             'cyan', 'violet', 'darkorange',
                                                                             'brown', 'black')),
                                            fluidRow (
                                              column (6, 'width:'),
                                              column (6, numericInput ('lineW', NULL, 1, width='90px'))),
                                            radioButtons ('lineStyle', label='line type', choices=ltyps, inline=TRUE, width='90px')),
                                          width=3),
                             
                             mainPanel( tabsetPanel (tabPanel ('plot', plotOutput (outputId='display')),
                                                     tabPanel ('stats', dataTableOutput ('stats')),
                                                     tabPanel ('histograms', plotOutput (outputId='hist')),
                                                     tabPanel ('soundings', plotOutput (outputId='barWvsZ')),
                                                     tabPanel ('listing', dataTableOutput ('listing')))))
                           
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
                 tabPanel ('histogram',
                           sidebarLayout(
                             sidebarPanel(h4('histogram definition'), 
                                          checkboxInput ('limits3','apply restrictions'), 
                                          width=2),
                             
                             mainPanel())
                 ),
                 tabPanel ('scatterplot',
                           sidebarLayout(
                             sidebarPanel(h4('scatterplot definition'), 
                                          checkboxInput ('limits4','apply restrictions'), 
                                          width=2),
                             
                             mainPanel())
                 ),
                 tabPanel ('bar-vs-height',
                           sidebarLayout(
                             sidebarPanel(h4('box-whisker definition'), 
                                          checkboxInput ('limits5','apply restrictions'), 
                                          width=2),
                             
                             mainPanel())
                 ),
                 tabPanel ('skewT diagram',
                           sidebarLayout(
                             sidebarPanel(h4('skew-T definition'), 
                                          checkboxInput ('limits6', 'apply restrictions'), 
                                          checkboxInput ('footer6', 'footer?'),
                                          width=2),
                             
                             mainPanel(plotOutput (outputId='skewT')))
                 ),
                 tabPanel ('size distributions',
                           sidebarLayout(
                             sidebarPanel(h4('define size distributions'), 
                                          checkboxInput ('limits7','apply restrictions'), 
                                          width=2),
                             
                             mainPanel())
                 ),
                 tabPanel ('variance spectra',
                           sidebarLayout(
                             sidebarPanel(h4('spectral analysis definition'), 
                                          checkboxInput ('limits8','apply restrictions'), 
                                          width=2),
                             
                             mainPanel())
                 )
    )
  )
)



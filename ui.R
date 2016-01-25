# ui.R

minT <- as.POSIXct(0, origin='2012-05-29', tz='UTC')
maxT <- as.POSIXct(3600*8, origin='2012-05-29', tz='UTC')
step <- 60
shinyUI(
  fluidPage (    ##navbarPage('Explorer',  ##fluidPage(
    # titlePanel("Explorer"),
    # tabPanel ('plot vs time',
    navlistPanel (tabPanel ('data & plot', fluidRow (
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
        #         fluidRow (
        #           column(8,
        #                  selectInput ('addVar', label=NULL,
        #                               choices=c('add var',sort(FI$Variables)))),
        #           column (2, actionButton (inputId='selectVariables', label='choose'))),
      )))),
      tabPanel ('restrictions', 
                column(6, wellPanel(
                  sliderInput("times", label=NA, min=minT, max=maxT,
                              value=c(minT, maxT),
                              # animate=TRUE,
                              step=step,  
                              timeFormat='%T', dragRange=TRUE,
                              timezone='+0000')
                )),
                column (6, wellPanel ( fluidRow (
                  #                   column (3, numericInput ('minTAS', 'tas min', 110, width='90px')),
                  #                   column (3, numericInput ('maxROLL', 'roll', 5, width='90px')),
                  #                   column (3, numericInput ('minZ', 'Zmin-km', 2, width='90px')),
                  #                   column (3, numericInput ('maxROC', 'abs ROC', 5, width='90px'))
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
                                          # checkboxInput ('limits','restrictions'), 
                                          fluidRow (
                                            column (6, numericInput ('panels', 'panels', 
                                                                     plotSpec[[1]]$panels, width='60px')),
                                            column (6, numericInput ('cols', 'cols', plotSpec[[1]]$columns, min=1, width='60px'))),
                                          fluidRow (
                                            column (6, numericInput ('panel', 'panel', 1, min=1, max=5,width='50px')),
                                            column (6, checkboxInput ('logY', 'log?'),
                                                    checkboxInput ('fixed', 'set ylim?', 
                                                                   value=plotSpec[[1]]$panel[[1]]$fixed)
                                            )),
                                          fluidRow (
                                            column (6, numericInput ('lineMin', 'min', plotSpec[[1]]$panel[[1]]$ylim[1])),
                                            column (6, numericInput ('lineMax', 'max', plotSpec[[1]]$panel[[1]]$ylim[2]))),
                                          fluidRow (
                                            column (7, checkboxInput ('limits','apply restrictions')),
                                            column (5, checkboxInput ('footer','footer?'))),
                                          wellPanel (
                                            fluidRow (
                                              column (6, h4('line:')),
                                              column (6, numericInput ('lineV', NULL, 1, width='90px'))),
                                            selectInput ('addVarP', label=NULL,
                                                         choices=c('select', 'omit',sort(FI$Variables))),
                                            selectInput ('varColor', NULL, c('blue', 'darkgreen', 'red',
                                                                             'cyan', 'violet', 'darkorange',
                                                                             'brown', 'black')),
                                            fluidRow (
                                              column (6, 'width:'),
                                              column (6, numericInput ('lineW', NULL, 1, width='90px'))),
                                            radioButtons ('lineStyle', label='line type', choices=c('solid',
                                                                                                    'dashed', 'dotted',
                                                                                                    'd-dot', 'lg dash'), inline=TRUE, width='90px')),
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
                                          checkboxInput ('limits6','apply restrictions'), 
                                          width=2),
                             
                             mainPanel(plotOutput (outputId='skewT')))
                 ),
                 tabPanel ('variance spectra',
                           sidebarLayout(
                             sidebarPanel(h4('spectral analysis definition'), 
                                          checkboxInput ('limits7','apply restrictions'), 
                                          width=2),
                             
                             mainPanel())
                 )
    )
  )
)



sidebarUni <- sidebarPanel(
  #fixed responsive img #added class img
  img(src="menfishing21.png", class = "img-responsive", align = 'middle'),
  
  hr(),
  
  bsCollapse(id = "collapseSidebar" ,  open = "Upload Data", multiple = FALSE,
            bsCollapsePanel("Upload Data", style = "primary", 
                radioButtons(inputId = 'ext', label = 'File extention', choices = c('xlsx', 'xls', 'csv'), 
                             selected = 'xlsx', inline = TRUE),
                hr(),
                checkboxInput(inputId = 'header', label = 'First line is a header', value = TRUE),
                hr(),
                radioButtons(inputId = 'th.sep', label = 'Thousand Separator', choices = c(Comma=',', Dot='.'), 
                             selected = ',', inline = TRUE),
                tags$hr(),
                fileInput('file', 'Choose Excel File...', accept = c('.xls', '.xlsx', '.csv')),
                tags$hr(),
                h5("Press to Upload Dataset..."),
                actionButton("action", label = "Action")
           ),  # end bsCollapsePanel Upload Data
           bsCollapsePanel("Dimensions", style = "primary",
              fluidRow(column(6,
                         uiOutput("dimSpecies"),
                         uiOutput("dimUnit"),
                         uiOutput("dimHatchery"),
                         uiOutput("dimOriginMonth"),
                         uiOutput("dimOriginYear"),
                         uiOutput("dimActualFeed"),
                         uiOutput("dimStartAvWtCat"),
                         uiOutput("dimEndAvWtCat")
                       ),
                       column(6,
                         uiOutput("dimRegion"),
                         uiOutput("dimSite"),
                         uiOutput("dimBatch"),
                         uiOutput("dimSamplMonth"),
                         uiOutput("dimSamplYear"),
                         uiOutput("dimSupplier"),
                         uiOutput("dimFeedCategory"),
                         uiOutput("dimFeedingPolicy")
                       ) # end column
                ), # end fluidRow
                fluidRow(    
                       uiOutput("dateRangeFrom"),
                       uiOutput("dateRangeTo")       
                ) # end fluidRow
           ), # end bsCollapsePanel Dimensions
           bsCollapsePanel('Measures', style = "primary" ,
              fluidRow(  uiOutput("rangeStAvWeight"), 
                         uiOutput("rangeEndAvWeight"),
                         uiOutput("rangeBiolPeriodFCR"),
                         uiOutput("rangeEconPeriodFCR") 
                       ),
              fluidRow(column(6,
                         uiOutput("rangePeriodSGR"),
                         uiOutput("rangePeriodTGC"),
                         uiOutput("rangeAvWtDeviation"),
                         uiOutput("rangeAvgTemp"),
                         uiOutput("rangeFeedDeviation"),
                         uiOutput("rangeLTDEconFCR")
                       ),
                       column(6,
					               uiOutput("rangePeriodSFR"),
					               uiOutput("rangeGrowthPerDay"),
					               uiOutput("rangePeriodMortalityPerc"),
                         uiOutput("rangeDiffDays"),
                         uiOutput("rangePeriodDayDegrees"),
					               uiOutput("rangeLTDMortalityPerc")
					             ) # end column
                ) # end fluid row
          ) # end of colapsePanel Measures
  ), # end bsCollapse

  hr(),
  actionButton(inputId = 'Go.Button',  label = 'Go...')

) # end sidebarUni function
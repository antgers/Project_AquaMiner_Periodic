### Version Periodic (Sampling To Sampling) Datasets
#
# Created: 30/08/2016
# Last Modified: 23/10/2016
#
# Author: Gerasimos Antzoulatos (i2s)
#

source("helpers.R")
source("SidebarUi.R")

#----------------------------------------------------
# 
shinyUI( 

  navbarPage( theme = "bootstrap.css", 
              img(src="Aquamanager-logo.png", class = "img-responsiveLogo"), #align = 'top',
#            "AquaSmart",
 
#---------------------------------------------------------- First MenuPage -- Descriptive Statistics
              tabPanel(" Descriptive Statistics ", id="MenuPage_1",
                       fluidPage( theme = "bootstrap.css",
                                  
                                  tags$head(tags$script(src="script2.js")),
                                  actionLink(inputId = "showHideButtonForMenu", label="", icon = icon("filter", lib = "glyphicon")),
                                  
                                  sidebarLayout(
                                  
                                    sidebarUni,
                                    mainPanel( 
                                      
                                     tabsetPanel(
                                        tabPanel("Dataset", 
                                            h4("Initial dataset:"),     
                                            DT::dataTableOutput("dataset"),
                                            hr(),
                                            h4("Final dataset:"),
                                            DT::dataTableOutput("preproc.dataset") #,
                                          #  downloadButton('downloadData', 'Download')
                                        ), # end tabPanel
                                        tabPanel("Line Plots", 
                                          fluidRow(
                                                  column(3, uiOutput("line.DimX")),
                                                  column(3, uiOutput("line.group.Batch")),
                                                  column(3, uiOutput("line.group.Unit")),
                                                  br(),
                                                  column(3, actionButton(inputId = 'View.Lines',  label = 'View...'))
                                          ), #end fluidRow
                                          br(),
                                          plotlyOutput("line_plots")
                                          
                                        ),
                                        tabPanel("Histogram", 
                                            fluidRow(
                                                   column(3,
                                                          sliderInput('numbins', 'Number of Bins:', 
                                                                      min=1, max=50, value=10, step=1) #,
                                                        #  checkboxInput('houtlier', 'Remove outliers')
                                                   ),
                                                   column(3, 
                                                          uiOutput("hist.group"),
                                                          conditionalPanel(
                                                              condition = "input.hgroup != 'None'",
                                                              checkboxInput('chkb.hfacet', 'Facets'))
                                                   ),
                                                   column(3, 
                                                          uiOutput("hist.measures")
                                                   ),
                                                   column(3, 
                                                          checkboxInput('saveHPlot', "Check to save"),
                                                          br(),
                                                          actionButton(inputId = 'View.Hist',  label = 'View...'),
                                                          br()
                                                   ) 
                                                 ),  # end fluidRow
                                                 br(),
                                                 uiOutput("hist_plots")
                                                 
                                                 #plotOutput("hist_plots", height = 800, width = 800)
                                                   
                                        ), # end tabPanel
                                        tabPanel("Density Plots", 
                                            fluidRow(
                                                   column(4,
                                                          uiOutput("dens.group"),
                                                          conditionalPanel(
                                                              condition = "input.dgroup != 'None'",
                                                              checkboxInput('chkb.dfacet', 'Facets')) #,
                                                        #  checkboxInput('doutlier', 'Remove outliers')
                                                   ),
                                                   column(4, 
                                                          uiOutput("dens.measures")
                                                   ),
                                                   column(4, 
                                                          checkboxInput('saveDPlot', "Check to save"),
                                                          actionButton(inputId = 'View.Dens',  label = 'View...') 
                                                   )
                                                 ),  # end fluidRow
                                                 br(),
                                                 #plotOutput("dens_plots", height = 800, width = 800)
                                                 uiOutput("dens_plots")  
                                          
                                        ), # end tabPanel
                                        tabPanel("Box Plots",
                                            fluidRow(     
                                                  column(4,
                                                         uiOutput("boxplot.group"),
                                                         conditionalPanel(
                                                              condition = "input.bxgroup != 'None'",
                                                              checkboxInput('chkb.bxfacet', 'Facets'))
                                                  ),
                                                  column(4, 
                                                         uiOutput("boxplot.measures"),
                                                         checkboxInput('chkb.bxnotch', 'Notches') #,
                                                      #   checkboxInput('bxoutlier', 'Remove outliers')
                                                  ),
                                                  column(4, 
                                                         checkboxInput('saveBoxPlot', "Check to save"),
                                                         actionButton(inputId = 'View.Boxplot',  label = 'View...')
                                                  )
                                             ),  # end fluidRow
                                             br(),
                                             #plotOutput("box_plots", height = 800, width = 800)         
                                             uiOutput("box_plots")    
                                        ), # end tabPanel
                                        tabPanel("Bar Plots",
                                            fluidRow(   
                                              column(3,
                                                     uiOutput("bar.dim"),
                                                     br()
                                              ),
                                              column(3,
                                                     uiOutput("bar.meas"),
                                                     conditionalPanel(
                                                       condition = "input.barMeas != 'None'",
                                                       radioButtons(inputId = 'radio.sd.se', label = 'Type of error bar', 
                                                                    choices = c('St.Dev', 'SEM'), 
                                                                    selected = 'St.Dev', inline = TRUE) #,
                                                       #checkboxInput('baroutlier', 'Remove outliers')
                                                      ),
                                                      br()
                                              ),
                                              column(3,
                                                     uiOutput("bar.group")
                                              ),
                                              column(3,
                                                    checkboxInput('saveBarPlot', "Check to save"),
                                                    actionButton(inputId = 'View.Barplots',  label = 'View...')
                                              ),
                                              br()
                                            ),  # end fluidRow
                                            #plotOutput("bar_plots", height = 800, width = 800) 
                                            uiOutput("bar_plots")
                                        ), # end tabPanel
                                        tabPanel("Scatter Plots",
                                                 fluidRow(     
                                                   column(3,
                                                          uiOutput("sc.dimX"),
                                                          radioButtons(inputId = 'method.regress', label = 'Regression line', 
                                                                       choices = c('None', 'lm', 'loess'),  
                                                                       selected = 'None', inline = TRUE ),
                                                          br()
                                                   ),
                                                   column(3,
                                                          uiOutput("sc.dimY"),
                                                          br()
                                                   ),
                                                   column(3,
                                                          uiOutput("sc.size"),
                                                          checkboxInput('saveScatterPlot', "Check to save"),
                                                          br()
                                                   ),
                                                   column(3,
                                                          uiOutput("sc.group"),
                                                          actionButton(inputId = 'View.Scatterplots', label = 'View...'),
                                                          br()
                                                   ),
                                                   br()
                                                 ),  # end fluidRow
                                                 plotlyOutput("scatter_plots", height = 600, width = 900) 
                                                   
                                        ), # end tabPanel
                                        tabPanel("Summary",
                                                 fluidRow(     
                                                   column(4,
                                                      uiOutput("sum.group")
                                                   ),
                                                   column(4, 
                                                      uiOutput("sum.meas")
                                                   ),
                                                   column(4, 
                                                      checkboxInput('saveStats', "Save to file"),
                                                      actionButton(inputId = 'View.Stats', label = 'View...')
                                                   ),
                                                   br()
                                                 ),  # end fluidRow
                                                 tableOutput("summary_stats")
                                        ) # end tabPanel
                                      ) # end tabsetPanel
                                 ) # end mainPanel
                                ) # end sidebarLayout
                              ) # end fluidPage
              ), # end tabPanel Descriptive Statistics

#---------------------------------------------------------- Second MenuPage --- OLAP Cubes                
              tabPanel("Pivot Table", 
                #     rpivotTable::rpivotTableOutput('pivTable', height = "800px") 
                     rpivotTableOutput("pivotTable", height = "800px") 
              ),            
#---------------------------------------------------------- Third MenuPage --- Machine Learning         
              tabPanel(" Machine Learning Models ", id="MenuPage_2", 
                       fluidPage( 
                         
                        # tags$head(tags$script(src="script2.js")),
                         actionLink(inputId = "showHideButtonForMachine", label="", icon = icon("filter", lib = "glyphicon")),
                         
                         sidebarLayout(
                         
                           sidebarPanel(
                             img(src="feedingfish1.png",class = "img-responsive", align = 'middle'),
                             hr(),
                             uiOutput("targs.ML.Variables"),
                             hr(),
                             uiOutput("preds.ML.Variables"),
                             hr(),
                             selectInput(inputId='radioML.model', label=h3("Choose ML model..."),
                                         choices=list("Support Vector Machine (RBF Kernel)"=1, 
                                                      "Generalized Linear Models (Boosted GLM)"=2,
                                                      "Generalized Additive Models (GAMs)"=3,
                                                      "Random Forest Regression"=4,
                                                      "Multivariate Adaptive Regression Spline (MARS)"=5), 
                                         selected=1, multiple=FALSE),
                             sliderInput("folds", "Folds:",min = 1, max = 20, value = 10),
                             hr(),
                             actionButton(inputId = 'goAnalysis',  label = 'Start Analysis')
                             
                           ),  # end sidebarPanel 
                           mainPanel(tabsetPanel( 
                                tabPanel("Build the Model...",
                                         br(),
                                         h4('Formula:'),
                                         fluidRow(column(12, verbatimTextOutput("fmla.model"))),
                                         hr(),
                                         
                                         # fluidRow(column(12, verbatimTextOutput("summary_model"))),
                                         # hr(),
                                         fluidRow(column(6, h4('RMSE:')), 
                                                  column(6, h4('R-Squared:'))
                                         ),       
                                         fluidRow(column(6, infoBoxOutput("approvalBox.RMSE")),
                                                  column(6, infoBoxOutput("approvalBox.Rsquare"))
                                         ), 
                                         hr(),
                                         #------ relative importance
                                         hr(),
                                         h4('Variable Importance:'),   
                                         fluidRow(column(12, plotlyOutput("plot_ML_Var_Impo",
                                                                          height = 600, width = 600))),
                                         fluidRow(column(12, verbatimTextOutput("ML.Var.Impo")))
                                ), # end tabPanel Training   
                                tabPanel("Evaluate the Training",
                                         fluidRow(column(4,
                                                    sliderInput("perc", "Percentage of population for testing:",
                                                                 min = 0, max = 100, value = 10, step=0.5)
                                                  ), # end column  
                                                  column(4,
                                                    sliderInput("thresh.RE", "Threshold of Relative Error:",
                                                                 min = 0, max = 100, value = c(0,10), step=0.5)
                                                  ), # end column
                                                  column(4,
                                                    checkboxInput('saveTesting', "Save to file"),
                                                    actionButton(inputId = 'ViewTesting',  label = 'View...')
                                                  ) # end column
                                         ), # end fluidRow
                                         plotlyOutput("plot_Testing",height = 800, width = 800),
                                         br(),
                                         h4('Evaluate the training ML model:'),
                                         fluidRow(column(12, verbatimTextOutput("evaluate_model")))
                                ), # end tabPanel Testing
                                tabPanel("Predict with it...",
                                         # predict response value using user-defined values for each predictor  
                                         fluidRow(column(6,   
                                                    h3("Set values to Predictors:"),
                                                    uiOutput("dyn_input.ML")
                                         ), 
                                         hr(),
                                         column(6, 
                                                    actionButton(inputId = 'goPrediction',  label = 'Start prediction'),
                                                    hr(),
                                                    h3("Prediction with Machine Learning model..."),
                                                    fluidRow(column(12, verbatimTextOutput("prediction.value.ML")))
                                         ) # end column
                                         ) # end fluidRow
                                ) # end tabPanel Predict
                              ) # end tabsetPanel
                           ) # end mainPanel
                         ) # end sidebarLayout
                       ) # end fluidPage
                  ),  # end tabPanel ML Models

#---------------------------------------------------------- Forth MenuPage --- Business Cases     
                  tabPanel(" KPIs Table Estimation ", id="MenuPage_3",
                          fluidPage(
                              sidebarPanel(
                                img(src="feedingfish1.png",class = "img-responsive", align = 'middle'),
                                hr(),
                                numericInput('temp.step', 'Step of Temperature:', 1,
                                             min = 1, max = 5, step = 0.5),
                                hr(),
                                numericInput('weight.step', 'Step of Weight Categories:', 50,
                                             min = 0, max = 800, step = 10),
                                hr(),
                                selectInput(inputId='radioKPI', label=h3("Choose the KPI..."),
                                            choices=list("Biological FCR"=1,
                                                         "Economical FCR"=2,
                                                         "SFR"=3,
                                                         "SGR"=4,
                                                         "Mortality %"=5
                                                        ),
                                            selected=1, multiple=FALSE)
                                
                              ),  # end sidebarPanel
                              mainPanel(tabsetPanel( 
                                  tabPanel("Cross-Tabular", 
                                           fluidRow(column(10, h4(" KPIs Table estimation by Machine Learning modeling:")),
                                                    column(2, actionButton(inputId = 'ViewKPITable',  label = 'View KPI Table'))
                                           ),
                                           tableOutput("KPI_Table")
                                  ), # end tabPanel Cross-Tabular
                                  tabPanel("2D", 
                                           fluidRow(column(6, uiOutput("CatAvWt")),
                                                    column(6, actionButton(inputId = 'View2D',  label = 'View 2D'))
                                           ),
                                           plotlyOutput("plot_2D_Table")
                                  ), # end tabPanel 2D
                                  tabPanel("3D",
                                           fluidRow(column(3, actionButton(inputId = 'View3D',  label = 'View 3D'))
                                           ),
                                           plotlyOutput("plot_3D_Table",height = 800, width = 800)
                                  ) # end tabPanel 3D
                                ) # end tabSetPanel
                              ) # end mainPanel
                          ) # end fluidPage
                  ),  # end tabPanel Business Cases

#---------------------------------------------------------- Fifth MenuPage --- Business Cases     
                  tabPanel(" Methodological Approach ", id="MenuPage_4",
                           fluidPage(

                               wellPanel(

                                 # h2(" Knowledge Discovery & Data Mining Process: "),
                                 img(src="KDD_DM_process.png", class = "img-responsive", align = 'middle', width = 800)

                              ) # end wellPanel
                      ) # end fluidPage
                  ),  # end tabPanel Business Cases

#---------------------------------------------------------- Sixth MenuPage --- Business Cases     
                  tabPanel(" About ", id="MenuPage_5", 
                           fluidPage( 
                            
                               plotOutput("plot.buzzWords")
                               
                           ) # end fluidPage
                  )  # end tabPanel Business Cases 

    ) # end navbarPage
) # end shinyUI  



                                      
                                                   

                                       
                                                   
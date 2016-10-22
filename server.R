### Version Periodic (Sampling To Sampling) Datasets
#
# Created: 09/06/2016
# Last Modified: 31/08/2016
#
# Author: Gerasimos Antzoulatos (i2s)
#
# This is the server logic for a Shiny web application.

#---------------------------------------------------------------------------------- shinyServer.......
#
shinyServer(function(input, output, session){
  
  #---------------------------------------------------------------------------------------------------
  #     Import the Dataset 
  #---------------------------------------------------------------------------------------------------
  Inpt.Dataset <- reactive({ 

    inFile <- input$file
    
    if (is.null(inFile)){
       return(NULL)
    }
    
    # Read from the input file
    newpath.filename <- paste(inFile$datapath, input$ext, sep=".")
    file.rename(inFile$datapath, newpath.filename)
    if ( input$ext == 'csv' )
    {
        dset <- read.csv(newpath.filename, header = input$header, sep = ';', dec = '.')
    }
    else{
        dset <- read_excel(newpath.filename, sheet = 1, col_names = input$header)
    }
    
    new.names <- names(dset)
    new.names <- gsub("[:(:]No[:):]", "No", new.names)
    new.names <- gsub("[:(:]Kg[:):]", "Kg", new.names)
    new.names <- gsub("[:(:][:%:][:):]", "Perc", new.names)
    new.names <- gsub("[:%:]", "Perc", new.names)
    new.names <- gsub("[:(:]Kg [:/:] Hour[:):]","Kg.per.Hour", new.names)
    new.names <- gsub("[:+:]","Plus", new.names)
    new.names <- gsub("-", "Minus", new.names, fixed=TRUE)
    new.names <- gsub(" ", ".",new.names)
    new.names <- gsub("..", ".", new.names, fixed=TRUE)
    new.names <- gsub("_", ".", new.names, fixed=TRUE)
    
    colnames(dset) <- new.names
    data <- create.dataset(dset)
 
    write.csv(data, file = "MyData.csv",row.names=FALSE, sep=";", col.names = TRUE)
    
    return(data)
    
  })
  
  #---------------------------------------------------------------------------------------------------
  #     Subset of Dataset
  #---------------------------------------------------------------------------------------------------
  passDataDim <- reactive({
    
    data <- Inpt.Dataset()
    
    if (input$dimRegion != "All"){
      data <- subset(data, Region %in% c(input$dimRegion))
    }
    if (input$dimSpecies != "All"){
      data <- subset(data, Species %in% c(input$dimSpecies))
    }
    if (input$dimSite != "All"){
      data <- subset(data, Site %in% c(input$dimSite))
    }
    if (input$dimBatch != "All"){
      data <- subset(data, Batch %in% c(input$dimBatch))
    }
    if (input$dimUnit != "All"){
      data <- subset(data, Unit %in% c(input$dimUnit))
    }
    if (input$dimHatchery != "All"){
      data <- subset(data, Hatchery %in% c(input$dimHatchery))
    }
    if (input$dimOriginMonth != "All"){
      data <- subset(data, Origin.Month %in% c(input$dimOriginMonth))
    }
    if (input$dimOriginYear != "All"){
      data <- subset(data, Origin.Year %in% c(input$dimOriginYear))
    }
    if (input$dimSamplMonth != "All"){
      data <- subset(data, From.Month %in% c(input$dimSamplMonth))
    }
    if (input$dimSamplYear != "All"){
      data <- subset(data, From.Year %in% c(input$dimSamplYear))
    }
    if (input$dimActualFeed != "All"){
      data <- subset(data, Actual.Feed %in% c(input$dimActualFeed))
    }
    if (input$dimFeedCategory != "All"){
      data <- subset(data, Feed.Category %in% c(input$dimFeedCategory))
    }
    if (input$dimSupplier != "All"){
      data <- subset(data, Supplier %in% c(input$dimSupplier))
    }
    if (input$dimFeedingPolicy != "All"){
      data <- subset(data, Feeding.Policy %in% c(input$dimFeedingPolicy))
    }
    if (input$dimStartAvWtCat != "All"){
      data <- subset(data, Start.Av.Weight.Category %in% c(input$dimStartAvWtCat))
    }
    if (input$dimEndAvWtCat != "All"){
      data <- subset(data, End.Av.Weight.Category %in% c(input$dimEndAvWtCat))
    }
    
    return(data)
  })
  
  #----------------------------
  passData <- reactive({

    data <- Inpt.Dataset()
    
    if (input$dimRegion != "All"){
      data <- subset(data, Region %in% c(input$dimRegion))
    }
    if (input$dimSpecies != "All"){
      data <- subset(data, Species %in% c(input$dimSpecies))
    }
    if (input$dimSite != "All"){
      data <- subset(data, Site %in% c(input$dimSite))
    }
    if (input$dimBatch != "All"){
      data <- subset(data, Batch %in% c(input$dimBatch))
    }
    if (input$dimUnit != "All"){
      data <- subset(data, Unit %in% c(input$dimUnit))
    }
    if (input$dimHatchery != "All"){
      data <- subset(data, Hatchery %in% c(input$dimHatchery))
    }
    if (input$dimOriginMonth != "All"){
      data <- subset(data, Origin.Month %in% c(input$dimOriginMonth))
    }
    if (input$dimOriginYear != "All"){
      data <- subset(data, Origin.Year %in% c(input$dimOriginYear))
    }
    if (input$dimSamplMonth != "All"){
      data <- subset(data, From.Month %in% c(input$dimSamplMonth))
    }
    if (input$dimSamplYear != "All"){
      data <- subset(data, From.Year %in% c(input$dimSamplYear))
    }
    if (input$dimActualFeed != "All"){
      data <- subset(data, Actual.Feed %in% c(input$dimActualFeed))
    }
    if (input$dimFeedCategory != "All"){
      data <- subset(data, Feed.Category %in% c(input$dimFeedCategory))
    }
    if (input$dimFeedingPolicy != "All"){
      data <- subset(data, Feeding.Policy %in% c(input$dimFeedingPolicy))
    }
    if (input$dimSupplier != "All"){
      data <- subset(data, Supplier %in% c(input$dimSupplier))
    }
    if (input$dimStartAvWtCat != "All"){
      data <- subset(data, Start.Av.Weight.Category %in% c(input$dimStartAvWtCat))
    }
    if (input$dimEndAvWtCat != "All"){
      data <- subset(data, End.Av.Weight.Category %in% c(input$dimEndAvWtCat))
    }
  
    data <- data[ data$Start.Av.Weight >= as.numeric(input$rangeStAvWeight[1]) &
                  data$Start.Av.Weight <= as.numeric(input$rangeStAvWeight[2]) &
                  data$End.Av.Weight >= as.numeric(input$rangeEndAvWeight[1]) &
                  data$End.Av.Weight <= as.numeric(input$rangeEndAvWeight[2]) &
                  data$Econ.FCR.Period >= as.numeric(input$rangeEconPeriodFCR[1]) &
                  data$Econ.FCR.Period <= as.numeric(input$rangeEconPeriodFCR[2]) &
                  data$Biol.FCR.Period >= as.numeric(input$rangeBiolPeriodFCR[1]) &
                  data$Biol.FCR.Period <= as.numeric(input$rangeBiolPeriodFCR[2]) &
                  data$SGR.Period.Perc >= as.numeric(input$rangePeriodSGR[1]) &
                  data$SGR.Period.Perc <= as.numeric(input$rangePeriodSGR[2]) &
                  data$SFR.Period.Perc >= as.numeric(input$rangePeriodSFR[1]) &
                  data$SFR.Period.Perc <= as.numeric(input$rangePeriodSFR[2]) &
                  data$TGC.Period >= as.numeric(input$rangePeriodTGC[1]) &
                  data$TGC.Period <= as.numeric(input$rangePeriodTGC[2]) &
                  data$Growth.Per.Day >= as.numeric(input$rangeGrowthPerDay[1]) &
                  data$Growth.Per.Day <= as.numeric(input$rangeGrowthPerDay[2]) &
                  data$Mortality.Perc >= as.numeric(input$rangePeriodMortalityPerc[1]) &
                  data$Mortality.Perc <= as.numeric(input$rangePeriodMortalityPerc[2]) &
                  data$Diff.Days >= as.numeric(input$rangeDiffDays[1]) &
                  data$Diff.Days <= as.numeric(input$rangeDiffDays[2]) &
                  data$Period.Day.Degrees >= as.numeric(input$rangePeriodDayDegrees[1]) &
                  data$Period.Day.Degrees <= as.numeric(input$rangePeriodDayDegrees[2]) &
                  data$Av.Wt.Deviation.Perc >= as.numeric(input$rangeAvWtDeviation[1]) &
                  data$Av.Wt.Deviation.Perc <= as.numeric(input$rangeAvWtDeviation[2]) &
                  data$Feed.Deviation.Perc  >= as.numeric(input$rangeFeedDeviation[1]) &
                  data$Feed.Deviation.Perc  <= as.numeric(input$rangeFeedDeviation[2]) & 
                  data$Avg.Temp >= as.numeric(input$rangeAvgTemp[1]) &
                  data$Avg.Temp <= as.numeric(input$rangeAvgTemp[2]) &
                  data$LTD.Econ.FCR >= as.numeric(input$rangeLTDEconFCR[1]) &  
                  data$LTD.Econ.FCR <= as.numeric(input$rangeLTDEconFCR[2]) &
                  data$LTD.Mortality.Perc >= as.numeric(input$rangeLTDMortalityPerc[1]) &  
                  data$LTD.Mortality.Perc <= as.numeric(input$rangeLTDMortalityPerc[2]) &  
                  (data$From >= ymd(input$dateRangeFrom[1]) & data$From <= ymd(input$dateRangeFrom[2])) & 
                  (data$To >= ymd(input$dateRangeTo[1]) & data$To <= ymd(input$dateRangeTo[2]))
               , ] 

    return(data)
  })
  
  #---------------------------------------------------------------------------------------------------
  #     Download (Save) the Dataset
  #---------------------------------------------------------------------------------------------------
  output$downloadData <- downloadHandler(
    
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() { paste("New", input$file, sep="_") },
        
      # This function should write data to a file given to it by
      # the argument 'file'.
            
      content = function(file){
              
          if (input$Go.Button == 0){
                return() }
          else{
              isolate({
                datasetInput <- passData()
                fname <- paste(file,"xlsx",sep=".")
                wb <- loadWorkbook(fname, create = TRUE)
                createSheet(wb, name = "Sheet1")
                writeWorksheet(wb, datasetInput, sheet = "Sheet1") # writes the data in file
                saveWorkbook(wb)
                file.rename(fname,file)
              }) # end isolate
          } # end if..else 
      } # end content

  )  # end downloadHandler
  
  
  #=================================================================================================
  #   Dynamic Sidebars
  #
  # ---- Dimensions
  
  output$dimRegion <- renderUI({
      data <- Inpt.Dataset()
      selectInput(inputId='dimRegion', label='Region',
                  choices=c("All", sort(unique(as.character(data$Region)))), selected="All", multiple=TRUE)
  })
  output$dimSpecies <- renderUI({
      data <- Inpt.Dataset()
      selectInput(inputId='dimSpecies', label='Species',
                choices=c("All", sort(unique(as.character(data$Species)))), selected="All", multiple=TRUE)
  })
  output$dimSite <- renderUI({
      data <- Inpt.Dataset()
      selectInput(inputId='dimSite', label='Site', choices=c("All", sort(unique(as.character(data$Site)))),
                  selected="All", multiple=TRUE)
  })
  output$dimUnit <- renderUI({
      data <- Inpt.Dataset()
      selectInput(inputId='dimUnit', label='Unit', choices=c("All", sort(unique(as.character(data$Unit)))),
                  selected="All", multiple=TRUE)
  })
  output$dimBatch <- renderUI({
      data <- Inpt.Dataset()
      selectInput(inputId='dimBatch', label='Batch', choices=c("All", sort(unique(as.character(data$Batch)))),
                  selected="All", multiple=TRUE)
  })
  output$dimHatchery <- renderUI({
      data <- Inpt.Dataset()
      selectInput(inputId='dimHatchery', label='Hatchery', choices=c("All", sort(unique(as.character(data$Hatchery)))),
                  selected="All", multiple=TRUE)
  })
  output$dimOriginMonth <- renderUI({
    data <- Inpt.Dataset()
    origin.months <- factor(unique(as.character(data$Origin.Month)), levels = month.name)
    selectInput(inputId='dimOriginMonth', label='Origin.Month',
                choices=c("All", levels(origin.months)), selected="All", multiple=TRUE)
  })
  output$dimOriginYear <- renderUI({
    data <- Inpt.Dataset()
    selectInput(inputId='dimOriginYear', label='Origin.Year',
                choices=c("All", sort(unique(as.character(data$Origin.Year)))), selected="All", multiple=TRUE)
  })
  output$dimSamplMonth <- renderUI({
    data <- Inpt.Dataset()
    from.months <- factor(unique(as.character(data$From.Month)), levels = month.name)
    selectInput(inputId='dimSamplMonth', label='Sampling.Month',
                choices=c("All", levels(from.months)), selected="All", multiple=TRUE)
  })
  output$dimSamplYear <- renderUI({
    data <- Inpt.Dataset()
    selectInput(inputId='dimSamplYear', label='Sampling.Year',
                choices=c("All", sort(unique(as.character(data$From.Year)))), selected="All", multiple=TRUE)
  })
  output$dimActualFeed <- renderUI({
    data <- Inpt.Dataset()
    selectInput(inputId='dimActualFeed', label='Actual.Feed',
                choices=c("All", sort(unique(as.character(data$Actual.Feed)))), selected="All", multiple=TRUE)
  })
  output$dimFeedCategory <- renderUI({
    data <- Inpt.Dataset()
    selectInput(inputId='dimFeedCategory', label='Feed.Category',
                choices=c("All", sort(unique(as.character(data$Feed.Category)))), selected="All", multiple=TRUE)
  })
  output$dimFeedingPolicy <- renderUI({
    data <- Inpt.Dataset()
    selectInput(inputId='dimFeedingPolicy', label='Feeding.Policy',
                choices=c("All", sort(unique(as.character(data$Feeding.Policy)))), selected="All", multiple=TRUE)
  })
  output$dimSupplier <- renderUI({
    data <- Inpt.Dataset()
    selectInput(inputId='dimSupplier', label='Supplier',
                choices=c("All", sort(unique(as.character(data$Supplier)))), selected="All", multiple=TRUE)
  })
  output$dimStartAvWtCat <- renderUI({
    data <- Inpt.Dataset()
    selectInput(inputId='dimStartAvWtCat', label='Start.Av.Weight.Category',
                choices=c("All", sort(unique(as.character(data$'Start.Av.Weight.Category')))), 
                      selected="All", multiple=TRUE)
  })
  output$dimEndAvWtCat <- renderUI({
    data <- Inpt.Dataset()
    selectInput(inputId='dimEndAvWtCat', label='End.Av.Weight.Category',
                choices=c("All", sort(unique(as.character(data$'End.Av.Weight.Category')))), 
                selected="All", multiple=TRUE)
  })
  output$dateRangeFrom <- renderUI({
    data <- Inpt.Dataset()
    dateRangeInput('dateRangeFrom',
                   label = paste(' Start Date (From): '),
                   start = min( ymd(data$From)-days(0) ), 
                   end = max( ymd(data$From)+days(1) ),
                   min = min( ymd(data$From)-days(0) ),
                   max = max( ymd(data$From)+days(1)),
                   separator = " to ", format = "dd/mm/yyyy",
                   startview = 'year', language = 'en', weekstart = 0
    )
  })
  output$dateRangeTo <- renderUI({
    data <- Inpt.Dataset()
    dateRangeInput('dateRangeTo',
                   label = paste(' End Date (To): '),
                   start = min( ymd(data$To)-days(1) ), 
                   end = max( ymd(data$To)+days(1) ),
                   min = min( ymd(data$To)-days(1) ),
                   max = max( ymd(data$To)+days(1) ),
                   separator = " to ", format = "dd/mm/yyyy",
                   startview = 'year', language = 'en', weekstart = 0
    )
  })
  # ---- Measures
   output$rangeStAvWeight  <- renderUI({
     data <- passDataDim()
     sliderInput("rangeStAvWeight", "Start.Av.Weight:",
                 min = min(as.double(data$Start.Av.Weight), na.rm=TRUE),
                 max = max(as.double(data$Start.Av.Weight), na.rm=TRUE),
                 value = c(min(as.double(data$Start.Av.Weight), na.rm=TRUE),
                           max(as.double(data$Start.Av.Weight), na.rm=TRUE)),
                 step=1, round=-2)
   })
   output$rangeEndAvWeight  <- renderUI({    
     data <- passDataDim()  
     sliderInput("rangeEndAvWeight", "End.Av.Weight:",
                  min = min(as.double(data$End.Av.Weight), na.rm=TRUE),
                  max = max(as.double(data$End.Av.Weight), na.rm=TRUE),
                  value = c(min(as.double(data$End.Av.Weight), na.rm=TRUE),
                            max(as.double(data$End.Av.Weight), na.rm=TRUE)),
                  step=1, round=-2)
   })
   output$rangeEconPeriodFCR <- renderUI({
     data <- passDataDim()  
     sliderInput("rangeEconPeriodFCR", "Econ.FCR.Period:",
                 min = min(as.double(data$Econ.FCR.Period), na.rm=TRUE),
                 max = max(as.double(data$Econ.FCR.Period), na.rm=TRUE),
                 value = c(min(as.double(data$Econ.FCR.Period), na.rm=TRUE),
                           max(as.double(data$Econ.FCR.Period), na.rm=TRUE)),
                 step=0.1, round=-2)
   })
   output$rangeBiolPeriodFCR <- renderUI({
     data <- passDataDim()  
     sliderInput("rangeBiolPeriodFCR", "Biol.FCR.Period:",
                 min = min(as.double(data$Biol.FCR.Period), na.rm=TRUE),
                 max = max(as.double(data$Biol.FCR.Period), na.rm=TRUE),
                 value = c(min(as.double(data$Biol.FCR.Period), na.rm=TRUE),
                           max(as.double(data$Biol.FCR.Period), na.rm=TRUE)),
                 step=0.1, round=-2)
   })
   output$rangePeriodSGR  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangePeriodSGR", "SGR.Period.Perc:",
                 min = min(as.double(data$SGR.Period.Perc), na.rm=TRUE),
                 max = max(as.double(data$SGR.Period.Perc), na.rm=TRUE),
                 value = c(min(as.double(data$SGR.Period.Perc), na.rm=TRUE),
                           max(as.double(data$SGR.Period.Perc), na.rm=TRUE)),
                 step=0.1, round=-2)
   })
   output$rangePeriodSFR  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangePeriodSFR", "SFR.Period.Perc:",
                 min = min(as.double(data$SFR.Period.Perc), na.rm=TRUE),
                 max = max(as.double(data$SFR.Period.Perc), na.rm=TRUE),
                 value = c(min(as.double(data$SFR.Period.Perc), na.rm=TRUE),
                           max(as.double(data$SFR.Period.Perc), na.rm=TRUE)),
                 step=0.1, round=-2)
   })
   output$rangePeriodTGC  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangePeriodTGC", "TGC.Period:",
                 min = min(as.double(data$TGC.Period), na.rm=TRUE),
                 max = max(as.double(data$TGC.Period), na.rm=TRUE),
                 value = c(min(as.double(data$TGC.Period), na.rm=TRUE),
                           max(as.double(data$TGC.Period), na.rm=TRUE)),
                 step=0.1, round=-2)
   })
   output$rangeGrowthPerDay  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangeGrowthPerDay", "Growth.Per.Day:",
                 min = min(as.double(data$Growth.Per.Day), na.rm=TRUE),
                 max = max(as.double(data$Growth.Per.Day), na.rm=TRUE),
                 value = c(min(as.double(data$Growth.Per.Day), na.rm=TRUE),
                           max(as.double(data$Growth.Per.Day), na.rm=TRUE)),
                 step=0.1, round=-2)
   })
   output$rangePeriodMortalityPerc  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangePeriodMortalityPerc", "Mortality.Perc:",
                 min = min(as.double(data$Mortality.Perc), na.rm=TRUE),
                 max = max(as.double(data$Mortality.Perc), na.rm=TRUE),
                 value = c(min(as.double(data$Mortality.Perc), na.rm=TRUE),
                           max(as.double(data$Mortality.Perc), na.rm=TRUE)),
                 step=0.1, round=-2)
  })
  output$rangeDiffDays  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangeDiffDays", "Diff.Days:",
                 min = min(as.double(data$Diff.Days), na.rm=TRUE),
                 max = max(as.double(data$Diff.Days), na.rm=TRUE),
                 value = c(min(as.double(data$Diff.Days), na.rm=TRUE),
                           max(as.double(data$Diff.Days), na.rm=TRUE)),
                 step=1, round=-2)
   })
   output$rangePeriodDayDegrees  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangePeriodDayDegrees", "Period.Day.Degrees:",
                 min = min(as.double(data$Period.Day.Degrees), na.rm=TRUE),
                 max = max(as.double(data$Period.Day.Degrees), na.rm=TRUE),
                 value = c(min(as.double(data$Period.Day.Degrees), na.rm=TRUE),
                           max(as.double(data$Period.Day.Degrees), na.rm=TRUE)),
                 step=10, round=-2)
   })
   output$rangeAvWtDeviation  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangeAvWtDeviation", "Av.Wt.Deviation.Perc:",
                 min = min(as.double(data$Av.Wt.Deviation.Perc), na.rm=TRUE),
                 max = max(as.double(data$Av.Wt.Deviation.Perc), na.rm=TRUE),
                 value = c(min(as.double(data$Av.Wt.Deviation.Perc), na.rm=TRUE),
                           max(as.double(data$Av.Wt.Deviation.Perc), na.rm=TRUE)),
                 step=0.5, round=-2)
   })
   output$rangeFeedDeviation  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangeFeedDeviation", "Feed.Deviation.Perc:",
                 min = min(as.double(data$Feed.Deviation.Perc), na.rm=TRUE),
                 max = max(as.double(data$Feed.Deviation.Perc), na.rm=TRUE),
                 value = c(min(as.double(data$Feed.Deviation.Perc), na.rm=TRUE),
                           max(as.double(data$Feed.Deviation.Perc), na.rm=TRUE)),
                 step=0.5, round=-2)
   })
   output$rangeAvgTemp  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangeAvgTemp", "Avg.Temp:",
                 min = min(as.double(data$Avg.Temp), na.rm=TRUE),
                 max = max(as.double(data$Avg.Temp), na.rm=TRUE),
                 value = c(min(as.double(data$Avg.Temp), na.rm=TRUE),
                           max(as.double(data$Avg.Temp), na.rm=TRUE)),
                 step=0.1, round=-2)
   })
   output$rangeLTDEconFCR  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangeLTDEconFCR", "LTD.Econ.FCR:",
                 min = min(as.double(data$LTD.Econ.FCR), na.rm=TRUE),
                 max = max(as.double(data$LTD.Econ.FCR), na.rm=TRUE),
                 value = c(min(as.double(data$LTD.Econ.FCR), na.rm=TRUE),
                           max(as.double(data$LTD.Econ.FCR), na.rm=TRUE)),
                 step=0.1, round=-2)
   })
   output$rangeLTDMortalityPerc  <- renderUI({
     data <- passDataDim()  
     sliderInput("rangeLTDMortalityPerc", "LTD.Mortality.Perc:",
                 min = min(as.double(data$LTD.Mortality.Perc), na.rm=TRUE),
                 max = max(as.double(data$LTD.Mortality.Perc), na.rm=TRUE),
                 value = c(min(as.double(data$LTD.Mortality.Perc), na.rm=TRUE),
                           max(as.double(data$LTD.Mortality.Perc), na.rm=TRUE)),
                 step=0.1, round=-2)
   })
   
   
  #=================================================================================================
  #
  # Initial (0) Tab functions -- Datasets
  #
  #
  output$dataset <- DT::renderDataTable({
    if (input$action == 0){
      return() }
    else{ 
      isolate({
        data <- Inpt.Dataset()
        DT::datatable(data, class='compact', rowname = TRUE, caption="Dataset for processing...",
                      filter = 'top', extensions = 'FixedColumns',
                      options = list(autoWidth=TRUE, scrollX = TRUE, 
                                     fixedColumns = list(leftColumns = 6)),
                      selection = list(mode = 'multiple', target = 'row') )
      })
    }
  })
  
  output$preproc.dataset<- DT::renderDataTable({
    if (input$Go.Button == 0){
      return() }
    else{
      isolate({
        data <- passData()
        DT::datatable(data, class='compact', rowname = TRUE, caption="Dataset after processing...",
                      filter = 'top', extensions = 'FixedColumns',
                      options = list(autoWidth=TRUE, scrollX = TRUE, 
                                     fixedColumns = list(leftColumns = 6)),
                      selection = list(mode = 'multiple', target = 'row'))
					  
      })
    }
  })

  
  #=================================================================================================
  #
  # First Tab functions -- Line Plots
  #
  output$line.DimX <- renderUI({
    selectInput('lineDimX', 'Dimension (X-axis):', choices=c("From", "To"), selected = "From", multiple = FALSE)
  })
  
  output$line.group.Batch <- renderUI({
    data <- passData()
    value.batch <- sort(unique(as.character(data$Batch)))
    selectInput('linegroupBatch', 'Batch:', choices=c("All", value.batch), selected = "All", 
                multiple = TRUE)
  })
  
  output$line.group.Unit <- renderUI({
    data <- passData()
    if (input$linegroupBatch != "All"){
      data <- subset(data, Batch %in% c(input$linegroupBatch))
    }
    
    value.unit <- sort(unique(as.character(data$Unit)))
    selectInput('linegroupUnit', 'Unit:', choices=c("All", value.unit), selected = "All", 
                multiple = TRUE)
  })
  
  output$line_plots <- renderPlotly({
      
    #  Re-run when button is clicked
    # if (input$View.Lines == 0){
    #   return() }
    # else{
    #   isolate({
        
        batch.val <- input$linegroupBatch 
        unit.val <- input$linegroupUnit
        dim.x <- input$lineDimX
        
        data <- passData()
        
        ds <- data.frame(data$From, data$To, data$Batch, data$Unit, data$Biol.FCR.Period, data$Econ.FCR.Period, 
                         data$SFR.Period.Perc, data$SGR.Period.Perc,  data$TGC.Period)
        colnames(ds) <- c("From", "To", "Batch", "Unit", "Biol.FCR.Period", "Econ.FCR.Period", "SFR.Period.Perc", 
                          "SGR.Period.Perc", "TGC.Period")
        
        if ( (batch.val == "All") & (unit.val == "All") ){
            ds <- ds[order(as.Date(ds[,dim.x])) , ]
        }else if ( (batch.val != "All") & (unit.val == "All") ){ 
            ds <- ds[order(as.Date(ds[,dim.x])) , ] 
            ds <- subset(ds, Batch %in% c(batch.val))
        }else if ( (unit.val != "All") & (batch.val == "All") ){ 
            ds <- ds[order(as.Date(ds[,dim.x])) , ]
            ds <- subset(ds, Unit %in% c(unit.val))
        }else if ( (batch.val != "All") & (unit.val != "All") ){
            ds <- ds[order(as.Date(ds[,dim.x])) , ]
            ds <- subset(ds, Batch %in% c(batch.val))
        }
      
        p <- plot_ly(ds, x = ds[,dim.x], y = Biol.FCR.Period, mode = "lines + markers", name = "Biol.FCR.Period") %>% 
                  add_trace(x = ds[,dim.x], y = Econ.FCR.Period, name = "Econ.FCR.Period") %>% 
                  add_trace(x = ds[,dim.x], y = SFR.Period.Perc, name = "SFR.Period.Perc") %>% 
                  add_trace(x = ds[,dim.x], y = SGR.Period.Perc, name = "SGR.Period.Perc") %>% 
                  add_trace(x = ds[,dim.x], y = TGC.Period, name = "TGC.Period") %>% 
                  layout(
                  #  title = " Period Econ.FCR vs SFR and SGR ", 
                    xaxis = list(
                      rangeselector = list(
                        buttons = list(
                          list(
                            count = 3, 
                            label = "3 mo", 
                            step = "month",
                            stepmode = "backward"),
                          list(
                            count = 6, 
                            label = "6 mo", 
                            step = "month",
                            stepmode = "backward"),
                          list(
                            count = 1, 
                            label = "1 yr", 
                            step = "year",
                            stepmode = "backward"),
                          list(
                            count = 1, 
                            label = "YTD", 
                            step = "year",
                            stepmode = "todate"),
                          list(step = "all"))),
                      
                      rangeslider = list(type = "date"), title="Date" ),
                    
                    yaxis = list(title = "KPIs"))
        
          p
        
    #   })
    # } 
  })   
  
  #=================================================================================================
  #
  # Second Tab functions -- Histograms
  #
  output$hist.group <- renderUI({
    data <- passData()
    ncat <- which(sapply(data, is.factor))
    data.cat <- data[, ncat]
    group.vars <- names(data.cat) 
    selectInput('hgroup', 'Filter:', choices=c("None", group.vars), multiple = FALSE)
  })
  #
  output$hist.measures <- renderUI({
    data <- passData()
    meas.vars <- c("Start.Av.Weight", "End.Av.Weight", "Econ.FCR.Period", "Mortality.Perc", "Mortality.Deviation.Perc",
                   "SFR.Period.Perc", "SGR.Period.Perc", "Avg.Temp", "Av.Wt.Deviation.Perc", "Diff.Days",
                   "LTD.Mortality.Perc", "LTD.Econ.FCR")
    selectInput('hmeasure', 'Measure (x-axis):', choices=meas.vars, multiple = TRUE, selected = "Econ.FCR.Period")
  })
  #
  # output$hist_plots <- renderPlot({
  # #  Re-run when button is clicked
  #     if (input$View.Hist == 0){
  #       return() }
  #     else{
  #       isolate({
  #         graphData <- passData()
  #             theGraph <- fun.histPlot(graphData,no.bins=input$numbins,meas=input$hmeasure,group_var=input$hgroup,
  #                                     flag.facet = input$chkb.hfacet, flag.save=input$saveHPlot)
  #         print(theGraph)
  #       })
  #     }
  # })
  
  #------------  Multiple histograms
  #
  multiHistPlotsOutput <- reactive({
    hmeas <- input$hmeasure
    n_plot <- length(hmeas)

    graphData <- passData()
    
    # lapply(1:n_plot, function(i){
    #           output[[paste("plot", i, sep="") ]] <- renderPlot({
    #                     theGraph <- fun.histPlot(graphData, no.bins=input$numbins,
    #                                              meas=hmeas[i], group_var=input$hgroup,
    #                                              flag.facet = input$chkb.hfacet, 
    #                                              flag.save=input$saveHPlot) 
    #                     print(theGraph)
    #           }) # end renderPlot()
    # }) # end lapply
    
  #  par(mar=c(4,3,3,2), oma=c(0,0,0,0))
    
    lapply(1:n_plot, function(i){
              output[[paste("plot", i, sep="") ]] <- renderPlotly({
                        theGraph <- fun.histPlot(graphData, no.bins=input$numbins,
                                                 meas=hmeas[i], group_var=input$hgroup,
                                                 flag.facet = input$chkb.hfacet,  
                                                 flag.save=input$saveHPlot)
                        
                        # Convert the ggplot to a plotly
                        theGraph <- ggplotly(theGraph)
                        
              }) # end renderPlot()
    }) # end lapply
  })     
   
  ##### Create divs ######
  output$hist_plots <- renderUI({
    #  Re-run when button is clicked
        if (input$View.Hist == 0){
          return() }
        else{
          isolate({
                  n_plot <- length(input$hmeasure)
                  plot_output_list <- lapply(1:n_plot, function(i) {
                            plotname <- paste("plot", i, sep="")
                            #plotOutput(plotname)
                            plotlyOutput(plotname, height = 800, width = 800)
                  })   
                  do.call(tagList, plot_output_list)
                  multiHistPlotsOutput()
          })
        } 
  })   
  
  #=================================================================================================
  #
  # Third Tab functions -- Density Plots
  #
  output$dens.group <- renderUI({
    data <- passData()
    ncat <- which(sapply(data, is.factor))
    data.cat <- data[, ncat]
    group.vars <- names(data.cat) 
    selectInput('dgroup', 'Filter:', choices=c("None", group.vars), multiple = FALSE)
  })
  #
  output$dens.measures <- renderUI({
    data <- passData()
    meas.vars <- c("Start.Av.Weight", "End.Av.Weight", "Econ.FCR.Period", "Mortality.Perc", "Mortality.Deviation.Perc",
                   "SFR.Period.Perc", "SGR.Period.Perc", "Avg.Temp", "Av.Wt.Deviation.Perc", "Diff.Days",
                   "LTD.Mortality.Perc", "LTD.Econ.FCR")
    selectInput('dmeasure', 'Measure (x-axis):', choices = sort(meas.vars), multiple = TRUE, 
                selected = "Econ.FCR.Period")
  })
  #
  # output$dens_plots <- renderPlot({
  #   #  Re-run when button is clicked
  #   if (input$View.Dens == 0){
  #     return() }
  #   else{
  #     isolate({
  #       graphData <- passData()
  #       theGraph <- fun.densPlot(graphData, meas=input$dmeasure, group_var=input$dgroup,
  #                                flag.facet = input$chkb.dfacet, flag.save=input$saveDPlot)
  #       print(theGraph)
  #     })
  #   }
  # })
  
  # Multiple Density plots
  
  multiDensPlotsOutput <- reactive({
    dmeas <- input$dmeasure
    n_plot <- length(dmeas)
   
    graphData <- passData()
    
    lapply(1:n_plot, function(i){
      output[[paste("plot", i, sep="") ]] <- renderPlotly({
        theGraph <- fun.densPlot(graphData, meas=dmeas[i], group_var=input$dgroup,
                                 flag.facet = input$chkb.dfacet, flag.save=input$saveDPlot)
        #print(theGraph)
        # Convert the ggplot to a plotly
        theGraph <- ggplotly(theGraph)
      }) # end renderPlot()
    }) # end lapply
  })     
  
  ##### Create divs######
  output$dens_plots <- renderUI({
    #  Re-run when button is clicked
    if (input$View.Dens == 0){
      return() }
    else{
      isolate({
        n_plot <- length(input$dmeasure)
        plot_output_list <- lapply(1:n_plot, function(i) {
          plotname <- paste("plot", i, sep="")
          plotlyOutput(plotname, height = 800, width = 800)
        })   
        
        do.call(tagList, plot_output_list)
        multiDensPlotsOutput()
        
      })
    } 
  })  
  
  #=================================================================================================
  #
  # Forth Tab functions -- Boxplots
  #
  #
  output$boxplot.group <- renderUI({
    data <- passData()
    ncat <- which(sapply(data, is.factor))
    data.cat <- data[, ncat]
    group.vars <- names(data.cat) 
    selectInput('bxgroup', 'Filter:', choices=c("None", group.vars), multiple = FALSE)
  })
  #
  output$boxplot.measures <- renderUI({
    data <- passData()
    meas.vars <- c("Start.Av.Weight", "End.Av.Weight", "Econ.FCR.Period", "Mortality.Perc", "Mortality.Deviation.Perc",
                   "SFR.Period.Perc", "SGR.Period.Perc", "Avg.Temp", "Av.Wt.Deviation.Perc", "Diff.Days",
                   "LTD.Mortality.Perc", "LTD.Econ.FCR")
    selectInput('bxmeasure', 'Measure (x-axis):', choices = sort(meas.vars), multiple = TRUE, 
                selected = "Econ.FCR.Period")
  })
  #
  # output$box_plots <- renderPlot({
  #   #  Re-run when button is clicked
  #   if (input$View.Boxplot == 0){
  #     return() }
  #   else{
  #     isolate({
  #       graphData <- passData()
  #       theGraph <- fun.boxPlot(graphData, meas=input$bxmeasure, group_var=input$bxgroup, flag.notch = input$chkb.bxnotch,
  #                                flag.facet = input$chkb.bxfacet, flag.save=input$saveBoxPlot)
  #       print(theGraph)
  #     })
  #   }
  # })
  
  # Multiple Boxplots
  
  multiBoxPlotsOutput <- reactive({
    bxmeas <- input$bxmeasure
    n_plot <- length(bxmeas)
    
    graphData <- passData()
    
    lapply(1:n_plot, function(i){
      output[[paste("plot", i, sep="") ]] <- renderPlotly({
                theGraph <- fun.boxPlot(graphData, meas=bxmeas[i], group_var=input$bxgroup, 
                                        flag.notch = input$chkb.bxnotch,
                                        flag.facet = input$chkb.bxfacet, 
                                        flag.save=input$saveBoxPlot)
                #print(theGraph)
                # Convert the ggplot to a plotly
                theGraph <- ggplotly(theGraph)
      }) # end renderPlot()
    }) # end lapply
  })     
  
  ##### Create divs######
  output$box_plots <- renderUI({
    #  Re-run when button is clicked
    if (input$View.Boxplot == 0){
      return() }
    else{
      isolate({
        n_plot <- length(input$bxmeasure)
        plot_output_list <- lapply(1:n_plot, function(i) {
          plotname <- paste("plot", i, sep="")
          plotlyOutput(plotname, height = 800, width = 800)
        })   
        
        do.call(tagList, plot_output_list)
        multiBoxPlotsOutput()
        
      })
    } 
  })  
  
  #=================================================================================================
  #
  # Fifth Tab functions -- Bar plots
  #
  #
  output$bar.dim <- renderUI({
    data <- passData()
    ncat <- which(sapply(data, is.factor))
    data.cat <- data[, ncat]
    dim.vars <- names(data.cat) 
    selectInput('barDim', 'Dimension (X):', choices=dim.vars, selected = "Species", multiple = FALSE)
  })  
  output$bar.group <- renderUI({
    data <- passData()
    ncat <- which(sapply(data, is.factor))
    data.cat <- data[, ncat]
    group.vars <- names(data.cat)[ !(names(data.cat) %in% input$barDim) ]  
    selectInput('bargroup', 'Filter:', choices=c("None", sort(group.vars)), selected = "None", multiple = FALSE)
  })
  #
  output$bar.meas <- renderUI({
    data <- passData()
    meas.vars <- c("Start.Av.Weight", "End.Av.Weight", "Econ.FCR.Period", "Mortality.Perc", "Mortality.Deviation.Perc",
                   "SFR.Period.Perc", "SGR.Period.Perc", "Avg.Temp", "Av.Wt.Deviation.Perc", "Diff.Days",
                   "LTD.Mortality.Perc", "LTD.Econ.FCR")
    selectInput('barMeas', 'Measure:', choices=c("None", sort(meas.vars)), selected = "None", multiple = TRUE)
  })
  
  # output$bar_plots <- renderPlot({
  #   #  Re-run when button is clicked
  #   if (input$View.Barplots == 0){
  #     return() }
  #   else{
  #     isolate({
  #       graphData <- passData()
  #       theGraph <- fun.barPlot(graphData, dimX = input$barDim, group_var = input$bargroup, 
  #                               meas = input$barMeas, flag.save = input$saveBarPlot,
  #                               flag.sd.se = input$radio.sd.se, flag.facet = input$chkb.barfacet)
  #       print(theGraph)
  #     })
  #   }
  # })
  
  # Multiple Barplots
  
  multiBarPlotsOutput <- reactive({
    brmeas <- input$barMeas
    n_plot <- length(brmeas)
    
    graphData <- passData()
    
    lapply(1:n_plot, function(i){
      output[[paste("plot", i, sep="") ]] <- renderPlotly({
        theGraph <- fun.barPlot(graphData, dimX = input$barDim, group_var = input$bargroup, 
                                meas=brmeas[i], flag.save = input$saveBarPlot,
                                flag.sd.se = input$radio.sd.se, flag.facet = input$chkb.barfacet)
        # print(theGraph)
        # Convert the ggplot to a plotly
        theGraph <- ggplotly(theGraph)
      }) # end renderPlot()
    }) # end lapply
  })     
  
  ##### Create divs######
  output$bar_plots <- renderUI({
    #  Re-run when button is clicked
    if (input$View.Barplots == 0){
      return() }
    else{
      isolate({
        n_plot <- length(input$barMeas)
        plot_output_list <- lapply(1:n_plot, function(i) {
          plotname <- paste("plot", i, sep="")
          plotlyOutput(plotname, height = 800, width = 800)
        })   
        
        do.call(tagList, plot_output_list)
        multiBarPlotsOutput()
        
      })
    } 
  })
  
  #=================================================================================================
  #
  # Sixth Tab functions -- Scatter plots
  #
  #
  output$sc.dimX <- renderUI({
    data <- passData()
    n.numeric <- which(sapply(data, is.numeric))
    data.num <- data[, n.numeric]
    dim.vars <- sort(names(data.num)) 
    selectInput('scDimX', 'Dimension (X):', choices=dim.vars, selected = "Start.Av.Weight", multiple = FALSE)
  })  
  
  output$sc.dimY <- renderUI({
    data <- passData()
    n.numeric <- which(sapply(data, is.numeric))
    data.num <- data[, n.numeric]
    dim.vars <- sort( names(data.num)[ !(names(data.num) %in% input$scDimX) ] ) 
    selectInput('scDimY', 'Dimension (Y):', choices=dim.vars, selected = "Econ.FCR.Period", multiple = FALSE)
  })  
  
  output$sc.size <- renderUI({
    data <- passData()
    n.numeric <- which(sapply(data, is.numeric))
    data.num <- data[, n.numeric]
    size.vars <- sort( names(data.num)[ !(names(data.num) %in% c(input$scDimX, input$scDimY)) ] )
    selectInput('scSize', 'Size:', choices=size.vars, selected = "Mortality.Perc", multiple = FALSE)
  })

  output$sc.group <- renderUI({
    data <- passData()
    ncat <- which(sapply(data, is.factor))
    data.cat <- data[, ncat]
    group.vars <- sort(names(data.cat))
    selectInput('scgroup', 'Filter:', choices=c("None", group.vars), multiple = FALSE)
  })

  output$scatter_plots <- renderPlotly({
  #Re-run when button is clicked
    if (input$View.Scatterplots == 0){
      return() }
    else{
      isolate({
        graphData <- passData()
        p <- fun.scatterPlot(graphData, dimX = input$scDimX, dimY=input$scDimY, Size=input$scSize, group_var = input$scgroup,
                             regr.method = input$method.regress, flag.save = input$saveScatterPlot)
        # print(p)
        # Convert the ggplot to a plotly
        p <- ggplotly(p)
      })
    }
  })


  #=================================================================================================
  #
  # Seventh Tab functions -- Summary Table
  #
  output$sum.group <- renderUI({
    data <- passData()
    ncat <- which(sapply(data, is.factor))
    data.cat <- data[, ncat]
    group.vars <- sort(names(data.cat))
    selectInput('sumgroup', 'Filter:', choices=c("None", group.vars), multiple = TRUE)
  }) 
  #
  output$sum.meas <- renderUI({
    data <- passData()
    meas.vars <- c("Start.Av.Weight", "End.Av.Weight", "Econ.FCR.Period", "Mortality.Perc", "Mortality.Deviation.Perc",
                   "SFR.Period.Perc", "SGR.Period.Perc", "Avg.Temp", "Av.Wt.Deviation.Perc", "Diff.Days",
                   "LTD.Mortality.Perc", "LTD.Econ.FCR")
    selectInput('sumMeas', 'Measure:', choices=sort(meas.vars), multiple = FALSE)
  })
  
  output$summary_stats <- renderTable({
    if (input$View.Stats == 0) {
      return() }
    else{
      isolate({
        data <- passData()
        data_stats <- sum_stats(data, measurevar=input$sumMeas, groupvars=input$sumgroup,
                                na.rm=FALSE, conf.interval=.95, .drop=TRUE, flag.save=input$saveStats)
      })
      return(data_stats)
    }
  })

  #=================================================================================================
  #
  # Function for displaying OLAP cubes - Pivot Tables
  #
  #--------------------------------------------------------------------------------------------
  output$pivotTable <- renderRpivotTable({
     dataPivot = passData()
     rpivotTable(dataPivot)
  })

  #=================================================================================================
  #
  # Machine Learning Tab functions 
  #
  #--------------------------------------------------------------------------------------------
  #     Training Phase
  #--------------------------------------------------------------------------------------------
  #
  preprocess.MachineLearning <- function( data, list.vars, inpts.vars, response )
  {
    dset.train <- data[ , names(data) %in% unlist(list.vars) ]
    dummy.ds <- dummyVars("~.", data=dset.train[inpts.vars], sep=".", fullRank=F)
    dummy.dset.train <- data.frame(predict(dummy.ds, newdata = dset.train), dset.train[response])
    dummy.preds <- names( dummy.dset.train )[names(dummy.dset.train) != response]
    
    # Remove unused columns
    pos.zeros <- which( colSums(dummy.dset.train)==0 )
    if ( length(pos.zeros) != 0 ){
      dummy.dset.train <- dummy.dset.train[,-pos.zeros]
      dummy.preds <- dummy.preds[-pos.zeros]
    }
    
    # Create a new (dummy) formula
    dummy.fmla <- as.formula(paste(response," ~ ",paste(dummy.preds, collapse="+")))
    
    #----------------------------------- Preprocess
    #
    # Identifying Correlated Predictors
    descrCor <- cor(dummy.dset.train[ dummy.preds ])
    highlyCorDescr <- findCorrelation(descrCor, cutoff = .95)
    if ( length(highlyCorDescr) != 0 )
    {
      dummy.dset.train <- dummy.dset.train[, -highlyCorDescr]
      dummy.preds <- names( dummy.dset.train )[names(dummy.dset.train) != response]
      dummy.fmla <- as.formula(paste(response," ~ ",paste(dummy.preds, collapse="+")))
    }
    #
    # The preProcess class can be used for many operations on predictors, 
    #   including centering and scaling.
    #
    dp <- intersect(dummy.preds, inpts.vars)
    pre.data <- dummy.dset.train[ , names(dummy.dset.train) %in% dp ]
    pre.data <- pre.data[ names(pre.data) != response ]
    
    if ( ncol(as.data.frame(pre.data)) == 1 ){
      pre.data <- data.frame(pre.data)
    }
    if( nrow(pre.data) !=0 )
    {
      preProcValues <- preProcess(pre.data, method = c("center", "scale"))
      preproc.dummy.dset.train <- predict(preProcValues, dummy.dset.train)
    }else{
      preproc.dummy.dset.train <- dummy.dset.train
    }
    
    return( list(newdata = preproc.dummy.dset.train, fmla = dummy.fmla) )
    
  } # end preprocess.MachineLearning
  #------------------------------------------------------------------------------------------- 
  #
  output$targs.ML.Variables <- renderUI({ 
    
    selectInput(inputId='Targ.ML.Var', label=h3('Target (Response) Variable:'), 
                choices=c("Econ.FCR.Period", "SGR.Period.Perc", "Av.Wt.Deviation.Perc", "Mortality.Perc",
                          "LTD.Mortality.Perc", "LTD.Econ.FCR"), 
                selected="Econ.FCR.Period", multiple=FALSE)
  })  # end renderUI targs.Variables
  #
  output$preds.ML.Variables <- renderUI({
    data <- passData() 
    Vars <- names(data)
    ind.vars <- Vars[ Vars != input$Targ.ML.Var] 
    selectInput(inputId='Preds.ML.Vars', label=h3('Predictors:'), choices=ind.vars, multiple=TRUE)
  })  # end renderUI preds.Variables
  #
  output$fmla.model <- renderText({
    if (input$goAnalysis == 0){
      return() }
    else{ 
      isolate({   
        fmla = paste( as.character(input$Targ.ML.Var), 
                      paste(as.character(input$Preds.ML.Vars), collapse=" + "), sep=" ~ " )
      })  # end isolate
    } # end if...else
  })
 
  #
  #-----------------------------------------------
  #     Create Model --- SVM function
  #-----------------------------------------------
  runSVM <- reactive({
    
    response <- input$Targ.ML.Var
    inpts.vars <- input$Preds.ML.Vars 
    list.vars <- list(response, inpts.vars)
    
    # "data": dataset that based on the user choices in the first page
    data <- passData()  
    
    # Call Machine Learning preprocess function
    preprocML <- preprocess.MachineLearning( data, list.vars, inpts.vars, response )
    
    preproc.dummy.dset.train = preprocML$newdata
    dummy.fmla <- preprocML$fmla
    
    #----------------------------------- Train the model
    #
    # Setting training controls --- cross-validation
    #
    #---------- Customized the parameters
    set.seed(123)
    reps = 10
    kfolds = input$folds
    times = reps*kfolds
    
    nr <- nrow(preproc.dummy.dset.train)
    perc <- 0.75
    
    seeds <- vector(mode = "list", length = times)
    
    for(i in 1:times) seeds[[i]] <- sample.int(nr, round(nr*perc,digits=1))
    
    ## For the last model:
    seeds[[times + 1]] <- sample.int(nr, 1)
    
    set.seed(1)
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = kfolds,
      ## repeated ten times
      repeats = reps,
      ## set seeds
      seeds = seeds)
    
    svm.model <- train(dummy.fmla, data=preproc.dummy.dset.train, method = "svmRadial", na.action=na.omit,
                    trControl = fitControl, metric="RMSE")
    
    #------------------ Save into file the training dataset with response variable
    #
    Training.Dset <- data.frame("Cage.ID"=row.names(preproc.dummy.dset.train),preproc.dummy.dset.train)
    write.xlsx(Training.Dset, file="Training.Dset.SVM.xlsx", col.names=TRUE, row.names=FALSE)
    
    Training.Dset.RealValues <- data.frame("Cage.ID"=row.names(preproc.dummy.dset.train),
                                  data[ row.names(data) == row.names(preproc.dummy.dset.train), ])
    write.xlsx(Training.Dset.RealValues, file="Training.Dset.SVM.RealValues.xlsx", col.names=TRUE, row.names=FALSE)
    
    return(svm.model)
  })
  
  #
  #-----------------------------------------------------------------------------------
  #     Create Model --- GLM function
  #-----------------------------------------------------------------------------------
  runGLM.Boosting <- reactive({
    
    response <- input$Targ.ML.Var
    inpts.vars <- input$Preds.ML.Vars 
    list.vars <- list(response, inpts.vars)
    
    # "data": dataset that based on the user choices in the first page
    data <- passData()  
    
    # Call Machine Learning preprocess function
    preprocML <- preprocess.MachineLearning( data, list.vars, inpts.vars, response )
    
    preproc.dummy.dset.train <- preprocML$newdata
    dummy.fmla <- preprocML$fmla
    
    #----------------------------------- Train the model
    #
    # Setting training controls --- cross-validation
    #
    
    #---------- Customized the parameters
    set.seed(123)
    reps = 10
    kfolds = input$folds
    times = reps*kfolds
    
    nr <- nrow(preproc.dummy.dset.train)
    perc <- 0.75
    
    seeds <- vector(mode = "list", length = times)
    
    for(i in 1:times) seeds[[i]] <- sample.int(nr, round(nr*perc,digits=1))
    
    # For the last model:
    seeds[[times + 1]] <- sample.int(nr, 1)
    
    set.seed(1)
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = kfolds,
      # repeated ten times
      repeats = reps,
      # set seeds
      seeds = seeds)
    
    glmnet.model <- train(dummy.fmla, data=preproc.dummy.dset.train, method = "glmboost", trControl = fitControl,
                          na.action=na.omit, metric="RMSE")
    
    #------------------ Save into file the training dataset with response variable
    #
    Training.Dset <- data.frame("Cage.ID"=row.names(preproc.dummy.dset.train), preproc.dummy.dset.train)
    write.xlsx(Training.Dset, file="Training.Dset.GLMBoosting.xlsx", col.names=TRUE, row.names=FALSE)
    
    Training.Dset.RealValues <- data.frame("Cage.ID"=row.names(preproc.dummy.dset.train),
                                           data[ row.names(data) == row.names(preproc.dummy.dset.train), ])
    write.xlsx(Training.Dset.RealValues, file="Training.Dset.GLMBoosting.RealValues.xlsx", col.names=TRUE, row.names=FALSE)
    
    return(glmnet.model)
    
  })
  
  
  #-------------------------------------------------------------------------------------------
  #     Create Model --- GAM function 
  #-------------------------------------------------------------------------------------------
  runGAM <- reactive({ })
  
  #-------------------------------------------------------------------------------------------
  #     Create Model --- Random Forest function 
  #-------------------------------------------------------------------------------------------
  runRF <- reactive({ 
    
    response <- input$Targ.ML.Var
    inpts.vars <- input$Preds.ML.Vars 
    list.vars <- list(response, inpts.vars)
    
    # "data": dataset that based on the user choices in the first page
    data <- passData()  
    
    # Call Machine Learning preprocess function
    preprocML <- preprocess.MachineLearning( data, list.vars, inpts.vars, response )
    
    preproc.dummy.dset.train = preprocML$newdata
    dummy.fmla <- preprocML$fmla
    
    #----------------------------------- Train the model
    #
    # Setting training controls --- cross-validation
    #
    
    #---------- Customized the parameters
    set.seed(123)
    reps = 10
    kfolds = input$folds
    times = reps*kfolds
    
    nr <- nrow(preproc.dummy.dset.train)
    perc <- 0.75
    
    seeds <- vector(mode = "list", length = times)
    
    for(i in 1:times) seeds[[i]] <- sample.int(nr, round(nr*perc,digits=1))
    
    ## For the last model:
    seeds[[times + 1]] <- sample.int(nr, 1)
    
    set.seed(1)
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = kfolds,
      ## repeated ten times
      repeats = reps,
      ## set seeds
      seeds = seeds)
    
    rf.model <- train(dummy.fmla, data=preproc.dummy.dset.train, method = "rf", trControl = fitControl,
                      na.action=na.omit, metric="RMSE")
    
    #------------------ Save into file the training dataset with response variable
    #
    Training.Dset <- data.frame("Cage.ID"=row.names(preproc.dummy.dset.train),preproc.dummy.dset.train)
    write.xlsx(Training.Dset, file="Training.Dset.RF.xlsx", col.names=TRUE, row.names=FALSE)
    
    Training.Dset.RealValues <- data.frame("Cage.ID"=row.names(preproc.dummy.dset.train),
                                           data[ row.names(data) == row.names(preproc.dummy.dset.train), ])
    write.xlsx(Training.Dset.RealValues, file="Training.Dset.RF.RealValues.xlsx", col.names=TRUE, row.names=FALSE)
    
    return(rf.model)
    
  })
  
  #-------------------------------------------------------------------------------------------
  #     Create Model --- Multivariate Adaptive Regression Spline (MARS) function
  #-------------------------------------------------------------------------------------------
  runMARS <- reactive({ 
    
    response <- input$Targ.ML.Var
    inpts.vars <- input$Preds.ML.Vars 
    list.vars <- list(response, inpts.vars)
    
    # "data": dataset that based on the user choices in the first page
    data <- passData()  
    
    # Call Machine Learning preprocess function
    preprocML <- preprocess.MachineLearning( data, list.vars, inpts.vars, response )
    
    preproc.dummy.dset.train = preprocML$newdata
    dummy.fmla <- preprocML$fmla
    
    #----------------------------------- Train the model
    #
    # Setting training controls --- cross-validation
    #
    
    #---------- Customized the parameters
    set.seed(123)
    reps = 10
    kfolds = input$folds
    times = reps*kfolds
    
    nr <- nrow(preproc.dummy.dset.train)
    perc <- 0.75
    
    seeds <- vector(mode = "list", length = times)
    
    for(i in 1:times) seeds[[i]] <- sample.int(nr, round(nr*perc,digits=1))
    
    ## For the last model:
    seeds[[times + 1]] <- sample.int(nr, 1)
    
    set.seed(1)
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = kfolds,
      ## repeated ten times
      repeats = reps,
      ## set seeds
      seeds = seeds)
    
    marsGrid <- expand.grid(degree = 1:2, nprune = (1:4) * 10)
    
    mars.model <- train(dummy.fmla, data=preproc.dummy.dset.train, method = "earth", tuneGrid = marsGrid,
                      trControl = fitControl, metric="RMSE", na.action=na.omit)
    
    #------------------ Save into file the training dataset with response variable
    #
    Training.Dset <- data.frame("Cage.ID"=row.names(preproc.dummy.dset.train),preproc.dummy.dset.train)
    write.xlsx(Training.Dset, file="Training.Dset.MARS.xlsx", col.names=TRUE, row.names=FALSE)
    
    Training.Dset.RealValues <- data.frame("Cage.ID"=row.names(preproc.dummy.dset.train),
                                           data[ row.names(data) == row.names(preproc.dummy.dset.train), ])
    write.xlsx(Training.Dset.RealValues, file="Training.Dset.MARS.RealValues.xlsx", col.names=TRUE, row.names=FALSE)
    
    return(mars.model)
    
  })
  
  #----------------------------------------------------
  #
  output$summary_model <- renderPrint({
    if (input$goAnalysis == 0){
      return() }
    else{ 
      isolate({   
        if ( !is.null(input$Targ.ML.Var) ){
          
          # if SVM 
          if (input$radioML.model == 1){
            svm.mod <- runSVM()
            res <- svm.mod$results[rownames(svm.mod$bestTune), ]
            print( round(res, digits = 3) ) 
            
          }else if (input$radioML.model == 2){
            # if GLM
            gml.mod <- runGLM.Boosting()
            res <- gml.mod$results[rownames(gml.mod$bestTune),]
            print( round(res, digits = 3) ) 
        
          } else if (input$radioML.model == 4){ 
            # if Random Forest
            rf.mod <- runRF()
            res <- rf.mod$results[rownames(rf.mod$bestTune),]
            print( round(res, digits = 3) ) 
            
          } else if (input$radioML.model == 5){ 
            # if MARS
            mars.mod <- runMARS()
            res <- mars.mod$results[rownames(mars.mod$bestTune),]
            print( round(res, digits = 3) ) 
          }# end if..else for radioML.model
          
          res.df <- data.frame("RMSE"=round(res$RMSE, digits=3),
                               "R-Squared"=round(res$Rsquared, digits=3) )
          print(res.df)  
        
        }else{ 
          print(data.frame(Warning="Please select Model Parameters."))
        } # end if...else
      }) # end isolate
    } # end if...else
  })  
  
  #----------------------------------------------------
  #
  output$approvalBox.RMSE <- renderInfoBox({
    
    if (input$goAnalysis == 0){
      return() }
    else{ 
      isolate({   
        if ( !is.null(input$Targ.ML.Var) ){
          
          # if SVM 
          if (input$radioML.model == 1){
            svm.mod <- runSVM()
            res <- svm.mod$results[rownames(svm.mod$bestTune), ]
            
          }else if (input$radioML.model == 2){
            # if Boosting GLM
            gml.mod <- runGLM.Boosting()
            res <- gml.mod$results[rownames(gml.mod$bestTune),]
            
        # } else if (input$radioML.model == 3){...}  
            
          } else if (input$radioML.model == 4){ 
            # if Random Forest model
            
            rf.mod <- runRF()
            res <- rf.mod$results[rownames(rf.mod$bestTune), ]
            
          } else if (input$radioML.model == 5){ 
            # if MARS model
            
            mars.mod <- runMARS()
            res <- mars.mod$results[rownames(mars.mod$bestTune), ]  
            
          }# end if..else for radioML.model
          
          res.df <- data.frame("RMSE"=round(res$RMSE,digits=3),
                               "R-Squared"=round(res$Rsquared,digits=3) )
          
          if (res.df$RMSE <= 0.3){
            infoBox(
              "Success", res.df$RMSE, icon = icon("thumbs-up", lib = "glyphicon"),
              color = "green", fill = TRUE
            )
          }else if (res.df$RMSE > 0.3 & res.df$RMSE <= 0.6 ){
            infoBox(
              "Warning", res.df$RMSE, icon = icon("thumbs-warning-sign", lib = "glyphicon"),
              color = "yellow", fill = TRUE
            )
          }else{
            infoBox(
              "Danger", res.df$RMSE, icon = icon("thumbs-down", lib = "glyphicon"),
              color = "red", fill = TRUE
            )
          } # end if..else infoBoxes
        
        }else{ 
          print(data.frame(Warning="Please select Model Parameters."))
        } # end if...else
        
        }) # end isolate
      } # end if...else
    
  })
  #------------------------------- 
  output$approvalBox.Rsquare <- renderInfoBox({
    
    if (input$goAnalysis == 0){
      return() }
    else{ 
      isolate({   
        if ( !is.null(input$Targ.ML.Var) ){
          
          # if SVM 
          if (input$radioML.model == 1){
            svm.mod <- runSVM()
            res <- svm.mod$results[rownames(svm.mod$bestTune), ]
            
          }else if (input$radioML.model == 2){
            # if GLM
            gml.mod <- runGLM.Boosting()
            res <- gml.mod$results[rownames(gml.mod$bestTune),]
            
            # } else if (input$radioML.model == 3){...} 
            
          }else if (input$radioML.model == 4){ 
            # if Random Forest model
            rf.mod <- runRF()
            res <- rf.mod$results[rownames(rf.mod$bestTune), ]
          
          }else if (input$radioML.model == 5){ 
            # if MARS model
            mars.mod <- runMARS()
            res <- mars.mod$results[rownames(mars.mod$bestTune), ]
          
          }# end if..else for radioML.model
          
          res.df <- data.frame("RMSE"=round(res$RMSE,digits=3),
                               "R.Squared"=round(res$Rsquared,digits=3) )
          
          if (res.df$R.Squared >= 0.7){
            infoBox(
              "Success", res.df$R.Squared, icon = icon("thumbs-up", lib = "glyphicon"),
              color = "green", fill = TRUE
            )
          }else if (res.df$R.Squared >= 0.5 & res.df$R.Squared < 0.7 ){
            infoBox(
              "Warning", res.df$R.Squared, icon = icon("thumbs-warning-sign", lib = "glyphicon"),
              color = "yellow", fill = TRUE
            )
          }else if (res.df$R.Squared < 0.5){
            infoBox(
              "Danger", res.df$R.Squared, icon = icon("thumbs-down", lib = "glyphicon"),
              color = "red", fill = TRUE
            )
          } # end if..else infoBoxes
          
        }else{ 
          print(data.frame(Warning="Please select Model Parameters."))
        } # end if...else
        
      }) # end isolate
    } # end if...else
    
  })
  
  
  #---------------------------------------------
  output$ML.Var.Impo <- renderPrint({ 
      if (input$goAnalysis == 0){
         return() }
      else{
        isolate({
              # if SVM 
              if (input$radioML.model == 1){
                    svm.mod <- runSVM()
                    varImp.svm <- varImp(svm.mod, scale = FALSE)
                    vimp.df <- data.frame("Features" = rownames(varImp.svm$importance),
                                          "Importance" = round( varImp.svm$importance$Overall, digits = 3 ))
                    
                    # Remove NA or equal to zeros
                    vimp.df.gr0.na <- vimp.df %>% filter( Importance > 0 & !is.na(Importance) ) 
                    
                    # Calculate Variance Importance as percentage
                    vimp.df.gr0.na$Importance <- round(vimp.df.gr0.na$Importance * 100/sum(vimp.df.gr0.na$Importance),
                                                       digits = 3)
                    
                    # Sorting variables by descenting order in terms of Importance
                    sort.vimp.df.gr0.na <- vimp.df.gr0.na[ order(-vimp.df.gr0.na$Importance),]
                    
                    print(sort.vimp.df.gr0.na, justify="left", row.names=FALSE)
                    
              }else if (input$radioML.model == 2){
                    # if Boosting GLMnet
                    gml.mod <- runGLM.Boosting()
                    RocImp <- varImp(gml.mod, scale = FALSE)
                    
                    results <- data.frame(row.names(RocImp$importance),RocImp$importance$Overall)
                    results$VariableName <- rownames(RocImp)
                    colnames(results) <- c('Features','Importance')
                    
                    results$Importance <- round(results$Importance * 100/sum(results$Importance), digits = 3) 
                    
                    # Sorting variables by descenting order in terms of Importance
                    sort.RocImp <- results[ order(-results$Importance),]
                    print(sort.RocImp, digits=3, justify="left", row.names=FALSE)
                    
                    # }else if (input$radioML.model == 3){ ... }   
                    
              }else if (input$radioML.model == 4){ 
                # if Random Forest
                
                    rf.mod <- runRF()
                    RocImp <- data.frame('Features'=rownames(rf.mod$finalModel$importance), 
                                         'Importance'=rf.mod$finalModel$importance)
                    colnames(RocImp) <- c('Features','Importance')  
                    
                    RocImp$Importance <- round(RocImp$Importance * 100/sum(RocImp$Importance), digits = 3) 
                    
                    # Sorting variables by descenting order in terms of Importance
                    sort.RocImp <- RocImp[ order(-RocImp$Importance),]
                    print(sort.RocImp, digits=3, justify="left", row.names=FALSE) 
                   
              }else if (input$radioML.model == 5){ 
                # if MARS
                
                    mars.mod <- runMARS()
                    
                    varImp.mars <- varImp(mars.mod, scale = FALSE)
                    RocImp <- data.frame("Features" = rownames(varImp.mars$importance),
                                         "Importance" = round( varImp.mars$importance$Overall, digits = 3 ))
                    
                    colnames(RocImp) <- c('Features','Importance')  
                    
                    RocImp$Importance <- round(RocImp$Importance * 100/sum(RocImp$Importance), digits = 3) 
                    
                    # Sorting variables by descenting order in terms of Importance
                    sort.RocImp <- RocImp[ order(-RocImp$Importance),]
                    print(sort.RocImp, digits=3, justify="left", row.names=FALSE) 
                 
              } # end if..else for radioML.model    
        })
      }
  })
  #
  output$plot_ML_Var_Impo <- renderPlotly({
      if (input$goAnalysis == 0){
            return() }
      else{
          isolate({
            # if SVM 
            if (input$radioML.model == 1){
            
                  svm.mod <- runSVM()
                  varImp.svm <- varImp(svm.mod, scale = FALSE)
                  vimp.df <- data.frame("Features" = rownames(varImp.svm$importance),
                                        "Importance" = round( varImp.svm$importance$Overall, digits = 3 ))
                  
                  # Remove NA or equal to zeros
                  vimp.df.gr0.na <- vimp.df %>% filter( Importance > 0 & !is.na(Importance) ) 
                  
                  # Calculate Variance Importance as percentage
                  vimp.df.gr0.na$Importance <- round(vimp.df.gr0.na$Importance * 100/sum(vimp.df.gr0.na$Importance), 
                                                     digits = 3)                
                  
                  # Sorting variables by descenting order in terms of Importance
                  sort.vimp.df.gr0.na <- vimp.df.gr0.na[ order(-vimp.df.gr0.na$Importance),]
                
                  # Plot the Variable Importance
                  p <- ggplot(sort.vimp.df.gr0.na, aes(x = reorder(Features, Importance), y = Importance,
                                                       text=paste("Feature:", Features) )) +
                    geom_bar(stat = "identity", fill="steelblue") + xlab("Features") + ylab("Importance (%)") +
                    theme_economist(dkpanel=TRUE) + ggtitle("Variance Importance (%) using SVM model")
                  p <- p + coord_flip()
                  
                  ggplotly(p)
                  
                  # Save the figure
                  # fig.name <- "Fig.Variance.Importance.png"
                  # ggsave(fig.name, p, type="cairo-png", width = 17.58, height = 20.29, units="cm")
                  
            }else if (input$radioML.model == 2){  # if Boosting GLM
                  
                  gml.mod <- runGLM.Boosting()
                  RocImp <- varImp(gml.mod, scale = FALSE)
                  
                  results <- data.frame(row.names(RocImp$importance),RocImp$importance$Overall)
                  results$VariableName <- rownames(RocImp)
                  colnames(results) <- c('VariableName','Class')
                  results <- results[order(results$Class),]
                  results <- results[(results$Class != 0),]
                  results$Importance <- round(results$Class * 100/sum(results$Class), digits = 3) 
                  results$barcol <- ifelse((results$Class > 0), 'blue', 'red')
            
                  xx <- ggplot(results, aes(x = reorder(VariableName, Importance), y = Importance,
                                            text=paste("Feature:", VariableName) )) +
                    geom_bar(stat = "identity", fill="steelblue", width = 0.25) + 
                    ylab("< (-) importance >  < neutral >  < importance (+) >") + 
                    xlab("Features") + ggtitle("Variance Importance (%) using GLM model") + 
                    theme_economist(dkpanel=TRUE) + scale_color_manual(aes(color=barcol))
                  xx <- xx + coord_flip()
                  
                  ggplotly(xx)
                  
            # }else if (input$radioML.model == 3){ ... } 
            }else if (input$radioML.model == 4){ 
              # if Random forest 
              rf.mod <- runRF()
              RocImp <- data.frame('Features'=rownames(rf.mod$finalModel$importance), 
                                   'Importance'=rf.mod$finalModel$importance)
              colnames(RocImp) <- c('Features','Importance')  
              
              # Remove NA or equal to zeros
              vimp.df.gr0.na <- RocImp %>% filter( Importance > 0 & !is.na(Importance) ) 
              
              # Calculate Variance Importance as percentage
              vimp.df.gr0.na$Importance <- round(vimp.df.gr0.na$Importance * 100/sum(vimp.df.gr0.na$Importance), 
                                                 digits = 3)                      
              
              # Plot the Variable Importance
              p <- ggplot(vimp.df.gr0.na, aes(x = reorder(Features, Importance), y = Importance)) +
                geom_bar(stat = "identity", fill="steelblue") +
                geom_text(aes(label=Importance), hjust=-0.1, color="black", size=3.5) + 
                xlab("Features") + ylab("Importance (%)") +  
                theme_economist(dkpanel=TRUE) + scale_color_manual(aes(color=barcol))
              p <- p + coord_flip()
              
              ggplotly(p)     
          
            }else if (input$radioML.model == 5){ 
              # if MARS
              mars.mod <- runMARS()
              
              varImp.mars <- varImp(mars.mod, scale = FALSE)
              RocImp <- data.frame("Features" = rownames(varImp.mars$importance),
                                   "Importance" = round( varImp.mars$importance$Overall, digits = 3 ))
              
              colnames(RocImp) <- c('Features','Importance')  
              
              # Remove NA or equal to zeros
              vimp.df.gr0.na <- RocImp %>% filter( Importance > 0 & !is.na(Importance) ) 
              
              # Calculate Variance Importance as percentage
              vimp.df.gr0.na$Importance <- round(vimp.df.gr0.na$Importance * 100/sum(vimp.df.gr0.na$Importance), 
                                                 digits = 3)                      
              
              # Plot the Variable Importance
              p <- ggplot(vimp.df.gr0.na, aes(x = reorder(Features, Importance), y = Importance)) +
                geom_bar(stat = "identity", fill="steelblue") +
                geom_text(aes(label=Importance), hjust=-0.1, color="black", size=3.5) + 
                xlab("Features") + ylab("Importance (%)") +  
                theme_economist(dkpanel=TRUE) + scale_color_manual(aes(color=barcol))
              p <- p + coord_flip()
              
              ggplotly(p)               
            
           }  # end if..else
        }) # end isolate
      }
  })
  
  #--------------------------------------------------------------------------------------------
  #     Testing Phase  // Evaluate the Training
  #--------------------------------------------------------------------------------------------
  output$plot_Testing <- renderPlotly({
  
    if (input$ViewTesting == 0){
      return() }
    else{
      isolate({
        
        # Load Training dataset from Excel
        rootpath <- getwd()
        response <- input$Targ.ML.Var
        
        if (input$radioML.model == 1){  # if SVM 
          flname <- file.path(rootpath,"Training.Dset.SVM.xlsx")
          ds.training <- read_excel(flname, sheet = 1, col_names = TRUE)
          testing.flname <- "Testing.Dset.SVM.xlsx"
          trained.mod <- runSVM()
        }else if (input$radioML.model == 2){ # if GLM Boosting
          flname <- file.path(rootpath,"Training.Dset.GLMBoosting.xlsx")
          ds.training <- read_excel(flname, sheet = 1, col_names = TRUE)
          testing.flname <- "Testing.Dset.GLMBoosting.xlsx"
          trained.mod <- runGLM.Boosting()
        }else if (input$radioML.model == 4){ # if Random Forest
          flname <- file.path(rootpath,"Training.Dset.RF.xlsx")
          ds.training <- read_excel(flname, sheet = 1, col_names = TRUE)
          testing.flname <- "Testing.Dset.RF.xlsx"
          trained.mod <- runRF()
        }else if (input$radioML.model == 5){ # if MARS
          flname <- file.path(rootpath,"Training.Dset.MARS.xlsx")
          ds.training <- read_excel(flname, sheet = 1, col_names = TRUE)
          testing.flname <- "Testing.Dset.MARS.xlsx"
          trained.mod <- runMARS()
        }        
        
        # Remove cases with NAs
        ds.training.no.NAs <- ds.training[complete.cases(ds.training), ]
    
        # Choose randomly a perc of population for testing
        nr <- nrow(ds.training.no.NAs)
        p <- input$perc
        ids <- sample(1:nr, round(nr*p/100,0), replace = FALSE) 
        ds.testing <- ds.training.no.NAs[ ids, ]
   
        #------------------ Testing Phase
        test.patterns <- ds.testing[ , !(names(ds.testing) %in% response) ]
        test.targets <- ds.testing[ , response]

        testPred <- predict( trained.mod, test.patterns )
        rel.error <- (testPred - ds.testing[ , response])*100/ds.testing[ , response] 
        names(rel.error) <- "Rel.Err"
        
        #----- Estimate Relative Error of prediction for each observation
        ds.testing$Model.Preds <- testPred
        ds.testing <- cbind(ds.testing, rel.error)  
        
        #------------------ Save into file the testing dataset with response variable and Relative Error
        #
        write.xlsx(ds.testing, file=testing.flname, col.names=TRUE, row.names=FALSE)
        
        #-------- Plot Relative Error
        ds <- data.frame( ds.testing$Cage.ID, ds.testing$Model.Preds, 
                          ds.testing[ , response], ds.testing$Rel.Err )
        names(ds) <- c("Cage.ID", "Model.Prediction", "Actual.Response", "Rel.Err")
    
        # Filter mdata according to the Relative Error Threshold
        ds <- ds %>% filter( abs(Rel.Err) >= input$thresh.RE[1] &
                             abs(Rel.Err) <= input$thresh.RE[2] ) 
        
       mdata <- melt(ds, id=c("Cage.ID","Rel.Err"))
       names(mdata) <- c("Cage.ID", "Rel.Err", "Category", "Response")
       max.val <- max(mdata$Response)
        
        #png("BarPlot_Actual.Preds.png", width = 640, height = 480)
        p1 <- ggplot(mdata, aes(x=Cage.ID, y=Response, fill=Category,
                                text=paste("Rel.Error:", round(Rel.Err,digits = 3) ))) +
                  geom_bar(stat = "identity", position=position_dodge()) +
                 # geom_text(aes(y=Response, ymax=Response, label=round(Response, 1)), 
                  #          position= position_dodge(width=1), vjust=0, color="black", size=3) +
                  scale_y_continuous("Response", limits=c(0,max.val), breaks=seq(0, max.val, .5)) + 
                  scale_x_discrete("Cage.ID") + scale_fill_brewer(palette="Set1") +
                  theme_economist(stata = TRUE) + scale_color_economist() + 
                  theme(legend.position="bottom", legend.title = element_blank()) +
                  ggtitle("Actual Response Values V.S. Estimated Model values") +
                  coord_flip()
        
          if( input$saveTesting ){
            fig.name <- paste("Fig.Testing", response, "Rel.Err.Thresh", input$thresh.RE[1],"To",
                              input$thresh.RE[2], "png", sep=".")
            ggsave(fig.name, p1, type="cairo-png", width = 17.58, height = 20.29, units="cm")
          }
        
        ggplotly(p1)
        
      }) # end isolate
    } # end if..else
    
  })
  #--------------------------------------------------
  output$evaluate_model <- renderPrint({
    if (input$ViewTesting == 0){
      return() }
    else{ 
      isolate({   
  
        # Load Testing dataset from Excel & ML Models
        rootpath <- getwd()
        response <- input$Targ.ML.Var
        
        if (input$radioML.model == 1){  # if SVM 
          flname <- file.path(rootpath, "Testing.Dset.SVM.xlsx")
          ds.testing <- read_excel(flname, sheet = 1, col_names = TRUE)
          trained.mod <- runSVM()
        }else if (input$radioML.model == 2){ # if GLM Boosting
          flname <- file.path(rootpath, "Testing.Dset.GLMBoosting.xlsx")
          ds.testing <- read_excel(flname, sheet = 1, col_names = TRUE)
          trained.mod <- runGLM.Boosting()
        }else if (input$radioML.model == 4){ # if Random Forest 
          flname <- file.path(rootpath, "Testing.Dset.RF.xlsx")
          ds.testing <- read_excel(flname, sheet = 1, col_names = TRUE)
          trained.mod <- runRF()
        }else if (input$radioML.model == 5){ # if MARS 
          flname <- file.path(rootpath, "Testing.Dset.MARS.xlsx")
          ds.testing <- read_excel(flname, sheet = 1, col_names = TRUE)
          trained.mod <- runMARS()
        }  
        
        #-------- Plot Relative Error
        ds <- data.frame( "Cage.ID"= ds.testing$Cage.ID,
                          "Model.Prediction"=ds.testing$Model.Preds, 
                          "Actual.Response" = ds.testing[ , names(ds.testing) %in% response], 
                          "Rel.Err"= ds.testing$Rel.Err )
        names(ds) <- c("Cage.ID","Model.Prediction","Actual.Response","Rel.Err")
      
        # Filter mdata according to the Relative Error Threshold
        ds <- ds %>% filter( abs(Rel.Err) >= input$thresh.RE[1] &
                               abs(Rel.Err) <= input$thresh.RE[2] ) 
  
        # Calculate the coefficient of determination (R-squared)
        #
        real.mean <- round( mean(ds$Actual.Response), digits=3 )
        #---> The regression sum of squares, also called the explained sum of squares
        ESS <- round( sum( (ds$Model.Prediction - real.mean)^2 ), digits=3 )
        #---> The sum of squares of residuals, also called the residual sum of squares
        RSS <- round( sum( (ds$Actual.Response - ds$Model.Prediction)^2 ), digits = 3 )
        #---> The total sum of squares (proportional to the variance of the data)
        SS.tot <- round( sum( (ds$Actual.Response - real.mean)^2 ), digits = 3)
        
        R.squared <- round(1 - RSS/SS.tot, digits = 3 )
        
        # Fraction of unexplained variance (FUV)
        FUV = round( 1 - R.squared, digits = 3)
       
        # Root Mean Squared Error
        nr <- nrow(ds)
        RMSE <- round( sqrt( sum( (ds$Model.Prediction - ds$Actual.Response)^2 )/nr ),
                       digits = 3 )
        
        results <- list( "R.Squared"=R.squared, "Root Mean Squared Error"=RMSE,
                         "Fraction of Unexplained Variance"=FUV,
                         "Explained Sum of Squares"=ESS, "Residual Sum of Squares"=RSS,
                         "Total Sum of Squares"=SS.tot) 
    
        print( results )
      
      })
    }
  })
        
  #--------------------------------------------------------------------------------------------
  #     Prediction Phase
  #--------------------------------------------------------------------------------------------
  # Tab: Predict with Machine Learning Models
  #
  predict.with.ML.Model <- reactive({
    
    # load the ML model
    if (input$radioML.model == 1){ # if SVM 
      ML.model <- runSVM()
    }else if (input$radioML.model == 2){ # if GLM Boosting
      ML.model <- runGLM.Boosting()
    }else if (input$radioML.model == 4){ # if Random Forest     
      ML.model <- runRF()
    }else if (input$radioML.model == 5){ # if MARS     
      ML.model <- runMARS()
    }  
    
    # load the dataset
    data <- passData()
    
    # create an instance from the input values (user-defined values)
    list.predictors <- input$Preds.ML.Vars
    num.preds <- length(list.predictors)
    response <- input$Targ.ML.Var
    fmla <- as.formula( paste(" ", paste(input$Preds.ML.Vars, collapse="+"), sep=" ~ ") )
    
    newdata <- as.data.frame(matrix(0, nrow = 1, ncol=num.preds))
    newdata <- lapply(1:num.preds, function(i) {
      input_name <- paste0("input", i, sep="")
      input[[ input_name ]]
    } # end function
    )# end lapply
    names(newdata) <- list.predictors
 
    # Dummy dataset & variables
    tm.data <- rbind(data[ list.predictors ], newdata)
    names(tm.data) <- list.predictors
    
    dummy.instance.data <- dummyVars("~.", data=tm.data, fullRank=F)
    dummy.newdata <- data.frame( predict( dummy.instance.data, newdata = tm.data), 'Class'=NA)
    names(dummy.newdata)[names(dummy.newdata)=="Class"] <- response
  
    # The preProcess class can be used for many operations on predictors,
    #  including centering and scaling.
    dummy.preds <- names( dummy.newdata )[names(dummy.newdata) != response]
    dp <- intersect(dummy.preds, list.predictors)
    pre.data <- dummy.newdata[ names(dummy.newdata) %in% dp ]
    pre.data <- pre.data[ names(pre.data) != response ]
    
    if ( ncol(as.data.frame(pre.data)) == 1 ){
      pre.data <- data.frame(pre.data)
    }
    if( nrow(pre.data) !=0 )
    {
      preProcValues <- preProcess(pre.data, method = c("center", "scale"))
      preproc.dummy.newdata <- predict(preProcValues, dummy.newdata)
    }else{
      preproc.dummy.newdata <- dummy.newdata
    }
   
    dummy.inpts <- as.matrix(data.frame(preproc.dummy.newdata[ nrow(preproc.dummy.newdata), ]))
    
    pred_ML_model <- predict(ML.model, dummy.inpts, type="raw", na.action = na.omit)
    
    names(pred_ML_model) <- as.character(input$Targ.ML.Var)
    
    return(pred_ML_model)
    
  })
  #
  output$dyn_input.ML <- renderUI({
    
    data <- passData()
    list.predictors <- input$Preds.ML.Vars
    num.preds <- length(list.predictors)
    
    inputs <- lapply(1:num.preds, function(i) {
      input_name <- paste0("input", i, sep="")
      fluidRow(column(width=6, 
                      if ( is.factor( data[, list.predictors[[i]]] ) )
                      {
                        list.values <- unique( data[, list.predictors[[i]]] )
                        selectInput(inputId=input_name, label=h4( as.character(list.predictors[[i]]) ), 
                                    choices=as.character(list.values), multiple=FALSE)
                      }else{  
                        numericInput( input_name, label = h4( as.character(list.predictors[[i]]) ),
                                      value = NA)
                      } # end if...else
      ) # end column
      ) # end fluidRow
    } # end function
    ) # end lapply
    
    do.call(tagList, inputs)
  }) 
  #
  # predict value regarding the predictors' values
  output$prediction.value.ML <- renderPrint({ 
    
    if (input$goPrediction == 0){
      return() }
    else{ 
      isolate({
        
        pred_val <- predict.with.ML.Model()
        names(pred_val) <- as.character(input$Targ.ML.Var)
        
        ml.response <- data.frame(pred_val, stringsAsFactors = FALSE)
        print( ml.response )
        
      }) # end isolate
    } # end if...else
    
  })
  
  #--------------------------------------------------------------------------------------------
  #     Page "KPIs Table estimation" 
  #--------------------------------------------------------------------------------------------
  
  estimateKPI.Table <- reactive({ 
    
    rate = 0.8
    predictors <- c("Avg.Temp", "Period.Av.Weight")
    
    if (input$radioKPI == 1){  
      response.var <- "Biol.FCR.Period"
    }else if (input$radioKPI == 2){  
      response.var <- "Econ.FCR.Period"
    }else if (input$radioKPI == 3){ 
      response.var <- "SFR.Period.Perc"
    }else if (input$radioKPI == 4){      
      response.var <- "SGR.Period.Perc"
    }else if (input$radioKPI == 5){      
      response.var <- "Mortality.Perc"
    }  
  
    # "data": dataset that based on the user choices in the first page
    ds <- passData()  
  
    # Step 1: Create and find the best model
    
    # res.best.mod <- Find_Best_Model(ds, rate, response.var, predictors)
    
    # model that both predictors are non-linear
    
    predictors.vars <- c("Period.Av.Weight", "Avg.Temp")
    fmla <- as.formula( paste(response.var, 
                               paste(paste0("s(",predictors.vars, ", bs= \'cr\' )"), collapse="+"), sep="~") )
    gam.mod <- gam(formula=fmla, family=gaussian(link=identity), data=ds)
    
    summary.gam.mod <-summary(gam.mod)
    
    pred.data <- data.frame(ds[,predictors.vars[1]], ds[,predictors.vars[2]])
    colnames(pred.data) <- predictors.vars 
    pred.gam.mod <- predict(gam.mod, pred.data)
    rmse.gam.mod <- sqrt( mean( (ds[, response.var] - pred.gam.mod)^2 ) )
    
    # Step 2: Create the KPI Table
    #
    # Create the Weight categories and Temperature values
    minAvTemp <- max(10, round(min(ds$Avg.Temp)-1, digits=0) ) 
    maxAvTemp <- min(40, round(max(ds$Avg.Temp)+1, digits=0) ) 
    stepAvTemp <- input$temp.step
    Temp.vals <- seq(from = minAvTemp, to = maxAvTemp, by= stepAvTemp)
    
    minAvWeight <- min(ds$Period.Av.Weight) 
    maxAvWeight <- max(ds$Period.Av.Weight) 
    step.WeightCat <- input$weight.step
    AvWeight.vals <- seq(from = min(0,minAvWeight), to = maxAvWeight + step.WeightCat, by = step.WeightCat) 

    pred.table.values <- predict(gam.mod, newdata = expand.grid("Avg.Temp" = Temp.vals, 
                                                                 "Period.Av.Weight" = AvWeight.vals))
    
    mat<-matrix(pred.table.values, nrow =length(Temp.vals), ncol= length(AvWeight.vals))
    mat <- t(mat)
   
    colnames(mat) <- paste(Temp.vals, sep=" ")
    row.names(mat) <- paste(AvWeight.vals, sep=" ")
    
    KPI.Table <- as.data.frame( cbind("AvWeightCat"=AvWeight.vals, round(mat,3)) )
    
    if (input$radioKPI == 4){      
      KPI.Table <- abs(KPI.Table)
    }  
    
    return(KPI.Table)
                               
  })
  
  #------------------ KPI Table
  output$KPI_Table <- renderTable({
    
    if (input$ViewKPITable == 0){
      return() }
    else{
      isolate({
              
          KPI.Table <- estimateKPI.Table()
          return(KPI.Table)
      })
    }
    
  })
  
  #------------------ 3D plot KPI Table
  output$plot_3D_Table <- renderPlotly({ 
    
    # if (input$goKPIAnalysis == 0){
    #   return() }
    # else{ 
    #   isolate({
    #     
            # inputs
            KPI.Table <- estimateKPI.Table()
            ds <- passData()
           
            predictors.vars <- c("Period.Av.Weight", "Avg.Temp")
            
            if (input$radioKPI == 1){  
              response.var <- "Biol.FCR.Period"
            }else if (input$radioKPI == 2){  
              response.var <- "Econ.FCR.Period"
            }else if (input$radioKPI == 3){ 
              response.var <- "SFR.Period.Perc"
            }else if (input$radioKPI == 4){      
              response.var <- "SGR.Period.Perc"
            }else if (input$radioKPI == 5){      
              response.var <- "Mortality.Perc"
            }
            
            dset <- ds[ , names(ds) %in% c(predictors.vars, response.var)] 
            
            # KPIs table
            nc <- ncol(KPI.Table)
            nr <- nrow(KPI.Table)
            # remove first column from table
            table.data <- as.matrix(KPI.Table[,-1])
            
         #   RawData <- gather( table.data, predictors.vars[2], response.vars, 1:(nc-1) )
        #    newRawData <- RawData %>% select(predictors.vars[2], predictors.vars[1], response.vars )
            
            f <- list(
              family = "Courier New, monospace",
              size = 14,
              color = "#7f7f7f"
            )
            
            # "Avg.Temp" axis (columns of KPI.Table)
            ax1.names <- colnames(KPI.Table)
            ax1.vals <- as.numeric(as.vector(ax1.names[2:nc]))
            ax1.range = c( min(ax1.vals), max(ax1.vals) )
            ax1.lab <- list(range=ax1.range, tickmode="array",tickvals=ax1.vals, ticktext=as.character(ax1.vals), 
                          nticks = nc-1, showline=T, title = predictors.vars[2], titlefont = f) 
            
            # "Period.Av.Weight" axis (rows of KPI.Table)
            ax2.vals <- as.numeric(as.vector(KPI.Table[1:nr, 1]))
            ax2.range = c( min(ax2.vals), max(ax2.vals) )
            ax2.lab <- list(range=ax2.range, tickmode="array",tickvals=ax2.vals, ticktext=as.character(ax2.vals), 
                          nticks = nr, showline=T, title = predictors.vars[1], titlefont = f)
            
            # Z = "KPI" axis
            z.lab <- list(title = response.var, titlefont = f)
            
            scene=list(xaxis = ax2.lab, yaxis = ax1.lab, zaxis = z.lab)
                       # camera = list(eye = list(x = 1.25, y = 1.25, z = 1.25)))
            
            # 
            fig.title <- paste(response.var, "table per", predictors.vars[1], "and", predictors.vars[2], sep=" ")
            p <- plot_ly(x=ax2.vals, y=ax1.vals, z = table.data, type = "surface") %>%
                        layout(title = fig.title, scene=scene, width = 800, height = 600)
                          # add_trace(data = dset, x = dset[, predictors.vars[1]], 
                          #           y = dset[, predictors.vars[2]], z = dset[, response.var],
                          #           mode = "markers", type = "scatter3d",
                          #           marker = list(size = 3, color = "red", symbol = 104)) %>% 
            
            #p <- plot_ly(newRawData, x=~predictors.vars[2], y=~predictors.vars[1], 
            #             z = ~response.var, type = "surface") %>%
            #              layout(title = fig.title, scene=scene, width = 800, height = 600) 
            
         
      #   })
      # }    
  })
  
  
  #--------------------------------------------------------------------------------------------
  #     Page "About" --- buzz words
  #--------------------------------------------------------------------------------------------
  output$plot.buzzWords <- renderPlot({
    
    terms <- c("Data Mining", "Machine Learning", "KDD", "Clustering", 
                "Classification", "Association Rules", "k-NN", "Random Forest",
                "GAM", "GLM", "Regression", "Deep Learning", 
                "Neural Networks", "SVMs", "Descriptive Statistics", 
                "Predictive Analytics", "Statistical Learning", 
                "Outlier Detection", "Forecast", "Big Data", "Cloud", "Web",
                "Aquaculture", "AquaManager", "AquaTracker", "Feeder", "SFR", "SGR", "Mortality",  
                "Bass", "Bream", "Red Bream", "Meagre", "Hatchery", "FCR", "Temperature",
                "Currents", "Harvest", "Site", "Cages", "Specie", "Batch", "Stocking"
              )
    
    Words <- rep(terms, 5)
    Freqs <- round(runif(length(Words),0,1)*100 ,0)
    df.words <- data.frame(Words, Freqs)
    pal <- brewer.pal(8,"Dark2")
    wordcloud(df.words$Words,df.words$Freqs, scale=c(3,2), min.freq=2, max.words=1000, 
              random.order=T, rot.per=.25, colors=pal, vfont=c("sans serif","plain"))
    
  })
  
}) # end shinyServer
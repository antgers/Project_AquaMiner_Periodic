### Version Periodic (Sampling To Sampling) Datasets
#
# Created: 09/06/2016
# Last Modified: 23/10/2016
#
# Author: Gerasimos Antzoulatos (i2s)
#-------------------------------------------------------------
#
# load packages
#
library("shiny")
library("shinyFiles")
library("shinyBS")
library("shinydashboard")
library("lubridate")
library("htmltools")
library("d3heatmap")
library("DT") 
library("rpivotTable") #devtools::install_github("smartinsightsfromdata/rpivotTable")
library("htmlwidgets")
library("RColorBrewer") 
library("readxl") 
library("xlsx")
library("XLConnect")
library("graphics")
library("ggplot2")
library("plotly")
library("plot3D")
library("scales")
library("ggthemr") # devtools::install_github('ggthemr', 'cttobin')
library("ggthemes")
library("extrafont")
library("lattice")
library("mgcv")
library("plotrix")
library("psych")
library("plyr")
library("dplyr")
library("tidyr")
library("reshape")
library("GGally")
library("e1071")
library("caret")
library("mboost")
library("earth")
library("pROC")
library("glmnet")
library("rpart")
library("party")
library("rpart.plot")
library("partykit")
library("Hmisc")
library("effects")
library("car")
library("relaimpo")
library("ROCR")
library("kernlab")
library("fpc")
library("randomForest")
library("maptree")
library("nlme")
library("dimple") #devtools::install_github("Bart6114/dimple")
library("wordcloud")



# ------------------------------------------------------------------------
# Create the dataset
# ------------------------------------------------------------------------
create.dataset <- function(dataset){
  
  ds.names <- names(dataset)
  
  if ("Region" %in% ds.names){ data <- data.frame("Region" = dataset$Region) }
  if ("Site" %in% ds.names){ data <- cbind( data, "Site" = dataset$Site) }
  if ("Unit" %in% ds.names){ data <- cbind( data, "Unit" = dataset$Unit) }
  if ("Batch" %in% ds.names){ data <- cbind( data, "Batch" = dataset$Batch) }
  if ("Species" %in% ds.names){ data <- cbind( data, "Species" = dataset$Species) } 
  if ("Start.Fish.Density" %in% ds.names){ data <- cbind( data, "Start.Fish.Density" = dataset$Start.Fish.Density) }
  if ("End.Fish.Density" %in% ds.names){ data <- cbind( data, "End.Fish.Density" = dataset$End.Fish.Density) }
  if ("Hatchery" %in% ds.names){ data <- cbind( data, "Hatchery"  = dataset$Hatchery) }
  if ("Lot.Quality" %in% ds.names){data <- cbind( data, "Lot.Quality" = as.character(dataset$Lot.Quality)) }
  if ("Origin.Year" %in% ds.names){data <- cbind( data, "Origin.Year" = as.character(dataset$Origin.Year)) }
  if ("Origin.Month" %in% ds.names){data <- cbind( data, "Origin.Month" = dataset$Origin.Month) }
  if ("From" %in% ds.names){data <- cbind( data, "From"= ymd(as.Date(dataset$From, origin="1899-12-30")) ) }
  if ("From" %in% ds.names){ data <- cbind( data, "From.Month" = as.character(month(dataset$From, label = TRUE, abbr = FALSE)) ) } 
  if ("From" %in% ds.names){ data <- cbind( data, "From.Year" = as.character(year(dataset$From)) ) }
  if ("To" %in% ds.names){ data <- cbind( data, "To" = ymd(as.Date(dataset$To, origin="1899-12-30")) ) }
  if ("To" %in% ds.names){ data <- cbind( data, "To.Month" = as.character(month(dataset$To, label = TRUE, abbr = FALSE)) ) }
  if ("To" %in% ds.names){ data <- cbind( data, "To.Year" = as.character(year(dataset$To)) ) }
  if ( ("From" %in% ds.names) & ("To" %in% ds.names) )
  {
    data <- cbind( data, "Diff.Days" = interval(as.Date(dataset$From, origin="1899-12-30"), 
                                                as.Date(dataset$To, origin="1899-12-30") )%/%days(1) )
  }
  if ("Start.Av.Wt." %in% ds.names){ data <- cbind( data, "Start.Av.Weight" = round(dataset$Start.Av.Wt., digits = 2) ) }
  if ("End.Av.Wt." %in% ds.names){ data <- cbind( data, "End.Av.Weight" = round(dataset$End.Av.Wt., digits = 2) ) }
  if ( ("End.Av.Wt." %in% ds.names) & ("Start.Av.Wt." %in% ds.names) ){
    data <- cbind( data, 
                   "Period.Av.Weight" = round( ( dataset$End.Av.Wt. + dataset$Start.Av.Wt. )/2, digits = 2) )
  }
  if ("Av.Weight.Change" %in% ds.names){ data <- cbind( data, "Av.Weight.Change" = round(dataset$Av.Weight.Change, digits = 2) ) }
  if ("Model.End.Av.Wt.Act.Feed" %in% ds.names){ data <- cbind( data, "Model.End.Av.Wt.Act.Feed" = round(dataset$Model.End.Av.Wt.Act.Feed, digits = 2) ) }
  if ("Av.Wt.Deviation.Perc" %in% ds.names){ data <- cbind( data, "Av.Wt.Deviation.Perc" = round(dataset$Av.Wt.Deviation.Perc, digits = 2) ) }
  if ("Av.Wt.Before.Sampl." %in% ds.names){ data <- cbind( data, "Av.Wt.Before.Sampl" = round(dataset$Av.Wt.Before.Sampl., digits = 2) ) }
  if ("Model.End.Av.Wt.Sugg.Feed" %in% ds.names){ data <- cbind( data, "Model.End.Av.Wt.Sugg.Feed" = round(dataset$Model.End.Av.Wt.Sugg.Feed, digits = 2) ) }
  if ("Actual.Feed" %in% ds.names){ data <- cbind( data, "Actual.Feed" = dataset$Actual.Feed ) }
  if ("Feed.Category" %in% ds.names){ data <- cbind( data, "Feed.Category" = dataset$Feed.Category ) }
  if ("Supplier" %in% ds.names){ data <- cbind( data, "Supplier" = dataset$Supplier ) }
  if ("Proteins.Avg.Perc" %in% ds.names){ data <- cbind( data, "Proteins.Avg.Perc" = round(dataset$Proteins.Avg, digits = 2) ) }
  if ("Lipids.Avg.Perc" %in% ds.names){ data <- cbind( data, "Lipids.Avg.Perc" = round(dataset$Lipids.Avg, digits = 2) ) }
  if ("Gross.Energy" %in% ds.names){ data <- cbind( data, "Gross.Energy" = round(dataset$Gross.Energy, digits = 2) ) }
  if ("Digestive.Energy" %in% ds.names){ data <- cbind( data, "Digestive.Energy" = round(dataset$Digestive.Energy, digits = 2) ) }
  if ( ("Digestive.Energy" %in% ds.names) & ("Period.Feed.Qty" %in% ds.names) ){
      data <- cbind( data, "Period.Feed.Energy" = round(dataset$Digestive.Energy * dataset$Period.Feed.Qty, digits = 2) ) 
  }
  if ("Period.Feed.Qty" %in% ds.names){ data <- cbind( data, "Period.Feed.Qty"= round(dataset$Period.Feed.Qty, digits = 2) ) }
  if ("Model.Feed.Qty" %in% ds.names){ data <- cbind( data, "Model.Feed.Qty" = round(dataset$Model.Feed.Qty, digits = 2) ) }
  if ("Feed.Deviation.Perc" %in% ds.names){ data <- cbind( data, "Feed.Deviation.Perc" = round(dataset$Feed.Deviation.Perc, digits = 2) ) }
  if ("Opening.Fish.No" %in% ds.names){ data <- cbind( data, "Opening.Fish.No" =  dataset$Opening.Fish.No ) } 
  if ("Opening.Biomass" %in% ds.names){ data <- cbind( data, "Opening.Biomass" =  round(dataset$Opening.Biomass, digits = 2) ) }
  if ("Closing.Fish.No" %in% ds.names){ data <- cbind( data, "Closing.Fish.No" =  dataset$Closing.Fish.No ) }
  if ("Closing.Biomass" %in% ds.names){ data <- cbind( data, "Closing.Biomass" =  round(dataset$Closing.Biomass, digits = 2) ) }
  if ("Harvest.Biomass" %in% ds.names){ data <- cbind( data, "Harvest.Biomass" =  round(dataset$Harvest.Biomass, digits = 2) ) }
  if ("Transfer.Minus.Kg" %in% ds.names){ data <- cbind( data, "Transfer.Minus.Kg" = round(dataset$Transfer.Minus.Kg, digits = 2) ) }
  if ("Transfer.Plus.Kg" %in% ds.names){ data <- cbind( data, "Transfer.Plus.Kg" = round(dataset$Transfer.Plus.Kg, digits = 2) ) }
  if ("Biomass.Produced" %in% ds.names){ data <- cbind( data, "Biomass.Produced" = round(dataset$Biomass.Produced, digits = 2) ) }
  if ("Biomass.Produced.Before.Sampl." %in% ds.names){ data <- cbind( data, "Biomass.Produced.Before.Sampl" = round(dataset$Biomass.Produced.Before.Sampl., digits = 2) ) }
  if ("Biol.FCR.Period" %in% ds.names){ data <- cbind( data, "Biol.FCR.Period" = round(dataset$"Biol.FCR.Period", digits = 2) ) }
  if ("Econ.FCR.Period" %in% ds.names){ data <- cbind( data, "Econ.FCR.Period" = round(dataset$Econ.FCR.Period, digits = 2) ) }
  if ("Econ.FCR.Period.Before.Sampl." %in% ds.names){ data <- cbind( data, "Econ.FCR.Period.Before.Sampl" = round(dataset$Econ.FCR.Period.Before.Sampl., digits = 2) ) }
  if ("Mortality.No" %in% ds.names){ data <- cbind( data, "Mortality.No" = dataset$Mortality.No ) }
  if ("LTD.Mortality.Kg" %in% ds.names){ data <- cbind( data, "LTD.Mortality.Kg" = round(dataset$"LTD.Mortality.Kg", digits = 2) ) }
  if ("Model.Mortality.No" %in% ds.names){ data <- cbind( data,"Model.Mortality.No" = dataset$Model.Mortality.No ) }
  if ("Mortality.Deviation.Perc" %in% ds.names){ data <- cbind( data, "Mortality.Deviation.Perc" = round(dataset$Mortality.Deviation.Perc, digits = 2) ) }
  if (("Mortality.No" %in% ds.names) & ("Opening.Fish.No" %in% ds.names)){
        data <- cbind( data, "Mortality.Perc" = round(dataset$Mortality.No*100/dataset$Opening.Fish.No, digits = 2) ) }
  if ("SFR.Period.Perc" %in% ds.names){ data <- cbind( data,"SFR.Period.Perc" = round(dataset$SFR.Period.Perc, digits = 2) ) }
  if ("SFR.Period.Perc.Before.Sampl." %in% ds.names){ data <- cbind( data,"SFR.Period.Perc.Before.Sampl" = round(dataset$SFR.Period.Perc.Before.Sampl., digits = 2) ) }
  if ("SGR.Period.Perc" %in% ds.names){ data <- cbind( data,"SGR.Period.Perc" = round(dataset$SGR.Period.Perc, digits = 2) ) }
  if ("FCR.bio.*.SGR" %in% ds.names){ data <- cbind( data,"FCR.bio.*.SGR" = round(dataset$"FCR.bio.*.SGR", digits = 2) ) }
  if ("Growth.Per.Day" %in% ds.names){ data <- cbind( data,"Growth.Per.Day" = round(dataset$"Growth.Per.Day", digits = 2) ) }
  if ( ("End.Av.Wt." %in% ds.names) & ("Start.Av.Wt." %in% ds.names) & ("Period.Day.Degrees" %in% ds.names) ){
    data <- cbind( data, 
                   "TGC.Period" = round( ( dataset$End.Av.Wt.^(1/3) - dataset$Start.Av.Wt.^(1/3) )*1000 / dataset$Period.Day.Degrees, digits = 2) )
  }
  if ("Avg.Temp." %in% ds.names){ data <- cbind( data,"Avg.Temp" = round(dataset$Avg.Temp., digits = 2) ) }
  if ("Avg.Oxygene" %in% ds.names){ data <- cbind( data,"Av.Oxygene" = round(dataset$Avg.Oxygene, digits = 2) ) }
  if ("Period.Day.Degrees" %in% ds.names){ data <- cbind( data,"Period.Day.Degrees" = dataset$Period.Day.Degrees ) }
  if ("Start.Av.Weight.Category" %in% ds.names){ data <- cbind( data,"Start.Av.Weight.Category" = dataset$"Start.Av.Weight.Category" ) }
  if ("End.Av.Weight.Category" %in% ds.names){ data <- cbind( data,"End.Av.Weight.Category" = dataset$"End.Av.Weight.Category" ) } 
  if ("Max.Feed.Qty" %in% ds.names){ data <- cbind( data,"Max.Feed.Qty" = round(dataset$Max.Feed.Qty, digits = 2) ) }
  if ("Food.Price" %in% ds.names){ data <- cbind( data,"Food.Price" = round(dataset$Food.Price, digits = 2) ) }
  if ("Current.Grading" %in% ds.names){ data <- cbind( data,"Current.Grading" = as.character(dataset$Current.Grading) ) }
  if ("Feeding.Policy" %in% ds.names){ data <- cbind( data,"Feeding.Policy" = dataset$Feeding.Policy ) }
  if ("Group.Tag" %in% ds.names){ data <- cbind( data,"Group.Tag" = dataset$Group.Tag ) }
  if ("Vaccinated" %in% ds.names){ data <- cbind( data,"Vaccinated" = dataset$Vaccinated ) }
  if ("Feeder" %in% ds.names){ data <- cbind( data,"Feeder" = as.character(dataset$Feeder) ) }
  if ("Feeding.Rate.Kg.per.Hour" %in% ds.names){ data <- cbind( data,"Feeding.Rate.Kg.Per.Hour" = round(dataset$Feeding.Rate.Kg.per.Hour, digits = 2) ) }
  if ("Fastings.No" %in% ds.names){ data <- cbind( data,"Fastings.No" = dataset$Fastings.No ) }
  if ("Transfer.Minus.No" %in% ds.names){ data <- cbind( data,"Transfer.Minus.No" = dataset$Transfer.Minus.No ) }
  if ("Transfer.Plus.No" %in% ds.names){ data <- cbind( data,"Transfer.Plus.No" = dataset$Transfer.Plus.No ) }
  if ("Harvest.No" %in% ds.names){ data <- cbind( data,"Harvest.No" = dataset$Harvest.No ) }
  if ("Sampling.No" %in% ds.names){ data <- cbind( data,"Sampling.No" = dataset$Sampling.No ) }
  if ("LTD.Econ.FCR" %in% ds.names){ data <- cbind( data,"LTD.Econ.FCR" = round(dataset$LTD.Econ.FCR, digits = 2) ) }
  if ("LTD.Mortality.No" %in% ds.names){ data <- cbind( data,"LTD.Mortality.No" = round(dataset$LTD.Mortality.No, digits = 2) ) }
  if ("LTD.Mortality.Perc" %in% ds.names){ data <- cbind( data,"LTD.Mortality.Perc" = round(dataset$LTD.Mortality.Perc, digits = 2) ) }
  if ("LTD.Adjustment.No" %in% ds.names){ data <- cbind( data,"LTD.Adjustment.No" = round(dataset$LTD.Adjustment.No, digits = 2) ) }
  if ("LTD.Adjustment.Perc" %in% ds.names){ data <- cbind( data,"LTD.Adjustment.Perc" = round(dataset$LTD.Adjustment.Perc, digits = 2) ) }
  if ("LTD.Adjustment.&.Mortality.No" %in% ds.names){ data <- cbind( data,"LTD.Adjustment.&.Mortality.No" = round(dataset$"LTD.Adjustment.&.Mortality.No", digits = 2) ) }
  if ("LTD.Adjustment.&.Mortality.Perc" %in% ds.names){ data <- cbind( data,"LTD.Adjustment.&.Mortality.Perc" = round(dataset$"LTD.Adjustment.&.Mortality.Perc", digits = 2) ) } 
  if ("PRODUCT.TYPE" %in% ds.names){ data <- cbind( data,"PRODUCT.TYPE" = dataset$PRODUCT.TYPE ) }
  if ("GROUPING.PROD.BGT" %in% ds.names){data <- cbind( data,"GROUPING.PROD.BGT" = dataset$GROUPING.PROD..BGT ) }

  # Filter the dataset
  data <- data %>% filter( (Econ.FCR.Period >= 0.5 & Econ.FCR.Period <= 5.5)  ) %>%
                   filter( (Biol.FCR.Period >= 0.5 & Biol.FCR.Period <= 5.5)  ) %>%
                   filter( Mortality.Perc <= 5 ) %>% filter( LTD.Mortality.Perc <= 10 ) %>%
                   filter( LTD.Econ.FCR >= 0.5 & LTD.Econ.FCR <= 5.5 ) %>%
                   filter( SFR.Period.Perc > 0 ) %>% filter( Diff.Days >= 10 ) %>% 
                   filter( SGR.Period.Perc > 0 )

  return(data)

}

#------------------------------------------------------------------------------------------------
#   Function create colors
#
create_colors <- function(n)
{ 
  if ( n > 12 )
  {
    seq_col_pals = brewer.pal.info[brewer.pal.info$category %in% 'seq',]
    col_vector = unlist(mapply(brewer.pal, seq_col_pals$maxcolors, rownames(seq_col_pals)))
    cols <- sample(col_vector, n)
  }else{
    col_vector <- brewer.pal(12, name = 'Paired')
    cols <- sample(col_vector, n)
  }  
  
  return(cols)
}

#------------------------------------------------------------------------------------------------
#   Function for Histograms
#
fun.histPlot <- function(ds, no.bins, meas, group_var, flag.facet, flag.save)  
{
   if ( no.bins <= 15 ){ 
      range_var = diff(range(as.numeric(ds[,meas])))/no.bins
   }else{
      range_var = diff(range(as.numeric(ds[,meas])))/(no.bins*0.5)
   }
   minx = min(as.numeric(ds[,meas])) 
   maxx = max(as.numeric(ds[,meas])) 
   brks <- round(seq(minx, maxx, range_var),digits=1)
   
   if (group_var!="None")
   {
      cdf <- ddply( ds, group_var, function(df) mean(df[,meas]) )
      colnames(cdf)<-c(group_var, "x.means")
      
      n <- length(unique(ds[,group_var]))
      cols <- create_colors(n)
    
      fig.title <- paste("Histogram of", meas, "grouped by", group_var,"with", no.bins, "bins", sep=" ") 
      h <- ggplot(ds, aes_string(x=meas, color=group_var, fill=group_var)) +
          geom_histogram( aes( y=..density.. ), binwidth=range_var, position="identity", alpha=.85 ) +
          scale_x_continuous(limits=c(minx, maxx), breaks=brks) + scale_fill_manual(values=cols) +  
          geom_vline(data=cdf, aes_string(xintercept="x.means", colour=group_var), linetype="dashed", size=1) + 
          theme_economist(dkpanel=TRUE) + ggtitle( fig.title ) + 
          theme(axis.text.x=element_text(angle=0), legend.position="bottom", 
                legend.title = element_blank(), legend.text=element_text(size=8),  
                plot.title=element_text(family="Tahoma"), text=element_text(family="Tahoma"))
      
      if (flag.facet){
        fmla <- as.formula(paste0("~", group_var))  
        h <- h + facet_wrap(fmla) #, scales="free") 
      }
      
    }else{
      # group_var == "None"
      
      fig.title <- paste("Histogram of", meas, "with", no.bins, "bins", sep=" ") 
      h <- ggplot(ds, aes_string(x=meas)) + geom_histogram( aes( y=..density..,fill=..count.. ), binwidth=range_var ) +
        scale_x_continuous(limits=c(minx, maxx), breaks=brks) + scale_colour_brewer(palette="Set1") +
        geom_vline(aes_string(xintercept=mean(ds[,meas], na.rm=T)), color="red", linetype="dashed", size=1) +
        theme_economist(dkpanel=TRUE) + 
        theme(axis.text.x=element_text(angle=0), legend.position="bottom", legend.title = element_blank(),
              legend.text=element_text(size=8) ) + 
        ggtitle( fig.title ) +
        theme(plot.title=element_text(family="Tahoma"), text=element_text(family="Tahoma"))
    } # end if group_var
   
    # Save the histogram
    if( flag.save ){
         fig.name <- paste("Fig.Histograms", meas, "grouped by", group_var, "no.bins", no.bins, Sys.Date(), "png", sep=".")
         ggsave(fig.name, h, type="cairo-png", width = 20.29, height = 10.58, units="cm")
    }
   
    return(h)
}
#------------------------------------------------------------------------------------------------
#   Function for Density Plots
#
fun.densPlot <- function(ds, meas, group_var, flag.facet, flag.save)  
{
  
  if (group_var!="None")
  {
    cdf <- ddply( ds, group_var, function(df) mean(df[,meas]) )
    colnames(cdf)<-c(group_var, "x.means")
    
    n <- length(unique(ds[,group_var]))
    cols <- create_colors(n)
    
    fig.title <- paste("Density plot of", meas, "grouped by", group_var, sep=" ") 
    
    minx <- min(as.numeric(ds[,meas]))
    maxx <- max(as.numeric(ds[,meas]))
    step <- (maxx - minx)/10
    brks <- round(seq(minx, maxx, step),digits=1)
    
    dp <- ggplot(ds, aes_string(x=meas, color=group_var, fill=group_var)) +
              geom_density(size=1, alpha=0.5) +
              scale_x_continuous(limits=c(minx,maxx), breaks=brks) +  
              geom_vline(data=cdf, aes_string(xintercept="x.means", colour=group_var), linetype="dashed", 
                 size=1) + theme_economist(dkpanel=TRUE) + scale_fill_manual(values=cols) +
              theme(axis.text.x=element_text(angle=0), legend.position="bottom", 
                    legend.title = element_blank(),legend.text=element_text(size=8) ) + ggtitle( fig.title ) +
              theme(plot.title=element_text(family="Tahoma"), text=element_text(family="Tahoma"))
  }else{
    # group_var == "None"
    
    minx <- min(as.numeric(ds[,meas]))
    maxx <- max(as.numeric(ds[,meas]))
    step <- (maxx - minx)/10
    brks <- round(seq(minx, maxx, step),digits=1)
    
    fig.title <- paste("Density plot of", meas, sep=" ") 
    dp <- ggplot(ds, aes_string(x=meas)) + geom_density(aes_string(color=meas), size=1, alpha=0.8) +
              scale_x_continuous(limits=c(minx,maxx), breaks=brks) +
              geom_vline(aes_string(xintercept=mean(ds[,meas], na.rm=T)), color="red", linetype="dashed", size=1) +
              theme_economist(dkpanel=TRUE) +
              theme(axis.text.x=element_text(angle=0), legend.position="bottom", 
                    legend.title = element_blank(), legend.text=element_text(size=8) ) + ggtitle( fig.title ) +
              theme(plot.title=element_text(family="Tahoma"), text=element_text(family="Tahoma", size=6))
  } # end if group_var
  
  if (group_var!="None"){
    if (flag.facet){ 
      fmla <- as.formula(paste0("~", group_var))  
      dp <- dp + facet_wrap(fmla) #, scales="free")
    }
  }
  
  if( flag.save ){
    fig.name <- paste("Fig.Density.Plots", meas, "grouped by", group_var, "png", sep=".")
    ggsave(fig.name, dp, type="cairo-png", width = 20.29, height = 10.58, units="cm")
  }
  
  return(dp)
}

#------------------------------------------------------------------------------------------------
#   Function for BoxPlots
#
fun.boxPlot <- function(ds, meas, group_var, flag.notch, flag.facet, flag.save)  
{
  if (group_var!="None")
  {
      fig.title <- paste("Box plot of", meas, "grouped by", group_var, sep=" ")
      
      n <- length(unique(ds[,group_var]))
      cols <- create_colors(n)
      
      boxp <- ggplot(ds, aes_string(x=group_var, y=meas, fill=group_var)) +
              geom_boxplot(notch = flag.notch, width=0.5) +
              stat_summary(fun.y = mean, geom="point", shape=5, size=4) +
              theme_economist(dkpanel=TRUE) + scale_fill_manual(values=cols) +
              theme(legend.position="bottom", legend.title = element_blank(),
                    legend.text=element_text(size=8)) + ggtitle(fig.title) +
              theme(plot.title=element_text(family="Verdana"), text=element_text(family="Verdana"))
      
              # Rotate the boxplot if it necessary
              if( length(unique(ds[,group_var])) >= 6 & length(unique(ds[,group_var])) <= 10)
              {  
                boxp <- boxp + theme(axis.text.x=element_text(angle=90)) 
              }else if( length(unique(ds[,group_var])) > 10 ){
                boxp <- boxp + coord_flip() 
              }
  
  }else{
      fig.title <- paste("Box plot of", meas, sep=" ") 
      boxp <- ggplot(ds, aes_string(x=1, y=meas)) + 
              geom_boxplot(notch = flag.notch, width=0.5) +
              stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
              theme_economist(dkpanel=TRUE) + #scale_fill_brewer(palette="Set1") +
              theme(legend.position="bottom", legend.title = element_blank(),
                    legend.text=element_text(size=8)) + 
              ggtitle(fig.title) +
              theme(plot.title=element_text(family="Verdana"), text=element_text(family="Verdana"))
  } # end if group_var

  if (group_var!="None"){
    if (flag.facet){ 
      fmla <- as.formula(paste0("~", group_var))  
      boxp <- boxp + facet_wrap(fmla) #, scales="free", ncol=3)
    }
  }
  
  if( flag.save ){
    fig.name <- paste("Fig.BoxPlots", meas, "grouped by", group_var, "png", sep=".")
    ggsave(fig.name, boxp, type="cairo-png", width = 20.29, height = 10.58, units="cm")
  }

  return(boxp)
}

#------------------------------------------------------------------------------------------------
#   Function for Bar Plots
#
fun.barPlot <- function(ds, dimX, group_var, meas, flag.save, flag.sd.se, flag.facet)
{
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  if ( meas != "None")
  {
      if (group_var == "None")
      {
        ds.ij <- ds[, c(dimX, meas)] 
        
        summary.ds.ij <- ddply( ds.ij, dimX, .fun= function(xx,col){ 
                                                    c( Mean = mean(xx[[col]], na.rm=TRUE),
                                                       STD = sd(xx[[col]], na.rm=TRUE),
                                                       N = length2(xx[[col]], na.rm=TRUE))}, meas )
        summary.ds.ij$SEM <- summary.ds.ij$STD / sqrt(summary.ds.ij$N)
      
        fig.title <- paste("Error bars", meas, "per", dimX, sep=" ")  
        xlab <- names(summary.ds.ij)[1]
        ylab <- names(summary.ds.ij)[2]
        
        if (flag.sd.se == "St.Dev")
        {
          scale_y <- ceiling(max(summary.ds.ij$Mean) + max(summary.ds.ij$STD))
          err.bars <- ggplot(summary.ds.ij, aes_string(x = dimX, y = "Mean")) + 
            geom_bar(stat = "identity", position="dodge", fill='steelblue', width=.5) +
            geom_errorbar(aes(ymin = Mean - STD, ymax=Mean + STD), width=.25, color="red") + 
            theme_economist(dkpanel=TRUE) + 
            theme(plot.title=element_text(family="Tahoma"), text=element_text(family="Tahoma")) +
            scale_y_continuous(limits = c(0, scale_y)) + scale_fill_discrete(name=xlab)
        }else{
          scale_y <- ceiling(max(summary.ds.ij$Mean) + max(summary.ds.ij$SEM))
          err.bars <- ggplot(summary.ds.ij, aes_string(x = dimX, y = "Mean")) + 
            geom_bar(stat = "identity", position="dodge", fill='steelblue', width=.5) +
            geom_errorbar(aes(ymin = Mean - SEM, ymax=Mean + SEM), width=.25, color="red") + 
            theme_economist(dkpanel=TRUE) + 
            theme(plot.title=element_text(family="Tahoma"), text=element_text(family="Tahoma")) +
            scale_y_continuous(limits = c(0, scale_y)) + scale_fill_discrete(name=xlab) 
        }  
        
      }else{
        # group_var != "None"
        
        n <- length(unique(ds[,group_var]))
        cols <- create_colors(n)
        
        ds.ij <- ds[, c(dimX, group_var, meas)] 
        
        summary.ds.ij <- ddply(ds.ij, c(dimX, group_var), .fun= function(xx,col){ 
                                                              c( Mean = mean(xx[[col]], na.rm=TRUE),
                                                                 STD = sd(xx[[col]], na.rm=TRUE),
                                                                 N = length2(xx[[col]], na.rm=TRUE))}, meas )
        summary.ds.ij$SEM <- summary.ds.ij$STD / sqrt(summary.ds.ij$N)

        fig.title <- paste("Error bars", meas, "per", dimX, "grouped by", group_var, sep=" ")  
        xlab <- names(summary.ds.ij)[1]
        ylab <- names(summary.ds.ij)[3]
        
        if (flag.sd.se == "St.Dev")
        {
          scale_y <- ceiling(max(summary.ds.ij$Mean) + max(summary.ds.ij$STD))
          err.bars<- ggplot(summary.ds.ij, aes_string(x=dimX, y="Mean", fill=group_var)) +
            geom_bar(position=position_dodge(0.9), stat="identity", width=.5) +
            geom_errorbar(aes(ymin=Mean-STD, ymax=Mean+STD), 
                          width=.25, position=position_dodge(0.9), color="darkblue") +
            theme_economist(dkpanel=TRUE) + # scale_colour_economist() +
            theme(plot.title=element_text(family="Tahoma"), text=element_text(family="Tahoma")) +
            scale_y_continuous(limits = c(0, scale_y)) + scale_fill_discrete(name=group_var) +
            labs(x=xlab, y=ylab) + ggtitle(fig.title)
        }else{
          scale_y <- ceiling(max(summary.ds.ij$Mean) + max(summary.ds.ij$SEM))
          err.bars<- ggplot(summary.ds.ij, aes_string(x=dimX, y="Mean", fill=group_var)) +
            geom_bar(position=position_dodge(0.9), stat="identity", width=.5 ) +
            geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), 
                          width=.25, position=position_dodge(0.9), color="darkblue") +
            theme_economist(dkpanel=TRUE) + # scale_colour_economist() +
            theme(plot.title=element_text(family="Tahoma"), text=element_text(family="Tahoma")) +
            scale_y_continuous(limits = c(0, scale_y)) + scale_fill_discrete(name=group_var) +
            labs(x=xlab, y=ylab) + ggtitle(fig.title)
        } # end if..else flag.sd.se   
        
      } # end if..else group_var
      
      if( length(unique(ds[,dimX])) >= 10 )
      {
        err.bars <- err.bars + theme(axis.text.x=element_text(angle=90)) 
      }
   
      if( flag.save ){
        fig.name <- paste("Fig.BarPlots", dimX, meas, "grouped by", group_var, "png", sep=".")
        ggsave(fig.name, err.bars, type="cairo-png", width = 20.29, height = 10.58, units="cm")
      }
  
      return(err.bars)
  
  }else{
    # meas == 'None' => create barplots or stacked barplots
    if (group_var != "None")
    {
      ds.ij <- as.data.frame(ds[, c(dimX, group_var)])
      names(ds.ij) <- c(dimX, group_var)
      
      n <- length(unique(ds[,group_var]))
      cols <- create_colors(n)

      counts.perc.ds <- ds.ij %>% group_by_( dimX, group_var ) %>% summarise(Count=n()) %>%
                                  mutate(Percentage = round(Count*100/sum(Count),1),
                                         pos = cumsum(Percentage) - (0.5 * Percentage))
      names(counts.perc.ds) <- c(dimX, group_var, "Count", "Percentage","pos" )

      fig.title <- paste("Percentage of", dimX, "per", group_var, sep=" ")

      bpfan <- ggplot(counts.perc.ds, aes_string(y = "Percentage", x = dimX, fill = group_var )) +
                      geom_bar(stat="identity", width=.5) +
                      geom_text(aes(x = counts.perc.ds[1], y = pos, label = paste0(Percentage,"%")),
                                colour="black", family="Tahoma", size=4) +
                      theme_economist(dkpanel=TRUE) + scale_fill_manual(values=cols) +
                      theme(plot.title=element_text(family="Tahoma"), text=element_text(family="Tahoma")) +
                      scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
                      labs(x=dimX, y="Percentage") + ggtitle(fig.title)

      bpfan <- bpfan + theme(legend.position="bottom", legend.title = element_blank(), 
                             legend.text=element_text(size=8) )
      
    }else{
      # group_var = 'None'
      ds.ij <- as.data.frame(ds[, dimX])
      names(ds.ij) <- c(dimX)
        
      counts.perc.ds <- ds.ij %>% group_by_( dimX ) %>% summarise(Count=n()) %>%
                                   mutate(Percentage = round(Count*100/sum(Count),1),
                                          pos = Percentage - (0.5 * Percentage))
 
      names(counts.perc.ds) <- c(dimX, "Count", "Percentage","pos" )
      
      fig.title <- paste("Percentage of", dimX, sep=" ")

      bpfan <- ggplot(counts.perc.ds, aes_string(y = "Percentage", x = dimX)) + 
                        geom_bar(stat="identity", width =.5, fill='steelblue') +
                        geom_text(aes(x = counts.perc.ds[1], y = pos, label = paste0(Percentage,"%")),
                                  colour="black", family="Tahoma", size=4) +
                        theme_economist(dkpanel=TRUE) + # scale_fill_brewer(palette="Set1") +
                        theme(plot.title=element_text(family="Tahoma"), text=element_text(family="Tahoma")) +
                        scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
                        labs(x=dimX, y="Percentage") + ggtitle(fig.title)
        
      bpfan <- bpfan + theme(legend.position="bottom", legend.title = element_blank(), 
                             legend.text=element_text(size=8) )
    } # end if...else group_var
    
    if( length(unique(ds[,dimX])) >= 10 )
    {
      bpfan <- bpfan + theme(axis.text.x=element_text(angle=90)) 
    }
    
    if( flag.save ){
      fig.name <- paste("Fig.BarPlots", dimX,"grouped by", group_var, "png", sep=".")
      ggsave(fig.name, bpfan, type="cairo-png", width = 20.29, height = 10.58, units="cm")
    }
    return(bpfan)
    
    
  } # end if..else meas
  
}

#------------------------------------------------------------------------------------------------
#   Function for Scatter plots
#
fun.scatterPlot <- function(ds, dimX, dimY, Size, group_var, regr.method, flag.save)
{
  
  if (group_var == "None")
  {
        fig.title <- paste("Scatter plot of", dimX, "VS", dimY, sep=" ")
        scp <- ggplot(ds, aes_string(x=dimX, y=dimY, size=Size)) +
               geom_point(shape = 21) +
               scale_size_area(max_size = 15) + 
               scale_x_continuous(name=dimX) + scale_y_continuous(name=dimY) +
               theme_economist(dkpanel=TRUE) + ggtitle(fig.title) + scale_fill_brewer(palette="Set1")
               theme(legend.position = "bottom", legend.direction = "horizontal",
                     legend.box = "horizontal",
                     legend.key.size = unit(1, "cm"), 
                     axis.line.x = element_line(size=1, colour = "black", linetype=1),
                     plot.title = element_text(family="Tahoma"),
                     text = element_text(family = "Tahoma"),
                     axis.title = element_text(size = 12),
                     legend.text = element_text(size = 9),
                     legend.title = element_text(face = "bold", size = 9))  
        
        # Add regression line       
        if ( regr.method != "None" ){       
            scp <- scp + geom_smooth(method = regr.method, size = 1)
        } 
               
  }else{
        n <- length(unique(ds[,group_var]))
        cols <- create_colors(n)
    
        fig.title <- paste("Scatter plot of", dimX, "VS", dimY, "per", group_var, sep=" ")
        scp <- ggplot(ds, aes_string(x=dimX, y=dimY, color=group_var, size=Size, fill=group_var)) +  #, fill=group_var, size=group_var
               geom_point(shape = 21) + scale_size_area(max_size = 15) + 
               scale_x_continuous(name=dimX) + scale_y_continuous(name=dimY) +
               theme_economist(dkpanel=TRUE) + scale_fill_manual(values=cols) +
               ggtitle(fig.title) +
               theme(legend.position = "bottom", legend.direction = "horizontal",
                      legend.box = "horizontal",
                      legend.key.size = unit(1, "cm"), 
                      axis.line.x = element_line(size=1, colour = "black", linetype=1),
                      plot.title = element_text(family="Tahoma"),
                      text = element_text(family = "Tahoma"),
                      axis.title = element_text(size = 12),
                      legend.text = element_text(size = 9),
                      legend.title = element_text(face = "bold", size = 9)) 
        
        # Add regression line       
        if ( regr.method != "None" ){       
          scp <- scp + geom_smooth(method = regr.method, size = 1)
        } 
       
    }
    
    if( flag.save ){
      fig.name <- paste("Fig.ScatterPlots", dimX, "VS", dimY, "grouped by", group_var, "png", sep=".")
      ggsave(fig.name, scp, type="cairo-png", width = 20.29, height = 10.58, units="cm")
    }
  
  return(scp)
  
} 

#----------------------------------------------------------------------------------
##                 Summary Mutlivariate Statistics function
##
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
##
sum_stats <- function(data, measurevar, groupvars, na.rm=FALSE, conf.interval=.95, .drop=TRUE, flag.save)
{
  
  if ( groupvars != "None" ){
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x))
      else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, min, max, mean, median, sd, CV, kurtosis, skewness, Q1, Q3, IR, se and ci
    data_stats <- ddply(data, groupvars, .drop=.drop,
                        .fun = function(xx, col) {
                          c(N      = length2(xx[[col]], na.rm=na.rm),
                            min    = min(xx[[col]], na.rm=na.rm),
                            max    = max(xx[[col]], na.rm=na.rm),
                            mean   = mean(xx[[col]], na.rm=na.rm),
                            median = median(xx[[col]], na.rm=na.rm),
                            sd     = sd(xx[[col]], na.rm=na.rm),
                            CV     = ( sd(xx[[col]], na.rm=na.rm)/mean(xx[[col]], na.rm=na.rm) )*100,
                            kurtosis = kurtosis(xx[[col]], na.rm=na.rm),
                            skewness = skewness(xx[[col]], na.rm=na.rm),
                            Q1       = quantile(xx[[col]], 1/4, na.rm=na.rm, names=FALSE),
                            Q3       = quantile(xx[[col]], 3/4, na.rm=na.rm, names=FALSE),
                            IR       = IQR(xx[[col]], na.rm=na.rm)
                          )
                        },
                        measurevar
    )
 
    data_stats$se <- data_stats$sd / sqrt(data_stats$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, data_stats$N-1)
    data_stats$ci <- data_stats$se * ciMult
    
  }else{
    
    # This does the summary for all records of the data set at the variable measurevar 
    # N, min, max, mean, median, sd, CV, kurtosis, skewness, Q1, Q3, IR, se and ci
    
    ds <- data[,measurevar]
    data_stats <- data.frame(N     = length(ds),
                             min   = min(ds, na.rm=na.rm),
                             max    = max(ds, na.rm=na.rm),
                             mean   = mean(ds, na.rm=na.rm),
                             median = median(ds, na.rm=na.rm),
                             sd     = sd(ds, na.rm=na.rm),
                             CV     = ( sd(ds, na.rm=na.rm)/mean(ds, na.rm=na.rm) )*100,
                             kurtosis = kurtosis(ds, na.rm=na.rm),
                             skewness = skewness(ds, na.rm=na.rm),
                             Q1       = quantile(ds, 1/4, na.rm=na.rm, names=FALSE),
                             Q3       = quantile(ds, 3/4, na.rm=na.rm, names=FALSE),
                             IR       = IQR(ds, na.rm=na.rm)
    )
    
    
    data_stats$se <- data_stats$sd / sqrt(data_stats$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, data_stats$N-1)
    data_stats$ci <- data_stats$se * ciMult
    
    rownames(data_stats)<-"Total"
    
  }
  
  return(data_stats)
  
}
#----------------------------------------------------------------------------------
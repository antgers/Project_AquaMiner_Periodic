library(plotly)

source("helpers.R")

dset <- read_excel("Bass_Galaxidi.xlsx", sheet = 1, col_names = TRUE)

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

ds <- data.frame(data$From, data$Batch, data$Unit, data$Econ.FCR.Period, data$SFR.Period.Perc,  
                 data$SGR.Period.Perc)
colnames(ds) <- c("Date", "Batch", "Unit", "Econ.FCR.Period", "SFR.Period.Perc", "SGR.Period.Perc")


choice.batch <- "1404 Λ03 D2.GL2"
dim.x <- "Date"


ds1 <- ds[order(as.Date(ds$Date)),] %>% filter( Batch == choice.batch)

x1 <- ds1[,dim.x]

plot_ly(ds1, x = x1, y = Econ.FCR.Period, mode = "lines + markers", name = "Econ.FCR.Period") %>% 
  add_trace(x = x1, y = SFR.Period.Perc, name = "SFR.Period.Perc") %>% 
  add_trace(x = x1, y = SGR.Period.Perc, name = "SGR.Period.Perc") %>% 
  layout(
    title = " Period Econ.FCR vs SFR and SGR ",
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
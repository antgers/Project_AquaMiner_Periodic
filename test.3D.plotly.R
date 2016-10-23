library(plot3D)
library(plotly)
library(readxl)


predictors.vars <- c("Avg.Temp", "End.Av.Weight")
response.var <- "Biol.FCR.Period"

KPI.DF <- read.csv("C:/Users/gerasimos/OneDrive/src_JavaToR/KPI_Tables/Results_Folder_2016-10-20_134502/FCR.RawData.csv", header=FALSE)
KPI.DF <- KPI.DF[,3:5]
names(KPI.DF) <- c(predictors.vars, response.var)

source("helpers.R")
ds <- read_excel("Samplings_Anemokambi.xlsx", sheet = 1, col_names = TRUE)

new.names <- names(ds)
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

colnames(ds) <- new.names
dset <- create.dataset(ds)

dset <- dset[, names(dset) %in% c(predictors.vars,response.var)]

# x-axis = Avg.Temp, y-axis = End.Av.Weight
x.vals <- unique( as.numeric( KPI.DF[, predictors.vars[1]]) )
y.vals <- unique( as.numeric( KPI.DF[, predictors.vars[2]]) )
M <- mesh(x.vals, y.vals)

nr <- length(x.vals)
nc <- length(y.vals)

zvar <- as.numeric(KPI.DF[, response.var])
KPI.DF[, response.var] <- round(zvar, digits=3 )
mat <- matrix(zvar, nrow = nr, ncol = nc, byrow = TRUE)

# format 3D plot
#
f <- list(
  family = "Courier New, monospace",
  size = 14,
  color = "#7f7f7f"
)

fig.title <- paste(response.var, "table per", predictors.vars[1], "and", predictors.vars[2], sep=" ")

# x-axis = Avg.Temp
ax1.range <- range(x.vals)
ax1.lab <- list(range=ax1.range, # tickmode="array", tickvals=x.vals, ticktext=as.character(x.vals), 
                # nticks = nr-1, showline=T, 
                title = predictors.vars[1], titlefont = f) 

# y-axis = End.Av.Weight
ax2.range <- range(y.vals)
ax2.lab <- list(range=ax2.range, #tickmode="array",tickvals=y.vals, ticktext=as.character(y.vals),
                #      nticks = nc, showline=T, 
                title = predictors.vars[2], titlefont = f)

# Z = "KPI" axis
z.lab <- list(title = response.var, titlefont = f)

cust.scene = list(xaxis = ax1.lab, yaxis = ax2.lab, zaxis = z.lab)

p <- plot_ly(x = M$x, y = M$y, z = mat, type = "surface") %>% 
  add_trace(data = dset, x = dset[, predictors.vars[1]],
            y = dset[, predictors.vars[2]], z = dset[, response.var],
            mode = "markers", type = "scatter3d",
            marker = list(size = 3, color = "red", symbol = 104)) %>%
  layout(title = fig.title, scene=cust.scene, width = 800, height = 600) 

print(p)



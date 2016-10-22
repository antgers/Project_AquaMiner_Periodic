source("helpers.R")

#dset <- read_excel("Bass_Galaxidi.xlsx", sheet = 1, col_names = TRUE)

dset <- read_excel("Samplings_Anemokambi.xlsx", sheet = 1, col_names = TRUE)

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

# response <- "Econ.FCR.Period"
# predictors <- c("Start.Fish.Density","Diff.Days","Start.Av.Weight", "Av.Temp") 
# dummy.fmla <- as.formula(paste(response, paste(predictors, collapse="+"), sep="~" ))
# 
# preproc.dummy.dset.train <- data[, names(data) %in% c(response, predictors)]
# 
# set.seed(123)
# reps = 10
# kfolds = 5
# times = reps*kfolds
# 
# nr <- nrow(preproc.dummy.dset.train)
# perc <- 0.75
# 
# seeds <- vector(mode = "list", length = times)
# 
# for(i in 1:times) seeds[[i]] <- sample.int(nr, round(nr*perc,digits=1))
# 
# # For the last model:
# seeds[[times + 1]] <- sample.int(nr, 1)
# 
# set.seed(1)
# 
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = kfolds,
#   ## repeated ten times
#   repeats = reps) #, seeds = seeds)
# 
# #glmnet.model <- train(dummy.fmla, data=preproc.dummy.dset.train, method = "glmboost", trControl = fitControl,
# #                      na.action=na.omit, metric="RMSE")
# 
# #rf.model <- train(dummy.fmla, data=preproc.dummy.dset.train, method = "rf", trControl = fitControl,
# #                  na.action=na.omit, metric="RMSE")
# 
# marsGrid <- expand.grid(degree = 1:2, nprune = (1:4) * 10)
# mars.model <- train(dummy.fmla, data=preproc.dummy.dset.train, method = "earth", tuneGrid = marsGrid,
#                     trControl = fitControl, metric="RMSE", na.action=na.omit)
# 
# vi.mars <- varImp(mars.model$finalModel, scale = FALSE)

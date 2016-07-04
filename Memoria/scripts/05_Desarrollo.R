## ---- setupR ----
require(Matrix)
require(ggplot2)
require(data.table)
require(xgboost)
require(knitr)
require(parallel)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



## ---- transformReplay ----
# It's supossed we have the correct features
transformReplay <- function(replayID, dataset, auc = FALSE) {
    
  data.replay <- dataset[dataset$ReplayID == replayID,]

  features <- c("Minerals","Gas","Supply","TotalMinerals","TotalGas","TotalSupply",
                "GroundUnitValue","BuildingValue","AirUnitValue",
                "ObservedEnemyGroundUnitValue","ObservedEnemyBuildingValue","ObservedEnemyAirUnitValue",
                "ObservedResourceValue")

  if (auc)
    f <- function(feature) {
      A.value <- MESS::auc(x = data.replay[,"Frame"], y = data.replay[,feature])
      B.value <- MESS::auc(x = data.replay[,"Frame"], y = data.replay[,paste0("Enemy",feature)])
      res <- c(A.value, B.value)
      names(res) <- c(feature, paste0("Enemy",feature))
      res
    }
  else
    f <- function(feature) {
      A.value <- lm(paste0(feature," ~ Frame"), data = data.replay)
      B.value <- lm(paste0("Enemy",feature," ~ Frame"), data = data.replay)
      res <- c(A.value$coefficients[2], B.value$coefficients[2])
      names(res) <- c(feature, paste0("Enemy",feature))
      res
    }

  result <- unlist(lapply(features, f))

  result$Max.Frame <- max(data.replay$Frame)
  result$ReplayID <- replayID
  result$Duration <- unique(dataset[dataset$ReplayID == replayID,"Duration"])
  result$Winner <- unique(dataset[dataset$ReplayID == replayID,"Winner"])

  return(result)

}


## ---- transformData ----
transformData <- function(data, max.frame = NA, auc = FALSE) {
  replays <- unique(data[,"ReplayID"])
  if (is.na(max.frame))
    data.subset <- data
  else
    data.subset <- data[data$Frame <= max.frame,]

  cl <- makeCluster(getOption("cl.cores", 4))
  replays.list <- parLapply(cl,replays, transformReplay, dataset = data.subset, auc = auc)
  stopCluster(cl)
  
  # replays.list <- lapply(replays, transformReplay, dataset = data.subset, auc = auc)

  data.transformed <- rbindlist(replays.list)
  data.transformed

}




## ---- readData ----
data.pvp <- read.csv("../datos/data_pvp.csv", colClasses=c("integer",rep("numeric",28),"factor"))
data.pvt <- read.csv("../datos/data_pvt.csv", colClasses=c("integer",rep("numeric",28),"factor"))
data.pvz <- read.csv("../datos/data_pvz.csv", colClasses=c("integer",rep("numeric",28),"factor"))
data.tvt <- read.csv("../datos/data_tvt.csv", colClasses=c("integer",rep("numeric",28),"factor"))
data.tvz <- read.csv("../datos/data_tvz.csv", colClasses=c("integer",rep("numeric",28),"factor"))
data.zvz <- read.csv("../datos/data_zvz.csv", colClasses=c("integer",rep("numeric",28),"factor"))
data.full <- rbind(cbind(data.pvp, Races = "PvP"), cbind(data.pvt, Races = "PvT"),
                   cbind(data.pvz, Races = "PvZ"), cbind(data.tvt, Races = "TvT"),
                   cbind(data.tvz, Races = "TvZ"), cbind(data.zvz, Races = "ZvZ"))

data.full$ReplayID  <- as.factor(paste(data.full$Races,data.full$ReplayID,sep = "_"))

rm(data.pvp)
rm(data.pvt)
rm(data.pvz)
rm(data.tvt)
rm(data.tvz)
rm(data.zvz)

metadata <- data.full[, colnames(data.full) %in% c("ReplayID", "Duration", "Races")]
metadata <- unique(metadata)

# system.time(data.full.transformed <- transformData(data.full))
# system.time(data.full.transformed.auc <- transformData(data.full, auc = T))

## ---- replaysHistogram ----
gg <- ggplot(data=metadata) +
  geom_bar(aes(x=ReplayID,y=Duration,fill=Races), stat="identity", width = 0.75) +
  geom_hline(yintercept = mean(metadata$Duration), color = "red",linetype="dashed") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
gg

## ---- replayRaceHistogram
gg.facet <- ggplot(data=metadata) +
  geom_bar(aes(x=ReplayID,y=Duration,fill=Races), stat="identity") +
  geom_hline(yintercept = mean(metadata$Duration), color = "red",linetype="dashed", size=.1) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  facet_grid(Races ~ .)
gg.facet

## --- cleanReplays ----
frame.bound <- 75000
data.full.bound <- data.full[data.full$Duration <= frame.bound,]
output.label <- as.numeric(data.full.bound[,"Winner"] == "A")
# data.clean.transformed <- data.full.transformed[data.full.transformed$Duration <= frame.bound,]
# data.clean.transformed.auc <- data.full.transformed.auc[data.full.transformed.auc$Duration <= frame.bound,]
metadata.bound <- metadata[metadata$Duration <= frame.bound,]

gg <- ggplot(data=metadata.bound) +
  geom_bar(aes(x=ReplayID,y=Duration,fill=Races), stat="identity", width = 0.75) +
  geom_hline(yintercept = mean(metadata.bound$Duration), color = "red",linetype="dashed") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
gg

## ---- CVTest ----
max_frame <- 75000
positive.labels <- mean(as.numeric(data.subset.transformed[,Winner]=="A"))
negative.labels <- mean(as.numeric(data.subset.transformed[,Winner]=="B"))

data.subset.transformed <- transformData(data.full.clean, max.frame = max_frame)
xgb.data.regression <- xgb.DMatrix(data = as.matrix(data.subset.transformed[,!c("Winner", "ReplayID", "Max.Frame", "Duration"), with=F] ), 
                        label = as.numeric(data.subset.transformed[,Winner] == "A"))
cv.res.regression <- xgb.cv(data = xgb.data.regression, 
                            max.depth = 4, 
                            scale_pos_weight = negative.labels / positive.labels,
                            # max_delta_step = 1,
                            gamma = 1,
                            # min_child_weight = 3,
                            subsample = 0.5,
                            colsample_bytree = 0.5,
                            silent = 0,
                            # alpha = 0.01,
                            lambda = 1.5,
                            # eta = 0.001, 
                            nthread = 4, nround = 1000, objective = "binary:logistic", nfold = 10)

## ---- CVAUC ----
max_frame <- 75000
data.subset.transformed.auc <- transformData(data.full, max.frame = max_frame, auc = T)
xgb.data.auc <- xgb.DMatrix(data = as.matrix(data.subset.transformed.auc[,!c("Winner", "ReplayID", "Max.Frame", "Duration"), with=F] ), 
                        label = as.numeric(data.subset.transformed.auc[,Winner] == "A"))
cv.res.auc <- xgb.cv(data = xgb.data.auc, max.depth = 24, silent = 1,
                 eta = 0.1, nthread = 4, nround = 30,objective = "binary:logistic", nfold = 10)

## ---- ImportanceReg ----
xgb.model.regression <- xgboost(data = xgb.data, max.depth = 10,
               eta = 0.1, nthread = 4, nround = 30,objective = "binary:logistic")

importance.matrix.regression <- xgb.importance(colnames(data.full.transformed[,!c("Winner", "ReplayID", "Max.Frame", "Duration"), with=F] ), model = xgb.model.regression)
xgb.plot.importance(importance.matrix.regression)

## ---- ImportanceAUC ----
xgb.model.auc <- xgboost(data = xgb.data.auc, max.depth = 10,
               eta = 0.1, nthread = 4, nround = 30,objective = "binary:logistic")

importance.matrix.auc <- xgb.importance(colnames(data.full.transformed.auc[,!c("Winner", "ReplayID", "Max.Frame", "Duration"), with=F] ), model = xgb.model.auc)
xgb.plot.importance(importance.matrix.auc)

## ---- ImportanceFull ----
# data.full.clean <- data.full[,!c("Winner", "ReplayID", "Races"), with=F] 
data.full.clean <- data.full.bound[, !colnames(data.full.bound) %in% c("Duration","Winner","ReplayID","Races")]
xgb.data.full <- xgb.DMatrix(data = data.matrix(data.full.clean), 
                             label = output.label)
# rm(data.full)
# rm(data.full.bound)
cv.res.full <- xgb.cv(data = xgb.data.full, 
                      max.depth = 32, 
                      # scale_pos_weight = negative.labels / positive.labels,
                      # max_delta_step = 1,
                      # gamma = 1,
                      # min_child_weight = 3,
                      # subsample = 0.5,
                      # colsample_bytree = 0.5,
                      silent = 0,
                      # alpha = 0.01,
                      # lambda = 1.5,
                      # eta = 0.001, 
                      nthread = 4, nround = 10, objective = "binary:logistic", nfold = 5)

model.full <- xgboost(data = xgb.data.full,
                      max.depth = 24,
                      silent = 0,
                      nthread = 4, nround = 5, objective = "binary:logistic")

importance_matrix <- xgb.importance(colnames(data.full[, !colnames(data.full) %in% c("Duration","Winner","ReplayID","Races")] ), model = model.full)
xgb.plot.importance(importance_matrix)

## ---- ImportanceMean ----
data.mean <- data.full.bound[data.full.bound$Frame <= mean(metadata$Duration),]
output.label.mean <- as.numeric(data.mean[,"Winner"] == "A")
data.mean.clean <- data.mean[, !colnames(data.full.bound) %in% c("Duration","Winner","ReplayID","Races")]
xgb.data.mean <- xgb.DMatrix(data = data.matrix(data.mean.clean), 
                             label = output.label.mean)
# rm(data.full)
# rm(data.full.bound)
cv.res.mean <- xgb.cv(data = xgb.data.mean, 
                      max.depth = 32, 
                      # scale_pos_weight = negative.labels / positive.labels,
                      # max_delta_step = 1,
                      # gamma = 1,
                      # min_child_weight = 3,
                      # subsample = 0.5,
                      # colsample_bytree = 0.5,
                      silent = 0,
                      # alpha = 0.01,
                      # lambda = 1.5,
                      # eta = 0.001, 
                      nthread = 4, nround = 10, objective = "binary:logistic", nfold = 5)

model.mean <- xgboost(data = xgb.data.mean,
                      max.depth = 32,
                      silent = 0,
                      nthread = 4, nround = 10, objective = "binary:logistic")

importance_matrix <- xgb.importance(model = model.mean)
xgb.plot.importance(importance_matrix)


## ---- PrepareDMatrixFiles ----
mean.duration <- mean(metadata.bound$Duration)
data.mean <- data.full.bound[data.full.bound$Frame <= mean.duration,]
data.mean.clean <- data.mean[, !colnames(data.mean) %in% c("Duration","Winner","ReplayID","Races")]
output.label.mean <- output.label[data.full.bound$Frame <= mean.duration]
feature.names <- colnames(data.mean.clean)
saveRDS(feature.names, file = "../datos/features.rds")
sapply(c(100,75,50,25,20,10), function(fraction){
                                filename <- paste0("../datos/xgb.data.",fraction,".mean.data")
                                if (!file.exists(paste0(filename,".train"))){
                                  print(paste("No existe",filename))
                                  data.bound <- data.mean.clean[data.mean.clean$Frame <= fraction / 100 * mean.duration,]
                                  output.label.bound <- output.label.mean[data.mean.clean$Frame <= fraction / 100 * mean.duration]
                                  index <- sample(1:nrow(data.bound), 0.8*nrow(data.bound))
                                  data.train <- data.bound[index,]
                                  data.test <- data.bound[-index,]
                                  output.train <- output.label.bound[index]
                                  output.test <- output.label.bound[-index]
                                  xgb.data.train <- xgb.DMatrix(data = data.matrix(data.train[,!colnames(data.train) %in% c("Duration","Winner","ReplayID","Races")]),
                                                          label = output.train )
                                  xgb.DMatrix.save(xgb.data.train, paste0(filename,".train"))
                                  
                                  xgb.data.test <- xgb.DMatrix(data = data.matrix(data.test[,!colnames(data.test) %in% c("Duration","Winner","ReplayID","Races")]),
                                                               label = output.test)
                                  xgb.DMatrix.save(xgb.data.test, paste0(filename,".test"))
                                }
})

## ---- ProcessDMatrix ----
xgb.params <- list(max.depth = 32,
               silent = 0,
               nthread = 4,
               objective = "binary:logistic")

filenames.train <- list.files(path = "../datos", pattern = "xgb.data.*.mean.data.train", full.names = TRUE)
filenames.test <- list.files(path = "../datos", pattern = "xgb.data.*.mean.data.test", full.names = TRUE)
# filenames.train <- c("../datos/xgb.data.20.mean.data.train","../datos/xgb.data.10.mean.data.train")
# filenames.test <- c("../datos/xgb.data.20.mean.data.test","../datos/xgb.data.10.mean.data.test")
xgb.models <- lapply(filenames.train, function(filename){
                    xgb.data.train <- xgb.DMatrix(filename)
                    cv.res <- xgb.cv(data = xgb.data.train,
                                     params = xgb.params,
                                     nround = 10,
                                     nfold = 5)
                    write.csv(cv.res, paste0(filename,".csv"), row.names = FALSE)
                    xgb.model <- xgboost(data = xgb.data.train,
                                         params = xgb.params,
                                         nround = 10)
                    xgb.model
})

xgb.test.error <- NULL
for (i in 1:length(filenames.test)){
  filename <- filenames.test[i]
  model <- xgb.models[[i]]
  xgb.data.test <- xgb.DMatrix(filename)
  pred <- predict(model, xgb.data.test)
  err <- mean(as.numeric(pred > 0.5) != getinfo(xgb.data.test,"label"))
  xgb.test.error <- c(xgb.test.error, err)
}

feature.names <- loadRDS("../datos/features.rds")
xgb.importance.plots <- lapply(xgb.models, function(model){
                                          xgb.plot.importance(xgb.importance(feature_names = feature.names,
                                                         model = model))
})



## ---- PlotError ----
df.example <- read.csv("cv.res.mean.csv")
df.train <- df[,c(1,2)]
df.test <- df[,-c(1,2)]
colnames(df.test) <- c("Mean","Std","Set")
colnames(df.train) <- c("Mean","Std","Set")
df.full <- rbind(df.train,df.test)
df.full$Round <- rep(1:10,2)


error.filenames <- list.files(path = "../datos", pattern = "xgb.data.*.mean.data.train.csv", full.names = TRUE)
error.plots <- lapply(error.filenames, function(filename){
                                        fraction <- unique(na.omit(as.numeric(unlist(strsplit(unlist(filename), "[^0-9]+")))))
                                        df <- read.csv(filename)
                                        df.train <- df[,c(1,2)]
                                        df.train$Set <- "Train"
                                        df.test <- df[,-c(1,2)]
                                        df.test$Set <- "Test"
                                        colnames(df.test) <- c("Mean","Std","Set") 
                                        colnames(df.train) <- c("Mean","Std","Set")
                                        df.full <- rbind(df.train,df.test)
                                        df.full$Round <- as.factor(rep(1:10,2))
                                        gg <- ggplot(data=df.full, aes(x=Round, y = Mean, fill = Set))
                                        gg <- gg + geom_bar(stat='identity', position=position_dodge())
                                        gg <- gg + geom_errorbar(aes(ymin=Mean-Std, ymax = Mean+Std), width = 0.2, position = position_dodge(width=0.9))
                                        gg <- gg + ggtitle(paste0(fraction,"% of the mean of the duration"))
                                        gg <- gg + ylim(0, 0.4) + ylab("CV Error")
})

do.call("multiplot", error.plots)
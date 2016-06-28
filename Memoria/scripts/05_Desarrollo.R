## ---- setupR ----
require(Matrix)
require(ggplot2)
require(data.table)
require(xgboost)
require(knitr)
require(parallel)



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
print(getwd())
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
data.full.bound <- data.full[data.full$Duration <= 75000,]
output.label <- as.numeric(data.full.bound[,"Winner"] == "A")

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

## ---- testgg ----
set.seed(1234566)
ggplot(data.frame(x = runif(100), y= runif(100)), aes(x,y)) + geom_point() +
  theme(panel.grid.major = element_line(color = "gray60", size = 0.8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

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
data.full.clean <- data.full[data.full$Duration <= frame.bound,]
data.clean.transformed <- data.full.transformed[data.full.transformed$Duration <= frame.bound,]
data.clean.transformed.auc <- data.full.transformed.auc[data.full.transformed.auc$Duration <= frame.bound,]
metadata.clean <- metadata[metadata$Duration <= frame.bound,]

gg <- ggplot(data=metadata.clean) +
  geom_bar(aes(x=ReplayID,y=Duration,fill=Races), stat="identity", width = 0.75) +
  geom_hline(yintercept = mean(metadata$Duration), color = "red",linetype="dashed") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
gg

## ---- CV ----
result_list <- sapply(seq(min(data.full$Duration)/2,max(data.full$Duration),1000),
                      function(max_frame) {
                        data.subset <- data.full[data.full$Frame < max_frame, !colnames(data.full) %in% c("ReplayID")]
                        sparse_matrix <- sparse.model.matrix(Winner ~ .-1, data = data.subset)
                        output_vector = data.subset[,"Winner"] == "A"
                        cv.res <- xgb.cv(data = sparse_matrix, label = output_vector, max.depth = 5, silent = 1,
                                       eta = 0.1, nthread = 4, nround = 10,objective = "binary:logistic", nfold = 10)
                        print(paste("Frame",max_frame,"/",max(data.full$Duration)))
                        return(cv.res)
                      })

## ---- CVMax ----
result_list <- sapply(seq(max(data.full$Duration),max(data.full$Duration),1000),
                      function(max_frame) {
                        print(paste("Frame",max_frame,"/",max(data.full$Duration)))
                        data.subset <- data.full[data.full$Frame < max_frame, !colnames(data.full) %in% c("ReplayID")]
                        sparse_matrix <- sparse.model.matrix(Winner ~ .-1, data = data.subset)
                        output_vector = data.subset[,"Winner"] == "A"
                        cv.res <- xgb.cv(data = sparse_matrix, label = output_vector, max.depth = 5, silent = 1,
                                         eta = 0.1, nthread = 4, nround = 10,objective = "binary:logistic", nfold = 10)

                        return(cv.res)
                      })

## ---- CVtransformed ----
result_list <- sapply(seq(min(data.full$Duration)/2,max(data.full$Duration),1000),
                      function(max_frame) {
                        print(paste("Frame",max_frame,"/",max(data.full$Duration)))
                        print("Procesando...")
                        data.subset.transformed <- transformData(data.full, max.frame = max_frame)
                        xgb.data <- xgb.DMatrix(data = as.matrix(data.subset.transformed[,!c("Winner", "ReplayID", "Max.Frame", "Duration"), with=F] ), 
                                    label = as.numeric(data.subset.transformed[,Winner] == "A"))
                        print("Entrenando...")
                        cv.res <- xgb.cv(data = xgb.data, max.depth = 5, silent = 1,
                                         eta = 0.1, nthread = 4, nround = 10,objective = "binary:logistic", nfold = 10)

                        return(cv.res)
                      })

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
rm(data.full)
rm(data.full.bound)
cv.res.full <- xgb.cv(data = xgb.data.full, 
                      max.depth = 6, 
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
                      nthread = 4, nround = 5, objective = "binary:logistic", nfold = 5)

model.full <- xgboost(data = xgb.data.full,
                     max.depth = 24,
                     silent = 0,
                     nthread = 4, nround = 5, objective = "binary:logistic")

importance_matrix <- xgb.importance(colnames(data.full[, !colnames(data.full) %in% c("Duration","Winner","ReplayID","Races")] ), model = model.full)
xgb.plot.importance(importance_matrix)

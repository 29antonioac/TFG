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

  cl <- makeCluster(getOption("cl.cores", 7))
  replays.list <- parLapply(cl,replays, transformReplay, dataset = data.subset, auc = auc)
  stopCluster(cl)
  
  # replays.list <- lapply(replays, transformReplay, dataset = data.subset, auc = auc)

  data.transformed <- rbindlist(replays.list)
  data.transformed

}




## ---- readData ----
print(getwd())
data.pvp <- read.csv("../datos/data_pvp.csv")
data.pvt <- read.csv("../datos/data_pvt.csv")
data.pvz <- read.csv("../datos/data_pvz.csv")
data.tvt <- read.csv("../datos/data_tvt.csv")
data.tvz <- read.csv("../datos/data_tvz.csv")
data.zvz <- read.csv("../datos/data_zvz.csv")
data.full <- rbind(cbind(data.pvp, Races = "PvP"), cbind(data.pvt, Races = "PvT"),
                   cbind(data.pvz, Races = "PvZ"), cbind(data.tvt, Races = "TvT"),
                   cbind(data.tvz, Races = "TvZ"), cbind(data.zvz, Races = "ZvZ"))

data.full$ReplayID  <- as.factor(paste(data.full$Races,data.full$ReplayID,sep = "_"))
setDT(data.full)

metadata <- data.full[, colnames(data.full) %in% c("ReplayID","Duration", "Races")]
metadata <- unique(metadata[order(-metadata$Duration),])
metadata <- transform(metadata, ReplayID=reorder(ReplayID, -Duration) ) 

system.time(data.full.transformed <- transformData(data.full))
system.time(data.full.transformed.auc <- transformData(data.full, auc = T))

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
gg <- ggplot(data=metadata) +
  geom_bar(aes(x=levels(ReplayID),y=Duration,fill=Races), stat="identity", width = 0.75) +
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
gg.facet <- ggplot(data=metadata) +
  geom_bar(aes(x=levels(ReplayID),y=Duration,fill=Races), stat="identity") +
  geom_hline(yintercept = mean(metadata$Duration), color = "red",linetype="dashed", size=.1) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  facet_grid(Races ~ .)
gg.facet

## ---- CV ----
result_list <- sapply(seq(min(data.full$Duration)/2,max(data.full$Duration),1000),
                      function(max_frame) {
                        data.subset <- data.full[data.full$Frame < max_frame, !colnames(data.full) %in% c("ReplayID")]
                        sparse_matrix <- sparse.model.matrix(Winner ~ .-1, data = data.subset)
                        output_vector = data.subset[,"Winner"] == "A"
                        cv.res <- xgb.cv(data = sparse_matrix, label = output_vector, max.depth = 5, silent = 1,
                                       eta = 0.1, nthread = 6, nround = 10,objective = "binary:logistic", nfold = 10)
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
                                         eta = 0.1, nthread = 6, nround = 10,objective = "binary:logistic", nfold = 10)

                        return(cv.res)
                      })

## ---- CVtransformed ----
result_list <- sapply(seq(min(data.full$Duration)/2,max(data.full$Duration),1000),
                      function(max_frame) {
                        print(paste("Frame",max_frame,"/",max(data.full$Duration)))
                        print("Procesando...")
                        data.full.transformed <- transformData(data.full, max.frame = max_frame)
                        data.subset <- data.full.transformed[, !colnames(data.full) %in% c("ReplayID")]
                        sparse_matrix <- sparse.model.matrix(Winner ~ .-1, data = data.subset)
                        output_vector = data.subset[,"Winner"] == "A"
                        print("Entrenando...")
                        cv.res <- xgb.cv(data = sparse_matrix, label = output_vector, max.depth = 5, silent = 1,
                                         eta = 0.1, nthread = 6, nround = 10,objective = "binary:logistic", nfold = 10)

                        return(cv.res)
                      })

## ---- CVTest ----
sparse_matrix <- sparse.model.matrix(Winner ~ .-ReplayID, data = data.full.transformed)
output_vector <- data.full.transformed$Winner == "A"
cv.res <- xgb.cv(data = sparse_matrix, label = output_vector, max.depth = 10, silent = 1,
                 eta = 0.1, nthread = 8, nround = 100,objective = "binary:logistic", nfold = 10)

## ---- CVAUC ----
sparse_matrix <- sparse.model.matrix(Winner ~ .-ReplayID, data = data.full.transformed.auc)
output_vector <- data.full.transformed.auc$Winner == "A"
cv.res <- xgb.cv(data = sparse_matrix, label = output_vector, max.depth = 10, silent = 1,
                 eta = 0.1, nthread = 8, nround = 100,objective = "binary:logistic", nfold = 10)

## ---- ImportanceReg ----
xgb.data <- xgb.DMatrix(data = as.matrix(data.full.transformed[,!c("Winner", "ReplayID", "Max.Frame", "Duration"), with=F] ), 
                        label = as.numeric(data.full.transformed[,Winner] == "A"))
bst <- xgboost(data = xgb.data, max.depth = 10,
               eta = 0.1, nthread = 8, nround = 10,objective = "binary:logistic")

importance_matrix <- xgb.importance(colnames(data.full.transformed[,!c("Winner", "ReplayID", "Max.Frame", "Duration"), with=F] ), model = bst)
xgb.plot.importance(importance_matrix)

## ---- ImportanceAUC ----
xgb.data <- xgb.DMatrix(data = as.matrix(data.full.transformed.auc[,!c("Winner", "ReplayID", "Max.Frame", "Duration"), with=F] ), 
                        label = as.numeric(data.full.transformed.auc[,Winner] == "A"))
bst <- xgboost(data = xgb.data, max.depth = 10,
               eta = 0.1, nthread = 8, nround = 10,objective = "binary:logistic")

importance_matrix <- xgb.importance(colnames(data.full.transformed.auc[,!c("Winner", "ReplayID", "Max.Frame", "Duration"), with=F] ), model = bst)
xgb.plot.importance(importance_matrix)

## ---- ImportanceFull ----
data.full.clean <- data.full[,!c("Winner", "ReplayID", "Races"), with=F] 
xgb.data <- xgb.DMatrix(data = data.matrix(sapply(data.full.clean,as.numeric )), 
                        label = as.numeric(data.full.clean[,Winner] == "A"))
bst <- xgboost(data = xgb.data, max.depth = 10,
               eta = 0.1, nthread = 8, nround = 10,objective = "binary:logistic")

importance_matrix <- xgb.importance(colnames(data.full[,!c("Winner", "ReplayID"), with=F] ), model = bst)
xgb.plot.importance(importance_matrix)

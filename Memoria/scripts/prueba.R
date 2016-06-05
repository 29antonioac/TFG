## ---- readData
data <- read.csv("../Datos/data.csv")
metadata <- unique(data[, colnames(data) %in% c("ReplayID","Duration")])
# clean_data <- data[,!(colnames(data) %in% c("ReplayID","Duration"))]
# data_replay <- data[data$ReplayID==114, ]
# data_replay <- data_replay[, !(colnames(data_replay) %in% c("ReplayID","Duration"))]
# winner <- data_replay[1,"Winner"]
# loser <- if(winner=='A') 'B' else 'A'

## ---- CV
result_list <- sapply(seq(min(clean_data$Duration)/2,max(clean_data$Duration),1000),
                      function(max_frame) {
                        data_subset <- clean_data[clean_data$Frame < max_frame]
                        sparse_matrix <- sparse.model.matrix(Winner ~ .-1, data = data_subset)
                        output_vector = data_subset[,"Winner"] == "A"
                        cv.res <- xgb.cv(data = sparse_matrix, label = output_vector, max.depth = 48, silent = 1,
                                       eta = 0.1, nthread = 8, nround = 100,objective = "binary:logistic", nfold = 10)
                        return(cv.res)
                      })

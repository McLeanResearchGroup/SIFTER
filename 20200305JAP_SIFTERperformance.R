library(randomForest)
library(pROC)
all <- read.csv("~/Box Sync/R_Scripts&Data/20171218JAP_iceberg/R-output/allMASTER.csv", header = TRUE, stringsAsFactors = FALSE)

sb.full <- all[, c("Compound", "InChiKey", "Kingdom", "Super.Class", "Class","Subclass", "mz", "CCS")]
sb <- sb.full %>% group_by(Subclass) %>% filter(n() >= 10) #filters for classes with n >= 10
sb$Subclass <- ifelse(sb$Subclass == "", NA, sb$Subclass)
sb <- na.omit(sb)
sb$km <- sb$mz/1.0011178
sb$nkm <- floor(sb$km)
sb$kmd <- sb$nkm - sb$km

# train and test set
# test.row <- sample(nrow(sb), 0.15*(nrow(sb)))
# test <- sb[test.row,]
# train <- sb[-test.row,]
train <- sb[, c("Compound", "InChiKey", "Kingdom", "Super.Class", "Class","Subclass", "mz", "CCS", "kmd")]
train[, c("Kingdom", "Super.Class", "Class", "Subclass")] <- lapply(train[, c("Kingdom", "Super.Class", "Class", "Subclass")], as.factor)
train[, c("mz", "CCS", "kmd")] <- lapply(train[, c("mz", "CCS", "kmd")], as.numeric)

#method build
rf.k <- randomForest(Kingdom ~ mz + CCS + kmd, data = train, ntree = 500, mtry = 3, importance=TRUE, norm.votes=TRUE, proximity=TRUE, keep.forest=TRUE)
rf.s <- randomForest(Super.Class ~ mz + CCS + kmd + Kingdom, data = train, ntree = 100, mtry = 4, importance=TRUE, norm.votes=TRUE, proximity=TRUE, keep.forest=TRUE)
rf.c <- randomForest(Class ~ mz + CCS + kmd + Kingdom + Super.Class, data = train, ntree = 100, mtry = 4.5, importance=TRUE, norm.votes=TRUE, proximity=TRUE, keep.forest=TRUE, nodesize=2)
rf.sb <- randomForest(Subclass ~ mz + CCS + kmd + Kingdom + Super.Class + Class, data = train, ntree = 1000, mtry = 5, importance=TRUE, norm.votes=TRUE, proximity=TRUE, keep.forest=TRUE, nodesize=2)

rf.k.roc <- roc(train$Kingdom, rf.k$votes[,2])
plot(rf.k.roc)
rf.k.auc <- auc(rf.k.roc)
plot(rf.k.auc)

rf.s.roc <- roc(train$Super.Class, rf.s$votes[,2])
plot(rf.s.roc)
rf.s.auc <- auc(rf.s.roc)
plot(rf.s.auc)

rf.c.roc <- roc(train$Class, rf.c$votes[,2])
plot(rf.c.roc)
rf.c.auc <- auc(rf.c.roc)
plot(rf.c.auc)

rf.sb.roc <- roc(train$Subclass, rf.sb$votes[,2])
plot(rf.sb.roc)
rf.sb.auc <- auc(rf.sb.roc)
plot(rf.sb.auc)

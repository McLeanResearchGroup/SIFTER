#20200120JAP_SIFTERforUnknowns
#J.A. Picache

all <- read.csv("20190425JAP_SIFTER/testSetData/_TJC/allMASTER_dummy.csv", header = TRUE, stringsAsFactors = FALSE)

TJCdata <- read.csv("20190425JAP_SIFTER/testSetData/_TJC/_TJCdata.csv")
test <- TJCdata[, c(1, 4, 6, 11:14)]
test[, c(2:3)] <- lapply(test[,c(2,3)], as.numeric)
test.predict <- test[, c(2:3)]
colnames(test.predict) <- c("mz", "CCS")
#need df of values to predict
test.predict$km <- test.predict$mz/1.0011178
test.predict$nkm <- floor(test.predict$km)
test.predict$kmd <- test.predict$nkm - test.predict$km
colnames(test.predict) <- c("mz", "CCS","km", "nkm", "kmd")
test.predict <- cbind(test.predict[, c("mz", "CCS", "kmd")])

#kingdom
test.predict[,"Kingdom"] <- predict(rf.k, test.predict[,c("mz", "CCS", "kmd")])
res.k <- predict(rf.k, test.predict[,c("mz", "CCS", "kmd")], type = "prob")
res.k <- as.data.frame(cbind(as.character(test.predict$Kingdom), res.k))
colnames(res.k)[1] <- "Predicted.Kingdom"
test.predict[, "prob.k"] <- as.numeric(apply(res.k[2:ncol(res.k)], 1, max))
write.csv(res.k, "~/Box Sync/R_Scripts&Data/20190425JAP_SIFTER/testSetData/_TJC/20200304JAP_compendium_kingdom_result.csv")

#superClass
test.predict[,"Super.Class"] <- predict(rf.s, test.predict[,c("mz", "CCS", "kmd", "Kingdom")])
res.s <- predict(rf.s, test.predict[,c("mz", "CCS", "kmd", "Kingdom")], type = "prob")
res.s <- as.data.frame(cbind(as.character(test.predict$Super.Class), res.s))
colnames(res.s)[1] <- "Predicted.Super.Class"
test.predict[, "prob.s"] <- as.numeric(apply(res.s[2:ncol(res.s)], 1, max))
write.csv(res.s, "~/Box Sync/R_Scripts&Data/20190425JAP_SIFTER/testSetData/_TJC/20200304JAP_compendium_superclass_result.csv")

#class
test.predict[,"Class"] <- predict(rf.c, test.predict[,c("mz", "CCS", "kmd", "Kingdom", "Super.Class")])
res.c <- predict(rf.c, test.predict[,c("mz", "CCS", "kmd", "Kingdom", "Super.Class")], type = "prob")
res.c <- as.data.frame(cbind(as.character(test.predict$Class), res.c))
colnames(res.c)[1] <- "Predicted.Class"
test.predict[, "prob.c"] <- as.numeric(apply(res.c[2:ncol(res.c)], 1, max))
write.csv(res.c, "~/Box Sync/R_Scripts&Data/20190425JAP_SIFTER/testSetData/_TJC/20200304JAP_compendium_class_result.csv")

#subclass
test.predict[,"Subclass"] <- predict(rf.sb, test.predict[,c("mz", "CCS", "kmd", "Kingdom", "Super.Class", "Class")])
res.sb <- predict(rf.sb, test.predict[,c("mz", "CCS", "kmd", "Kingdom", "Super.Class", "Class")], type = "prob")
res.sb <- as.data.frame(cbind(as.character(test.predict$Subclass), res.sb))
colnames(res.sb)[1] <- "Predicted.Subclass"
test.predict[, "prob.sb"] <- as.numeric(apply(res.sb[2:ncol(res.sb)], 1, max))
write.csv(res.sb, "~/Box Sync/R_Scripts&Data/20190425JAP_SIFTER/testSetData/_TJC/20200304JAP_compendium_subclass_result.csv")

#prediction results
write.csv(test.predict, "~/Box Sync/R_Scripts&Data/20190425JAP_SIFTER/testSetData/_TJC/20200304JAP_probAnalysis.csv")

#confusion matrices
mat.k <- as.matrix(rf.k[["confusion"]])
mat.k.var <- as.data.frame(importance(rf.k))
write.csv(mat.k, "~/Box Sync/R_Scripts&Data/20190425JAP_SIFTER/testSetData/_TJC/20200304JAP_compendium_kingdom_errors.csv")
mat.s <- as.matrix(rf.s[["confusion"]])
mat.s.var <- as.data.frame(importance(rf.s))
write.csv(mat.s, "~/Box Sync/R_Scripts&Data/20190425JAP_SIFTER/testSetData/_TJC/20200304JAP_compendium_superclass_errors.csv")
mat.c <- as.matrix(rf.c[["confusion"]])
mat.c.var <- as.data.frame(importance(rf.c))
write.csv(mat.c, "~/Box Sync/R_Scripts&Data/20190425JAP_SIFTER/testSetData/_TJC/20200304JAP_compendium_class_errors.csv")
mat.sb <- as.matrix(rf.sb[["confusion"]])
mat.sb.var <- as.data.frame(importance(rf.sb))
write.csv(mat.sb, "~/Box Sync/R_Scripts&Data/20190425JAP_SIFTER/testSetData/_TJC/20200304JAP_compendium_subclass_errors.csv")

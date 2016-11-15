
library(lme4)


covNames <- c("age",                     "are.caretaker",           "deep.brain.stimulation", 
              "diagnosis.year",          "education",               "employment",             
              "gender",                  "healthcare.provider",     "maritalStatus",          
              "medical.usage.yesterday", "medication.start.year",   "onset.year",             
              "past.participation",      "phone.usage",             "smartphone",             
              "smoked") 


## remove NAs (randomForest function does not handle NAs)
##
rdat <- na.omit(dat[, c("healthCode", "medTimepoint", "tod", featNames, covNames)])


## run random forest on original data no demographics
##
set.seed(12345)
rf0 <- RunRandomForests(nRuns = 30, dat = rdat, respName = "medTimepoint", 
                        featNames = c(featNames, covNames), 
                        positiveClassName = "Just after Parkinson medication (at your best)",
                        negativeClassName = "Immediately before Parkinson medication")


## run random forest on original data no demographics
##
set.seed(12345)
rf1 <- RunRandomForests(nRuns = 30, dat = rdat, respName = "medTimepoint", featNames = c(featNames), 
                        positiveClassName = "Just after Parkinson medication (at your best)",
                        negativeClassName = "Immediately before Parkinson medication")


## scale the data before computing PCs
##
sdat <- ScaleData(rdat, featNames = featNames)


## compute PCs
##
pcs <- princomp(sdat[, featNames])


## create data.frame with PCs
##
pcdat <- data.frame(sdat[, c("healthCode", "medTimepoint")], pcs$scores)


## run random forest on principal components
##
set.seed(12345)
rf2 <- RunRandomForests(nRuns = 30, dat = pcdat, respName = "medTimepoint", featNames = colnames(pcs$scores), 
                        positiveClassName = "Just after Parkinson medication (at your best)",
                        negativeClassName = "Immediately before Parkinson medication")


## Fit mixed effect logistic regression on the PC data 
##
nms <- colnames(pcs$scores)
fixedForm <- paste("medTimepoint ~ ", paste(nms, collapse = " + "), sep = "")
randomForm <- " + (1 | healthCode)"
myFormula <- as.formula(paste(fixedForm, randomForm, sep = ""))

fm1 <- glmer(myFormula, pcdat, binomial)
su1 <- summary(fm1)
su1





ca <- 1
cm <- 1
cl <- 1

fileName <- paste(figPath, paste(paste("rf_all_variables", activity, sep = "_"), "pdf", sep = "."), sep = "")
pdf(fileName, width = 10, height = 8)
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1) + 0.1, mgp = c(2, 0.75, 0))
boxplot(data.frame(rf0$Auc, rf0$pAuc), cex.axis = ca, cex.lab = cl, cex.main = cm,
        names = c("original", "permuted"), ylab = "AUC",
        main = "original features", horizontal = FALSE,
        ylim = c(0.4, 0.9))
par(mar = c(5, 8, 2, 1) + 0.1)
plotDat0 <- ShapeDataForBoxplot(t(rf0$Imp), sort.by = "median", rev = FALSE)
boxplot(plotDat0, horizontal = TRUE, las = 2, xlab = "Importance", 
        main = "Importance plot", cex = 0.5)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))
dev.off()


fileName <- paste(figPath, paste(paste("rf_orig_and_pc", activity, sep = "_"), "pdf", sep = "."), sep = "")
pdf(fileName, width = 10, height = 8)
par(mfrow = c(1, 4), mar = c(5, 4, 2, 1) + 0.1, mgp = c(2, 0.75, 0))
boxplot(data.frame(rf1$Auc, rf1$pAuc), cex.axis = ca, cex.lab = cl, cex.main = cm,
        names = c("original", "permuted"), ylab = "AUC",
        main = "original features", horizontal = FALSE,
        ylim = c(0.4, 0.9))
boxplot(data.frame(rf2$Auc, rf2$pAuc), cex.axis = ca, cex.lab = cl, cex.main = cm,
        names = c("original", "permuted"), ylab = "AUC",
        main = "principal components", horizontal = FALSE,
        ylim = c(0.4, 0.9))
par(mar = c(5, 8, 2, 1) + 0.1)
plotDat1 <- ShapeDataForBoxplot(t(rf1$Imp), sort.by = "median", rev = FALSE)
boxplot(plotDat1, horizontal = TRUE, las = 2, xlab = "Importance", 
        main = "Importance plot", cex = 0.5)
plotDat2 <- ShapeDataForBoxplot(t(rf2$Imp), sort.by = "median", rev = FALSE)
boxplot(plotDat2, horizontal = TRUE, las = 2, xlab = "Importance", 
        main = "Importance plot", cex = 0.5)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))
dev.off()



fileName <- paste(figPath, paste(paste("glmm_estimates_pvals", activity, sep = "_"), "pdf", sep = "."), sep = "")
pdf(fileName, width = 12, height = 6)
par(mfrow = c(1, 2), mar = c(5, 3, 2, 1) + 0.1)
barplot(su1$coefficients[, 1], las = 2, main = "estimates")
barplot(-log(su1$coefficients[, 4], 10), las = 2, main = "-log10(p-value)")
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()


sortedImp <- sort(apply(rf2$Imp, 1, median), decreasing = TRUE)
top <- names(sortedImp)

fileName <- paste(figPath, paste(paste("loadings_top_pcs_from_rf", activity, sep = "_"), "pdf", sep = "."), sep = "")
pdf(fileName, width = 12, height = 8)
par(mfrow = c(2, 2),mar = c(8, 4, 2, 2) + 0.1)
barplot(pcs$loadings[, top[1]], las = 2, main = top[1])
barplot(pcs$loadings[, top[2]], las = 2, main = top[2])
barplot(pcs$loadings[, top[3]], las = 2, main = top[3])
barplot(pcs$loadings[, top[4]], las = 2, main = top[4])
par(mfrow = c(1, 1),mar = c(5, 4, 4, 2) + 0.1)
dev.off()



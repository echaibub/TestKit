
library(nlme)


ranForm <- as.formula(" ~ 1 | healthCode")
pvalsMix <- rep(NA, length(featNames))
names(pvalsMix) <- featNames
for (i in seq(length(featNames))) {
  #cat(i, "\n")
  respName <- featNames[i]
  fixForm <- as.formula(paste(respName, " ~ medTimepoint", sep = ""))
  try(fm <- lme(fixForm, data = na.omit(dat[, c(respName, "medTimepoint", "healthCode")]), random = ranForm, method = "ML"), silent = TRUE)
  if (!inherits(fm, "try-error")) {
    pvalsMix[i] <- anova(fm)[2, 4] 
  }
}


fileName <- paste(figPath, paste(paste("feature_by_feature_mixed_model_pvalues", activity, sep = "_"), "pdf", sep = "."), sep = "")
pdf(fileName, width = 12, height = 8)
pvalsMix2 <- pvalsMix
pvalsMix2[pvalsMix2 == 0] <- 1e-16 ## replace 0 p-values by small but positive value to avoid error in plotting 
par(mar = c(8, 4, 4, 2) + 0.1)
barplot(-log(pvalsMix2, 10), las = 2, ylab = "-log10(p-value)")
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()



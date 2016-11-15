
## Get demographics data
##
cat("getting demographics table data", "\n")
demoTb <- synTableQuery("SELECT * FROM syn5511429")
dim(demoTb@values)
demo <- demoTb@values
names(demo) <- make.names(names(demo))


## Get activity table data
##
cat(paste("getting", activity, "table data", sep = " "), "\n")
if (activity == "tap") {
  activityTable <- synTableQuery(paste("SELECT * FROM", activityTableSynId, "WHERE \"tapping_results.json.TappingSamples\" is not null", sep = " "))
}
if (activity == "rest") {
  activityTable <- synTableQuery(paste("SELECT * FROM", activityTableSynId, "WHERE \"deviceMotion_walking_rest.json.items\" is not null", sep = " "))
}
if (activity == "walk") {
  activityTable <- synTableQuery(paste("SELECT * FROM", activityTableSynId, "WHERE \"deviceMotion_walking_outbound.json.items\" is not null", sep = " "))
}
if (activity == "voice") {
  activityTable <- synTableQuery(paste("SELECT * FROM", activityTableSynId, "WHERE \"audio_audio.m4a\" is not null", sep = " "))
}
if (activity == "memory") {
  activityTable <- synTableQuery(paste("SELECT * FROM", activityTableSynId, "WHERE \"MemoryGameResults.json.MemoryGameGameRecords\" is not null", sep = " "))
}


## Get activity features data
##
cat(paste("getting", activity, "features data", sep = " "), "\n")
activityFile <- synGet(activityFeaturesSynId)
if (activity == "tap") {
  datFeat <- read.delim(getFileLocation(activityFile), sep = "\t", stringsAsFactors = FALSE, row.names = "tapping_results.json.TappingSamples")
}
if (activity == "rest") {
  datFeat <- read.delim(getFileLocation(activityFile), sep = "\t", stringsAsFactors = FALSE, row.names = "deviceMotion_walking_rest.json.items")
}
if (activity == "walk") {
  datFeat <- read.delim(getFileLocation(activityFile), sep = "\t", stringsAsFactors = FALSE, row.names = "deviceMotion_walking_outbound.json.items")
}
if (activity == "voice") {
  datFeat <- read.delim(getFileLocation(activityFile), sep = "\t", stringsAsFactors = FALSE, row.names = "row.id")
}
if (activity == "voice2") {
  datFeat <- read.delim(getFileLocation(activityFile), sep = "\t", stringsAsFactors = FALSE, row.names = "row.id")
}
if (activity == "memory") {
  datFeat <- read.delim(getFileLocation(activityFile), sep = "\t", stringsAsFactors = FALSE, row.names = "MemoryGameResults.json.MemoryGameGameRecords")
}

## Get subset of the feature data corresponding to recordIds in the table data
##
dat <- datFeat[datFeat$recordId %in% activityTable@values$recordId,] 



## Merge the features data with demographics table data
##
cat("merging with demographics data", "\n")
dat <- MergeDemoWithFeatures(demo = demo, feat = dat)


## Convert character variables to factors (for the sake of the random forest fit)
##
dat <- ToFactor(dat)




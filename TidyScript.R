# Run the analysis -> creates the tiny data set and saves it under "tiny.txt"

# Read files

readFiles <- function(filePath, filteredFeatures, features) {
        cols_width <- rep(-16, length(features))
        cols_width[filteredFeatures] <- 16
        rawData <- read.fwf(
                file=filePath,
                widths=cols_width,
                col.names=features[filteredFeatures])
}

# Reads an additional file 

readAdditionalFile <- function(dataDirectory, filePath) {
        filePathTest <- paste(dataDirectory, "/test/", filePath, "_test.txt", sep="")
        filePathTrain <- paste(dataDirectory, "/train/", filePath, "_train.txt", sep="")
        data <- c(read.table(filePathTest)[,"V1"], read.table(filePathTrain)[,"V1"])
        data
}

# Tidy feature names

tidyFeatureName <- function(featureName) {
        featureName <- gsub("\\(", "", featureName)
        featureName <- gsub("\\)", "", featureName)
        featureName
}

# Read sets

readSets <- function(dataDirectory) {
        # Add train and test files
        featuresPath <- paste(dataDirectory, "/features.txt", sep="")
        features <- read.table(featuresPath)[,"V2"]
        filteredFeatures <- sort(union(grep("mean\\(\\)", features), grep("std\\(\\)", features)))
        features <- correctFeatureName(features)
        set <- readBaseSet(paste(dataDirectory, "/test/X_test.txt", sep=""), filteredFeatures, features)
        set <- rbind(set, readBaseSet(paste(dataDirectory, "/train/X_train.txt", sep=""), filteredFeatures, features))
        
        # Add subjects
        set$subject <- readAdditionalFile("UCI HAR Dataset", "subject")
        
        # Add activities
        activitiesFilePath <- paste(dataDirectory, "/activity_labels.txt", sep="")
        activities <- read.table(activitiesFilePath)[,"V2"]
        set$activity <- activities[readAdditionalFile("UCI HAR Dataset", "y")]
        
        set
}

# From set, create tidy dataset

createSummaryDataset <- function(dataDirectory) {
        sets <- readSets(dataDirectory)
        sets_x <- sets[,seq(1, length(names(sets)) - 2)]
        summary_by <- by(sets_x,paste(sets$subject, sets$activity, sep="_"), FUN=colMeans)
        summary <- do.call(rbind, summary_by)
        summary
}

dataDirectory <- "UCI HAR Dataset"
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        tmp_file <- "./temp.zip"
        download.file(url,tmp_file, method="curl")
        unzip(tmp_file, exdir="./")
        unlink(tmp_file)

summary <- createSummaryDataset(dataDirectory)
write.table(summary, "tidy.txt")

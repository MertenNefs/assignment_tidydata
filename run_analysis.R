run_analysis<- function(){
      
##Merges the training and the test sets to create one data set.
trainValues <- read.table("UCI HAR Dataset/train/X_train.txt")
trainLabels <- read.table("UCI HAR Dataset/train/y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")

testValues <- read.table("UCI HAR Dataset/test/X_test.txt")
testLabels <- read.table("UCI HAR Dataset/test/y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")

## adding a rownumber
trainValues$rowname <- as.numeric(rownames(trainValues))
trainLabels$rowname <- as.numeric(rownames(trainLabels))
trainSubjects$rowname <- as.numeric(rownames(trainSubjects))
testValues$rowname <- as.numeric(rownames(testValues))
testLabels$rowname <- as.numeric(rownames(testLabels))
testSubjects$rowname <- as.numeric(rownames(testSubjects))

##merging the training and teset data
trainings <- merge(trainLabels, trainSubjects, by.x = "rowname",by.y = "rowname")
trainData <- merge(trainings, trainValues, by.x = "rowname",by.y = "rowname")
tests <- merge(testLabels, testSubjects, by.x = "rowname",by.y = "rowname")
testData <- merge(tests, testValues, by.x = "rowname",by.y = "rowname")
totalData <- rbind(trainData,testData)

## Extracts only the measurements on the mean and standard deviation for each measurement.
library(dplyr)
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
key <- grep("mean|std", features$V2) ## makes vector containing variable names including 'mean' or 'std'
remove <- grep("Freq|gravityMean", features$V2) ##excluding Freqmean and gravityMean
key<- key[!key %in% remove] ##eliminating the removal vector from the key vector
      key1<- key+3 ##compensating the vector for the three included columns
      key2<- append(key1,1:3,after=0) ##compensating the vector for the three included columns
selectedData <- totalData[, key2] ##selected data


## Uses descriptive activity names to name the activities in the data set
##1 WALKING 2 WALKING_UPSTAIRS3 WALKING_DOWNSTAIRS4 SITTING5 STANDING6 LAYING
selectedData$activity[selectedData$V1.x == 1] <- "walking"
selectedData$activity[selectedData$V1.x == 2] <- "upstairs"
selectedData$activity[selectedData$V1.x == 3] <- "downstairs"
selectedData$activity[selectedData$V1.x == 4] <- "sitting"
selectedData$activity[selectedData$V1.x == 5] <- "standing"
selectedData$activity[selectedData$V1.x == 6] <- "laying"

## Appropriately labels the data set with descriptive variable names.
names <- features[key,2]
names <- append(names, c("observation","activitynr","subject"), after=0)
names <- append(names, "activity", after=69)
colnames(selectedData)<- names ##full column headers vector

## creates a second, independent tidy data set with the average of each variable for each activity and each subject.
selectedData %>% group_by(subject,activity) -> groupedData ##group data by subject + activity
summarize_all(groupedData,mean)->summarizedData ##summarize grouped data - mean of all variables
arrange(summarizedData, subject,activitynr)-> summarizedData ##arranging in ascending order
tidyData<- summarizedData[,-(3:4)] ##selecting tidy data columns
write.table(tidyData, "tidyData.txt", row.names = FALSE) ##writing tidy data file

}
## Import Data
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")

##Add activity data and subject data to main data
activityTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
activityTrain <- read.table ("./UCI HAR Dataset/train/y_train.txt")

subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")

subjectActivityTest <- cbind(activityTest, subjectTest)
subjectActivityTrain <- cbind(activityTrain, subjectTrain)

combinedTest <- cbind(subjectActivityTest, testData)
combinedTrain <- cbind(subjectActivityTrain, trainData)

##Turn into Data Frame
dfTest <- data.frame(combinedTest)
dfTrain <- data.frame(combinedTrain)

## Merge data
library(dplyr) 
mergedData <- merge(dfTest, dfTrain, all = TRUE) ##default setting for merge is to merge on all common names

##Add Features to Column Names
columnNames <- read.table("./UCI HAR Dataset/features.txt")
features <- as.character(columnNames[,2])
features2 <- as.character(features)       ##character vector of column names 

                        ##selectedNames <- select(columnNames, V2)     ##subsets our row names from DF
activitySaved <- select(mergedData, V1)      ##saves order of activity Data for split
subjectSaved <- select(mergedData, V1.1)     ##saves order of subject data for split
nonActivity <- select(mergedData, V1.2:V561) ## selects all of the data series except for activity and subject


##Extract only the mean and standard deviation measurements 
meanStdColumns <- grep("mean[^Freq]|std", features2) ## subsets our characters with either "mean" or "std" but not meanFreq

subsetData <- mergedData[,meanStdColumns] ##subsets columns by numbers in the vector

cNames <- grep("mean[^Freq]|std", features2, value = TRUE) ##gets the column names with std and mean but not meanFreq

forNames <- nonActivity[,meanStdColumns]    ## filter out mean/std columns 
colnames(forNames) <- cNames             ##assign names to columns
namedData <- cbind(activitySaved,forNames) ## add activity column back in 
namedData <- rename(namedData, Activity = V1)  ##rename first column activity

namedSubjectData <- cbind(subjectSaved,namedData)  ##add subject column back in 
namedSubjectData <- rename(namedSubjectData, Subject = V1.1) ##rename first column Subject

##Use descriptive activity names to name activity in data set
namedSubjectData$Activity[namedData$Activity == "1"] <- "walking"
namedSubjectData$Activity[namedData$Activity == "2"] <- "walking_upstairs"
namedSubjectData$Activity[namedData$Activity == "3"] <- "walking_downstairs"
namedSubjectData$Activity[namedData$Activity == "4"] <- "sitting"
namedSubjectData$Activity[namedData$Activity == "5"] <- "standing"
namedSubjectData$Activity[namedData$Activity == "6"] <- "laying"

##From the data set in step 4, create a second, independent tidy data set with the average of each variable for each 
##activity and each subject.
install.packages("reshape2") ##automatically installs dependency "plyr"as well 
library(reshape2)  ##load package into R

meltedData <- melt(namedSubjectData, id = c("Activity","Subject"), measure.vars = cNames) ##make the data molten 
dt2 <- dcast(meltedData, Activity + Subject ~ variable, mean) ##Cast it back out into tidy data requirements

write.table(dt2, file = "tidyr2.txt")  ##write out the table into a text file


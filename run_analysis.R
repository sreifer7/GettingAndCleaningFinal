## Import Data
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")

##Add activity data to main data
activityTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
activityTrain <- read.table ("./UCI HAR Dataset/train/y_train.txt")

combinedTest <- cbind(activityTest, testData)
combinedTrain <- cbind(activityTrain, trainData)

##Turn into Data Frame
dfTest <- data.frame(combinedTest)
dfTrain <- data.frame(combinedTrain)

## Merge data
library(dplyr) 
mergedData <- merge(trainData, testData, all = TRUE) ##default setting for merge is to merge on all common names

##Add Features to Column Names
columnNames <- read.table("./UCI HAR Dataset/features.txt")
features <- as.character(columnNames[,2])
features2 <- as.character(features) ##character vector of column names 

                        ##selectedNames <- select(columnNames, V2)     ##subsets our row names from DF
activitySaved <- select(mergedData, V1)      ##saves order of activity Data for split
nonActivity <- select(mergedData, V1.1:V561) ## selects all of the data series except for activity

##Extract only the mean and standard deviation measurements 
meanStdColumns <- grep("mean[^Freq]|std", features2) ## subsets our characters with either "mean" or "std" but not meanFreq

subsetData <- mergedData[,meanStdColumns] ##subsets columns by numbers in the vector

cNames <- grep("mean[^Freq]|std", features2, value = TRUE) ##gets the column names with std and mean but not meanFreq

forNames <- nonActivity[,meanStdColumns]    ## filter out mean/std columns 
columnNames(forNames) <- cNames             ##assign names to columns
namedData <- cbind(activitySaved,forNames) ## add activity column back in 
namedData <- rename(namedData, Activity = V1)  ##rename first column activity

##Use descriptive activity names to name activity in data set
namedData$Activity[namedData$Activity == "1"] <- "walking"
namedData$Activity[namedData$Activity == "2"] <- "walking_upstairs"
namedData$Activity[namedData$Activity == "3"] <- "walking_downstairs"
namedData$Activity[namedData$Activity == "4"] <- "sitting"
namedData$Activity[namedData$Activity == "5"] <- "standing"
namedData$Activity[namedData$Activity == "6"] <- "laying"

##From the data set in step 4, create a second, independent tidy data set with the average of each variable for each 
##activity and each subject.
install.packages("reshape2") ##automatically installs dependency "plyr"as well 
library(reshape2)  ##load package into R

meltedData <- melt(namedData, id = "Activity", measure.vars = cNames) ##make the data molten 
dt2 <- dcast(meltedData, Activity ~ variable, mean)

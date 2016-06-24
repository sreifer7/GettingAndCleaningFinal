**Data:** 
Data was taken from the UCI Machine Learning Repository

**Abstract:**
Human Activity Recognition Database built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smart phone with embedded inertial sensors. 

**Source:**
Jorge L. Reyes-Ortiz(1,2), Davide Anguita(1), Alessandro Ghio(1), Luca Oneto(1) and Xavier Parra(2)
1 - Smartlab - Non-Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova, Genoa (I-16145), Italy. 
2 - CETpD - Technical Research Centre for Dependency Care and Autonomous Living
Universitat Politècnica de Catalunya (BarcelonaTech). Vilanova i la Geltrú (08800), Spain
activityrecognition '@' smartlab.ws

**Description:**
There were 30 volunteers for the experiment between the ages of 19-48. Every volunteer performed the six daily activities (walking, walking_upstairs, walking_downstairs, sitting, standing, laying) while wearing the smart phone (a Samsung Galaxy S II) on their waist while the phone recorded the required measurements.

**Variables:**
	Variables were sorted to only the mean and standard deviation measurements of each measurement that was taken. 
	Then it was averaged and sorted by activity.

**Values:**
trainData: File name that holds all the train file data in R
testData: File name that holds all the test file data in R

activityTest: File name that holds activity codes for the Test data set
activityTrain: File name that holds activity codes for the Train data set

mergedData: File name that holds the merger of the two above data sets


**Transformations of All Data**

##Add activity data to main data
activityTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
activityTrain <- read.table ("./UCI HAR Dataset/train/y_train.txt")

combinedTest <- cbind(activityTest, testData)  ##combining activity to corresponding data
combinedTrain <- cbind(activityTrain, trainData) ##combining activity to corresponding data


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
dt2 <- dcast(meltedData, Activity ~ variable, mean) ## takes mean of every variable by activity value

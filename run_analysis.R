
setwd("D:/Courses/Data Science Specialization/03. Getting and Cleaning Data/Week3/CP")

# Download Data
if(!file.exists("./data")){
    dir.create("./data")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url=fileUrl,destfile="./data/getdata-projectfiles.zip")
unzip('./data/getdata-projectfiles.zip')
# list.files("UCI HAR Dataset", recursive=T)

# Reading Data
library(data.table)

# Goal: create 3 variables (features, subject, activity)

## Reading Features
trainFeatures <- read.table("UCI HAR Dataset/train/X_train.txt")
testFeatures <- read.table("UCI HAR Dataset/test/X_test.txt")

## Reading Subject
trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")

## Reading Activity
trainActivity <- read.table("UCI HAR Dataset/train/y_train.txt")
testActivity <- read.table("UCI HAR Dataset/test/y_test.txt")

# Explore Features
dim(trainFeatures)
str(trainFeatures)

dim(testFeatures)
str(testFeatures)

# Merges the training and the test sets to create one data set
dataFeatures <- rbind(trainFeatures, testFeatures)
dim(dataFeatures)
# str(dataFeatures)
dataSubject <- rbind(trainSubject, testSubject)
dim(dataSubject)
# str(dataSubject)
dataActivity <- rbind(trainActivity, testActivity)
dim(dataActivity)
# str(dataActivity)

# MERGE ALL by column
dim(dataFeatures)
dim(dataSubject)
dim(dataActivity)

df <- cbind(dataFeatures, dataSubject, dataActivity)
dim(df)


# Set names of columns to Features 
namesFeatures <- read.table("UCI HAR Dataset/features.txt")
names(dataFeatures) <- namesFeatures$V2

# Extracts only the measurements on the mean and standard deviation for each measurement
str(df)
meanSd  <- grepl("(-std\\(\\)|-mean\\(\\))",namesFeatures$V2)
meanSdData <- dataFeatures[, which(meanSd == TRUE)]
dim(meanSdData)

# Uses descriptive activity names to name the activities in the data set
namesActivity <- read.table("UCI HAR Dataset/activity_labels.txt")
activity <- as.factor(dataActivity$V1)
subject <- as.factor(dataSubject$V1)
activityName <- activity
levels(activityName) <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING",
                          "STANDING", "LAYING")
actName <- cbind(activity, activityName, subject, meanSdData)

# Appropriately labels the data set with descriptive variable names
str(actName)

names(actName) <- gsub("^t", "time", names(actName))
names(actName) <- gsub("^f", "frequency", names(actName))
names(actName) <- gsub("Acc", "Accelerometer", names(actName))
names(actName) <- gsub("Gyro", "Gyroscope", names(actName))
names(actName) <- gsub("Mag", "Magnitude", names(actName))
names(actName) <- gsub("BodyBody", "Body", names(actName))
names(actName) <- gsub("std()", "sd", names(actName))
names(actName) <- gsub("mean()", "mean", names(actName))
names(actName)

# Create a second, independent tidy data set with the average of each variable for each activity and each subject
library(plyr)
newData <- aggregate(. ~ subject + activity + activityName, actName, mean)
newData <- newData[order(newData$subject, newData$activity), ]
write.table(newData, file = "tidydata.txt", row.name=FALSE)



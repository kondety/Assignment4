setwd('C:/Users/sowja/Desktop/Data Science/UCI HAR Dataset/UCI HAR Dataset')

#read data into data frames
subjecttest <- read.table("./test/subject_test.txt", sep = ",", header = TRUE)
xtest <- read.table("./test/x_test.txt", sep = ",", header = TRUE)
ytest <- read.table("./test/y_test.txt", sep = ",", header = TRUE)

#add column name for test data subject files and measurement files
colnames(subjecttest) <- "id"
colnames(xtest) <- "x1"
colnames(ytest) <- "y1"

#combine files into one data set
testdata <- cbind(subjecttest, xtest, ytest)


#readd train data into data frames
subjecttrain <- read.table("./train/subject_train.txt", sep = ",", header = TRUE)
xtrain <- read.table("./train/x_train.txt", sep = ",", header = TRUE)
ytrain <- read.table("./train/y_train.txt", sep = "'", header = TRUE)

#add column name flor train data subject files and measurement files
colnames(subjecttrain) <- "id"
colnames(xtrain) <- "x1"
colnames(ytrain) <- "y1"

# add column name for lable files
activity_labels <- read.table("./activity_labels", sep = ",", header = TRUE)
features <- read.table("./tidydata", sep = ",", header = TRUE)
colnames(activity_labels) <- c("v1", "v2")

#combine files into one data set
traindata <- cbind(subjecttrain, xtrain, ytrain)
totaldata <- merge(testdata, traindata, all = TRUE)

cn <- colnames(totaldata)

##step 2: Extracts only the measurements on the mean and standard deviation for each measurement
#determine which columns contain "mean() or "std()"
mean_deviation <- grepl("mean|std|subject|y1", colnames(totaldata), ignore.case = TRUE)
requiredColumns <- c(mean_deviation, 563,564)
dim(requiredColumns)
extractedData <- totaldata[,requiredColumns]
dim(extractedData)

##step3: Uses descriptive activity names to name the activities in the data set

extractedData$activityName <- as.character(extractedData$activityName)
for(i in 1:6){
  extractedData$activityName[extractedData$activityName ==i] <- as.character(activity_labels[i,2])
  
}

extractedData$activityName <- as.factor(extractedData$activityName)

##step 4:Appropriately labels the data set with descriptive variable names.
head(str(extractedData),2)
names(extractedData)<-gsub("std()", "SD", names(extractedData))
names(extractedData)<-gsub("mean()", "MEAN", names(extractedData))
names(extractedData)<-gsub("^t", "time", names(extractedData))
names(extractedData)<-gsub("^f", "frequency", names(extractedData))
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
# Names after
head(str(extractedData),6)

## step 5: creates a second, independent tidy data set with the average of each variable for each activity and each subject.
extractedData$Subject <- as.factor(extractedData$subject)
extractedData <- data.table(extractedData)
tidyData <- aggregate(. ~Subject + activityname, extractedData, mean)
tidyData <- tidyData[orderr(tidyData$Subject, tidyData$activityName),]
write.table(tidyData, file = "tidydataset.txt", row.names = FALSE)
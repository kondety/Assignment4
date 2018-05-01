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
colnames(ytrain) <- "activity"
colnames(xtest) <- "activity"

#combine files into one data set
traindata <- cbind(subjecttrain, xtrain, ytrain)
totaldata <- merge(testdata, traindata, all = TRUE)

cn <- colnames(totaldata)

##step 2: Extracts only the measurements on the mean and standard deviation for each measurement
#determine which columns contain "mean() or "std()"
mean_deviation <- totaldata[,grepl("mean|std|subject|y1", colnames(totaldata))]
activity <- read.table("activity_labels.txt", header = FALSE, sep = " ")
colnames(activity) <- c("activityid", "activity1")
data <- merge(x = mean_deviation, y = activity, by = "activityid")
unique(data[,c("activity1")])

##step3: Uses descriptive activity names to name the activities in the data set

# enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

# create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))


##step 4:Appropriately labels the data set with descriptive variable names.
head(str(dataTable),2)
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
# Names after
head(str(dataTable),6)

## step 5: creates a second, independent tidy data set with the average of each variable for each activity and each subject.

write.table(dataTable, "TidyData.txt", row.names = FALSE)

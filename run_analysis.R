## Run_analysis.R
## Coursera - Getting and Cleaning Data
## Course Project - Renato Deggau - rdeggau - June 2015

# 1. Merges the training and the test sets to create one data set.

if (!file.exists("./dados")) {
  dir.create("./dados")
}
library(dplyr)
## Read the files in txt format
activity_labels  = read.table("./dados/UCI HAR Dataset/activity_labels.txt", header=FALSE)
features         = read.table("./dados/UCI HAR Dataset/features.txt", header=FALSE)
extract_features = grepl("mean|std", features)

training_X       = read.table("./dados/UCI HAR Dataset/train/X_train.txt", header=FALSE)
training_Y       = read.table("./dados/UCI HAR Dataset/train/Y_train.txt", header=FALSE)
training_subject = read.table("./dados/UCI HAR Dataset/train/subject_train.txt", header=FALSE)

test_X           = read.table("./dados/UCI HAR Dataset/test/X_test.txt", header=FALSE)
test_Y           = read.table("./dados/UCI HAR Dataset/test/Y_test.txt", header=FALSE)
test_subject     = read.table("./dados/UCI HAR Dataset/test/subject_test.txt", header=FALSE)

## Naming the columns
colnames(activity_labels)  = c("CdActivity", "Activity")
colnames(training_X)       = features[ ,2]
colnames(training_subject) = "Subject"
colnames(training_Y)       = "Activity"
colnames(test_X)           = features[ ,2]
colnames(test_subject)     = "Subject"
colnames(test_Y)           = "Activity"

## Merging 
training = cbind(training_subject, training_Y, training_X)
test     = cbind(test_subject, test_Y, test_X)
MyData   = rbind(training, test)

# 2. Extract only the measurements on the mean and standard deviation for each measurement
MyData = MyData[,grepl("mean|std|Subject|Activity", names(MyData))]

# 3. Use descriptive activity names to name the activities in the data set
MyData$Activity <- as.character(MyData$Activity)
for (i in 1:6){
  MyData$Activity[MyData$Activity == i] <- as.character(activity_labels[i,2])
}
MyData$Activity <- as.factor(MyData$Activity)

# 4. Appropriately label the data set with descriptive variable names.
names(MyData) <- gsub('\\(|\\)',"",names(MyData), perl = TRUE)
names(MyData) <- make.names(names(MyData))
names(MyData) <- gsub('Acc',"Acceleration",names(MyData))
names(MyData) <- gsub('GyroJerk',"AngularAcceleration",names(MyData))
names(MyData) <- gsub('Gyro',"AngularSpeed",names(MyData))
names(MyData) <- gsub('Mag',"Magnitude",names(MyData))
names(MyData) <- gsub('\\.mean',".Mean",names(MyData))
names(MyData) <- gsub('\\.std',".StandardDeviation",names(MyData))
names(MyData) <- gsub('Freq\\.',"Frequency.",names(MyData))
names(MyData) <- gsub('Freq$',"Frequency",names(MyData))

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

MyDataFinal = MyData %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))
write.table(MyDataFinal, file="./MyData.txt", row.name=FALSE)

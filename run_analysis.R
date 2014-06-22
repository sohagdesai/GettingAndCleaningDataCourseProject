library(data.table)

## Implements Course Project for Getting and Cleaning Data.
## ----------------------------------------------------------------
## Step 1: Merge training and the test sets to create one data set.
## ----------------------------------------------------------------
## First, read in all the relevant files.
##
## Read in the test data.
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Make the column name in y_test more readable.
names(y_test) <- c("activity_code")



## Read in the training data.
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

## Make the column name in y_train more readable.
names(y_train) <- c("activity_code")



## Read in the activity labels. 
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

## Read in the feature list.
features <- read.table("./UCI HAR Dataset/features.txt")



## Transform activity_labels to have column names as activities 
## and cells as activity codes. 
activity <- as.vector(activity_labels$V1)
names(activity) <- c(as.vector(activity_labels$V2))

## Replace the activity codes in y_test with the activity name.
y_test$activity <- names(activity)[match(y_test$activity_code, activity)]

## Replace the activity codes in y_train with the activity name.
y_train$activity <- names(activity)[match(y_train$activity_code, activity)]

## Now we are ready to construct a dataframe in a tidy form with variables as column names (with
## friendly names) and observations as rows. 

# Name the columns with friendly names derived from the features. 
names(X_test) <- c("subject","activity",as.vector(features$V2))
names(X_train) <- c("subject","activity",as.vector(features$V2))

## Bind the columns of the subjects, activities and the activity data for the respective datasets.
test_data <- cbind(subject_test,y_test$activity,X_test)
train_data <- cbind(subject_train,y_train$activity,X_train)

## Subset the test data to incude only the variables that are mean or std. deviations.
X_test <- X_test[,grep("mean|std",colnames(X_test))]

## Subset the train data to include only the variables that are mean or std. deviations.
X_train <- X_train[,grep("mean|std",colnames(X_train))]

## Subset the features to include only the variables that are mean or std. deviations.
features <- features[,grep("mean|std",colnames(features))]

## Merge the training and test data sets to produce a single data set. 
merged_data <- rbind(train_data,test_data)

## Find the mean of each variable grouped by subject and activity.
mean_data <- with(merged_data, 
                  aggregate (cbind(merged_data[,3:79]),
                             list (merged_data$subject, 
                                   merged_data$activity),
                             mean))

## Write the merged data to a file. 
write.table(mean_data,"mean_data.txt",row.names=FALSE)
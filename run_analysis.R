#########################################################################################
# Step 0 
#########################################################################################
#set working directory
#setwd("C:/Users/rparks/Desktop/Perosnal/Coursera/hw/GetData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
#load dplyr library
library(dplyr)
#########################################################################################
# Step 1
#########################################################################################
# Read in needed data sets
x_train <- read.table("train/X_train.txt")
x_test <- read.table("test/X_test.txt")

y_train <- read.table("train/y_train.txt")
y_test <- read.table("test/y_test.txt")

subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")

# create 'x' data set
X <- rbind(x_train, x_test)
# create 'y' data set
Y <- rbind(y_train, y_test)
# create 'subject' data set
S <- rbind(subject_train, subject_test)
#########################################################################################
# Step 2
#########################################################################################
# Extract only the measurements on the mean and standard deviation for each measurement
features <- read.table("features.txt")
# get only columns with mean() or std() in their names
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
# subset the desired columns
X <- X[, indices_of_good_features]
# correct the column names
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))
#########################################################################################
# Step 3
#########################################################################################
# Use descriptive activity names to name the activities in the data set
activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
# update values with correct activity names
Y[,1] = activities[Y[,1], 2]
# correct column name
names(Y) <- "activity"
#########################################################################################
# Step 4
#########################################################################################
# Appropriately label the data set with descriptive variable names
#########################################################################################
names(S) <- "subject"
# bind all the data in a single data set
cleaned <- cbind(S, Y, X)

write.table(cleaned, "merged_clean_data.txt")

#########################################################################################
# Step 5
#########################################################################################
# Create a second, independent tidy data set with the average of each variable for each activity and each subject
# EXCEPT THE last two just added by the cbind above (activity & subject)
uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]
row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt",row.name=FALSE)
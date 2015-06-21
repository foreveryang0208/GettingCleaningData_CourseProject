setwd("./R_clean_data/project/UCI HAR Dataset/")

#---------------------------------------------------------------------------------------------------------
# PREPARE 1:
# Read in datasets that is in the training set and combine them into training.datset dataframe
----------------------------------------------------------------------------------------------------------
# Read subject_train.txt into a list named "subject" representing the index number (1-30) of experiement subjects 
subject <- read.table("./train/subject_train.txt")[,1]

# Read y_train.txt into a list named "activity" reprenting the activity label
activity_label <- read.table("./train/y_train.txt")[,1]

# Read features.txt into a list named "feature_names"
feature_names <- read.table("features.txt")[,2]

# Read X_train.txt into a data.table named "training.features" using feature_names as column names
training.features <- read.table("./train/X_train.txt", col.names = feature_names)

# Combine subject, training.features, activity into training.dataset
training.dataset <- cbind(subject, activity_label, training.features)


#---------------------------------------------------------------------------------------------------------
# PREPARE 2:
# Read in datasets that is in the test set and combine them into test.dataset dataframe
----------------------------------------------------------------------------------------------------------
  # Read subject_train.txt into a list named "subject" representing the index number (1-30) of experiement subjects 
subject <- read.table("./test/subject_test.txt")[,1]

# Read y_train.txt into a list named "activity" reprenting the activity label
activity_label <- read.table("./test/y_test.txt")[,1]

# Read X_train.txt into a data.table named "training.features" using feature_names as column names
test.features <- read.table("./test/X_test.txt", col.names = feature_names)

# Combine subject, training.features, activity into training.dataset
test.dataset <- cbind(subject, activity_label, test.features)

#---------------------------------------------------------------------------------------------------------
# Requirement 1:
# Merges the training and the test sets to create one data set, called entire.dataset
----------------------------------------------------------------------------------------------------------
entire.dataset <- rbind(training.dataset, test.dataset)

#---------------------------------------------------------------------------------------------------------
# Requirement 2:
# Extracts only the measurements on the mean and standard deviation for each measurement
----------------------------------------------------------------------------------------------------------
# subject and activity columns are ahead of all the feature columns, so add 2 to the returned column indexes from grep function
mean_std_columns.index <- sort(c( grep("mean", feature_names), grep("std",feature_names)))
mean_std_columns.names <- feature_names[mean_std_columns]
entire.meanstd <- entire.dataset[,c(1:2,2+mean_std_columns.index)]

#---------------------------------------------------------------------------------------------------------
# Requirement 3:
# Uses descriptive activity names to name the activities in the data set
----------------------------------------------------------------------------------------------------------  
activity <- read.table("activity_labels.txt", col.names = c("activity_label", "activity"))
entire.meanstd.activity <- merge(entire.meanstd, activity, by = "activity_label")

#---------------------------------------------------------------------------------------------------------
# Requirement 4:
# Appropriately labels the data set with descriptive variable names (this is met when reading in the data frames)
----------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------
# Requirement 5:
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject
----------------------------------------------------------------------------------------------------------  
library(reshape2)

entireMelt <- melt(entire.meanstd.activity[,!names(entire.meanstd.activity) == "activity_label"], id = c("subject", "activity"))
variable.means.bysub_act <- dcast(entireMelt, subject + activity ~ variable,mean)

write.table(variable.means.bysub_act, file = "tidydata.txt", row.name=FALSE)

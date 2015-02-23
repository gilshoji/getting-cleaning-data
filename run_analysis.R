#load library
library(plyr)

# Step 1 - Merges the training and the test sets to create one data set.

# reading train datasets
x_train <- read.table("C:/GILSON/coursera/data cleaning/bases/Project/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("C:/GILSON/coursera/data cleaning/bases/Project/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("C:/GILSON/coursera/data cleaning/bases/Project/UCI HAR Dataset/train/subject_train.txt")
 
# create a train dataset
train <- cbind(x_train,y_train,subject_train)

# reading test data sets
x_test <- read.table("C:/GILSON/coursera/data cleaning/bases/Project/UCI HAR Dataset/train/X_test.txt")
y_test <- read.table("C:/GILSON/coursera/data cleaning/bases/Project/UCI HAR Dataset/train/y_test.txt")
subject_test <- read.table("C:/GILSON/coursera/data cleaning/bases/Project/UCI HAR Dataset/train/subject_test.txt")

# create a train dataset
test <- cbind(x_test,y_test,subject_test)

# create a complete data
finaldata = rbind(train,test);

# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 

# read feature dataset
features <- read.table("C:/GILSON/coursera/data cleaning/bases/Project/UCI HAR Dataset/train/features.txt")

# find columns with mean or std in their names
find_cols <- grep("-(mean|std)\\(\\)", features[, 2])

# create a subset with this columns
finaldata <- finaldata[, find_cols]

# Make the label columns
names(finaldata) <- features[find_cols, 2]

# Step 3 - Use descriptive activity names to name the activities in the data set

# read the activity table
activity <- read.table("C:/GILSON/coursera/data cleaning/bases/Project/UCI HAR Dataset/train/activity_labels.txt")

# Merge the finalData with the acitivity table
finalData = merge(finalData,activity,by='activityId',all.x=TRUE);

# correct column name
colnames  = colnames(finalData); 

# Step 4 - Appropriately label the data set with descriptive variable names

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StandardDeviation",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","Time",colNames[i])
  colNames[i] = gsub("^(f)","Frequency",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Step 5 - From the data set in step 4, create a second, independent tidy data set with the average of each variable
# for each activity and each subject

# Create finalData2 without the activityType column
final2  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalData2 table to include the mean of each variable for each activity and each subject
Final_Data    = aggregate(final2[,names(final2) != c('activityId','subjectId')],by=list(activityId=finalDataN2$activityId,subjectId = finalData2$subjectId),mean);

# Merging the Final_Data with activity
Final_Data    = merge(Final_Data,activity,by='activityId',all.x=TRUE);

# Export the Final_Data 
write.table(Final_Data, "C:/GILSON/coursera/data cleaning/bases/Project/UCI HAR Dataset/train/Final_Data.txt'"row.name=FALSE,sep='\t');

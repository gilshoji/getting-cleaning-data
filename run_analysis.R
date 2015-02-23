#load library
library(plyr)

# Step 1 - Merges the training and the test sets to create one data set.

# read feature dataset
features <- read.table("C:/Gilson/coursera/Getting_Cleaning_Data/Project/UCI HAR Dataset/features.txt")

# reading train datasets
x_train <- read.table("C:/Gilson/coursera/Getting_Cleaning_Data/Project/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("C:/Gilson/coursera/Getting_Cleaning_Data/Project/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("C:/Gilson/coursera/Getting_Cleaning_Data/Project/UCI HAR Dataset/train/subject_train.txt")

# Assign column names
colnames(subject_train)  = "subjectId";
colnames(x_train)        = features[,2]; 
colnames(y_train)        = "activityId";

# create a train dataset
train <- cbind(y_train,subject_train,x_train)

# reading test data sets
x_test <- read.table("C:/Gilson/coursera/Getting_Cleaning_Data/Project/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("C:/Gilson/coursera/Getting_Cleaning_Data/Project/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("C:/Gilson/coursera/Getting_Cleaning_Data/Project/UCI HAR Dataset/test/subject_test.txt")

# Assign column names
colnames(subject_test)  = "subjectId";
colnames(x_test)        = features[,2]; 
colnames(y_test)        = "activityId";

# create a train dataset
test <- cbind(y_test,subject_test,x_test)

# create a complete data
finaldata = rbind(train,test);

# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 

# find columns with mean or std in their names
find_cols <- grep("-(mean|std)\\(\\)", features[, 2])

# create a subset with this columns
Finaldata <- finaldata[, find_cols]

# Make the label columns
names(Finaldata) <- features[find_cols, 2]

# Step 3 - Use descriptive activity names to name the activities in the data set

# read the activity table
activity <- read.table("C:/Gilson/coursera/Getting_Cleaning_Data/Project/UCI HAR Dataset/activity_labels.txt",header=FALSE)
colnames(activity)  = c('activityId','activityType');

# Step 4 - Appropriately label the data set with descriptive variable names

  names(Finaldata) = gsub("\\()","",names(Finaldata))
  names(Finaldata) = gsub("-std$","StandardDeviation",names(Finaldata))
  names(Finaldata) = gsub("-mean","Mean",names(Finaldata))
  names(Finaldata) = gsub("^(t)","Time",names(Finaldata))
  names(Finaldata) = gsub("^(f)","Frequency",names(Finaldata))
  names(Finaldata) = gsub("AccMag","AccMagnitude",names(Finaldata))
  names(Finaldata) = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",names(Finaldata))
  names(Finaldata) = gsub("JerkMag","JerkMagnitude",names(Finaldata))
  names(Finaldata) = gsub("GyroMag","GyroMagnitude",names(Finaldata))


# Step 5 - From the data set in step 4, create a second, independent tidy data set with the average of each variable
# for each activity and each subject

Finaldata = merge(Finaldata,activity,by='activityId',all.x=TRUE);

# Create a new table, Finaldata without the activityId column
Finaldata  = Finaldata[,names(Finaldata) != 'activityId'];

tidy = ddply(Finaldata, c("subjectId","activityType"), numcolwise(mean))

# Export the Final_Data 
write.table(Finaldata, 'C:/Gilson/coursera/Getting_Cleaning_Data/Project/UCI HAR Dataset/Final_Data.txt',row.name=FALSE,sep='\t');

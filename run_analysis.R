

# 1. Merges the training and the test sets to create one data set.

# set working directory
setwd("~/Coursera/Assignments/3. Getting and Cleaning data/UCI HAR Dataset")



# Reading data from train folder

#import features data
features = read.table('features.txt', header = FALSE);

#import activity_labels data
activityType = read.table('activity_labels.txt', header = FALSE);

#import subject_train data
subjectTrain = read.table('train/subject_train.txt', header = FALSE);

#import x_train data
xTrain = read.table('train/x_train.txt', header = FALSE);

#import y_train data
yTrain = read.table('train/y_train.txt', header = FALSE);

# Assign column names to the imported train data sets
colnames(activityType) = c('activityId', 'activityType');
colnames(subjectTrain) = "subjectId";
colnames(xTrain) = features[,2];
colnames(yTrain) = "activityId";

# Merge train data sets: subjectTrain, xTrain & yTrain
trainingData = cbind(yTrain, subjectTrain, xTrain);



# Readind data from test folder

#import subject_test data
subjectTest = read.table('test/subject_test.txt', header = FALSE);

#import x_test data
xTest = read.table('test/x_test.txt', header = FALSE);

#import y_test data
yTest = read.table('test/y_test.txt', header = FALSE);

# Assign column names to the imported test data sets
colnames(subjectTest) = "subjectId";
colnames(xTest) = features[,2];
colnames(yTest) = "activityId";

# Merge test data sets: yTest, subjectTest & xTest
testData = cbind(yTest, subjectTest, xTest);

# Merge train & test data sets
finalData = rbind(trainingData, testData);



# create a vector for the column names in finalData for which we will select mean & stddev
colNames = colnames(finalData);





# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# create a Vector that contains TRUE values for the ID, mean & stdev columns and FALSE for others
logicalVector = (grepl("activity..", colNames) | 
                grepl("subject..", colNames) | 
                grepl("mean..", colNames) |
                grepl("std..", colNames)
                );

# subset finalData table based on the logical vector to retain only desired columns
finalData = finalData[logicalVector == TRUE];




# 3. Uses descriptive activity names to name the activities in the data set

# merge finalData set with the activity type table to include descriptive activity names
finalData = merge(finalData, activityType, by = "activityId", all.x = TRUE);

# update the colNames vector to include new column names after merging
colNames = colnames(finalData);





# 4. Appropriately label the data set with descriptive activity names.

# clean up the variable names
for(i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;





# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType = finalData[, names(finalData) != 'activityType'];

# summarize the finalDataNoActivityType table to include the mean of each variable for each activity and subject
tidyData = aggregate(finalDataNoActivityType[, names(finalDataNoActivityType) != c('activityId', 'subjectId')], by = list(activityId = finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# merge tidyData with activityType to include descriptive activity names
tidyData = merge(tidyData,activityType,by='activityId', all.x = TRUE);

# export tidyData set
write.table(tidyData, 'tidy.txt', row.names = TRUE, sep = '\t');

##########################################################################################################
# The data for the project: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# Create one R script called run_analysis.R that does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
##########################################################################################################

# 1. Merge the training and the test sets to create one data set.

	# Set working directory
	setwd('C:/Documents and Settings/User/Mis documentos/Dropbox/Coursera/Getting and Cleaning Data/Assignment/UCI HAR Dataset');

	# Read data from files
	features = read.table('./features.txt',header=FALSE);
	activity = read.table('./activity_labels.txt',header=FALSE);
	xTrain = read.table('./train/x_train.txt',header=FALSE);
	yTrain = read.table('./train/y_train.txt',header=FALSE);
	subjectTrain = read.table('./train/subject_train.txt',header=FALSE);

	# Assign column names to the imported data
	colnames(activity) = c('activityId','activityType');
	colnames(xTrain) = features[,2]; 
	colnames(yTrain) = "activityId";
	colnames(subjectTrain) = "subjectId";

	# Merge trainingData
	trainingData = cbind(xTrain, yTrain, subjectTrain);

	# Read in the test data
	xTest = read.table('./test/x_test.txt',header=FALSE);
	yTest = read.table('./test/y_test.txt',header=FALSE);
	subjectTest = read.table('./test/subject_test.txt',header=FALSE);

	# Assign column names to the test imported data
	colnames(xTest) = features[,2]; 
	colnames(yTest) = "activityId";
	colnames(subjectTest) = "subjectId";

	# Merge testData
	testData = cbind(xTest, yTest,subjectTest);

	# Merge training and test data to create a final data set
	finalData = rbind(trainingData,testData);

	# Column names from the finalData
	colNames = colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

	# Contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
	id_mean_sd = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

	# Keep only desired columns
	finalData = finalData[id_mean_sd==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

	# Merge the final data set with the activity table to include descriptive activity names
	finalData = merge(finalData,activity,by='activityId',all.x=TRUE);

	# Updating the colNames vector
	colNames = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

	# Cleaning up the variable names
	for (i in 1:length(colNames)) 
	{
	 colNames[i] = gsub("\\()","",colNames[i])
	 colNames[i] = gsub("-std$","StdDev",colNames[i])
	 colNames[i] = gsub("-mean","Mean",colNames[i])
	 colNames[i] = gsub("^(t)","time",colNames[i])
	 colNames[i] = gsub("^(f)","freq",colNames[i])
	 colNames[i] = gsub("Mag","Magnitude",colNames[i]) 
	};

	# Reassigning the new descriptive column names to the finalData set
	colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

	# Create a new table without the activityType column
	DataSet2 = finalData[,names(finalData) != 'activityType'];

	# Include just the mean of each variable for each activity and each subject
	tidyDataSet2 = aggregate(DataSet2[,names(DataSet2) != c('activityId','subjectId')],by=list(activityId=DataSet2$activityId,subjectId = DataSet2$subjectId),mean);

	# Merging the tidyDataSet2 with activity to add descriptive activity names
	tidyDataSet2 = merge(tidyDataSet2,activity,by='activityId',all.x=TRUE);

	# Export the tidyDataSet2 set 
	write.table(tidyDataSet2, './tidyDataSet2.txt',row.names=FALSE,sep='\t');

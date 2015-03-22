# Reading all the files commom to both train and test
activity <- read.table("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE);

features <- read.table("UCI HAR Dataset/features.txt", sep="", header=FALSE);

# Creating a vector of the features.txt file to use for colnames

features_vector <- as.vector(features$V2);


## Reading all test files in r

X_test <- read.table("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE);

colnames(X_test) <- features_vector;

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE);

colnames(subject_test) <- "subjectID"

y_test <- read.table("UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE);

## Reading all train files in r, and check for NA's using 'any(is.na(var_name))'

X_train <- read.table("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE);

colnames(X_train) <- features_vector

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE);

colnames(subject_train) <- "subjectID"

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", sep="", header=FALSE);


## Build a single data set for train, by column binding the files

subject.X_train <- cbind(subject_train, X_train);

# Merge the activity_labels file with the y_train file
activity.y_train <- merge(activity, y_train, by="V1");

colnames(activity.y_train) <- c("activityNumber", "activityName");
train <- cbind(subject.X_train, activity.y_train);




## Build a single data set for test, by column binding the three files
subject.X_test <- cbind(subject_test, X_test);
activity.y_test <- merge(activity, y_test, by="V1");
colnames(activity.y_test) <- c("activityNumber", "activityName");
test <- cbind(subject.X_test, activity.y_test);

# Combine test and train into a single data set
final <- rbind(train,test);

# Subset the final dataset using only the mean and standard deviation for each 
# measurement.
cols <- c(1, 2, 3, 4, 42, 43, 44, 82, 83, 84, 122, 123, 124, 162, 163, 164, 202,
		  215, 228, 241, 254, 267, 268, 269, 295, 296, 297, 346, 347, 348, 374, 375, 376,
		  425, 426, 427, 453, 454, 455, 504, 514, 517, 527, 530, 540, 543, 553, 556, 557,
		  558, 559, 560, 561, 562, 5, 6, 7, 45, 46, 47, 85, 86,  87, 125, 126, 127,165, 
		  166, 167, 203, 216, 229, 242, 255, 270, 271, 272, 349, 350, 351, 428, 429, 430,
		  505, 518, 531, 544, 563, 564)

data <- final[,cols];
# Subsetted and cleaned data till step 4.
write.table(data, "subData", row.name= FALSE);

# Taking per subject

subjects <- as.numeric(levels(as.factor(data[, 1])))

tidydata <- data[FALSE, ];
rowNum <- 1;

for (subject in subjects) {
	
	data <- data[data$subjectID == subject, ]
	
	# divide by activity
	for (i in 1:6) {
		activityNumber <- data[data$activityNumber == i, "activityNumber"][1]
		activityName <- data[data$activityName == i, "activityName"][1]
		
		for (column in names(data)) {
			if (column == "activityName" |
					column == "activityNumber" |
					column == "subjectID") { next }
			
			tidydata[rowNum, column] <- mean(data[, column])
		}
		
		tidydata[rowNum, "subjectID"] <- subject
		tidydata[rowNum, "activityName"] <- activityName
		tidydata[rowNum, "activityNumber"] <- activityNumber
		rowNum <- rowNum + 1
	}
	
}
# Final file for the tidy data in step 5.
write.table(data, "final-data.txt", row.name = FALSE);
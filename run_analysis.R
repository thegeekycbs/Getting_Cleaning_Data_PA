# 1. Merges the training and the test sets to create one data set

xtemp1 <- read.table("train/X_train.txt")
xtemp2 <- read.table("test/X_test.txt")
x <- rbind(xtemp1, xtemp2)
#head(x)
#tail(x)

stemp1 <- read.table("train/subject_train.txt")
stemp2 <- read.table("test/subject_test.txt")
s <- rbind(stemp1, stemp2)
#head(s)
#tail(s)

ytemp1 <- read.table("train/y_train.txt")
ytemp2 <- read.table("test/y_test.txt")
y <- rbind(ytemp1,ytemp2)
#head(y)
#tail(y)

#2. Extracts only the measurements on the mean and standard deviation for each measurement

features_data <- read.table("features.txt")
#looking for any features related to mean and standard deviation, std and return only the particular column number for selected features
selected_features <- grep("-mean\\(\\)|-std\\(\\)",features_data[,2])
#return the relevant columns
x <- x[, selected_features]
#Replace the names of the selected columns with the names from selected features
names(x) <- features_data[selected_features,2]
#Substitute all the names in X with () with blank, take away all the () from the names
names(x) <- gsub("\\(|\\)","",names(x))
#Transform all the names in x to lower case
names(x) <- tolower(names(x))

#3. Uses descriptive activity names to name the activities in the data set
activities <- read.table("activity_labels.txt")
activities[,2] <- gsub("_","",tolower(as.character(activities[,2])))
y[,1] <- activities[y[,1],2]
names(y) <- "activity"

#4. Appropirately labels the data set with descriptive activity names
names(s) <- "subject"
cleaned_data <- cbind(s,y,x)
write.table(cleaned_data,"Cleaned_Data_Set.txt")

#5. Creates a 2nd, independent tidy data set with the average of each activity and each subject
uniqueSubjects <- unique(s)[,1]
numSubjects <- length(unique(s)[,1])
numActivities <- length(activities[,1])
numCols <- dim(cleaned_data)[2]
result <- cleaned_data[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned_data[cleaned_data$subject==s & cleaned_data$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "Data_with_Averages.txt")

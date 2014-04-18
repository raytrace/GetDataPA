setwd('/Volumes/TheOneThing/Courses/Track.DataSci/GetData/GetDataPA')

features <- read.table('data/features.txt',header=F)

# make the training dataset
train.X <- read.table('data/train/X_train.txt',header=F)
colnames(train.X) <- features[,2]
subjects <- read.table('data/train/subject_train.txt',header=F)
colnames(subjects) <- "subject"
train.data <- cbind(subjects,train.X)

# make the test dataset
test.X <- read.table('data/test/X_test.txt',header=F)
colnames(test.X) <- features[,2]
subjects <- read.table('data/test/subject_test.txt',header=F)
colnames(subjects) <- "subject"
test.data <- cbind(subjects,test.X)

# 1. Merge the training and the test sets to create one data set.
data <- rbind(train.data,test.data)

# 2. Extract only the measurements on the mean and standard
#    deviation for each measurement.
cols <- c(grep("mean()",colnames(data)),grep("std()",colnames(data)))
meanstd.data <- data[,cols]

# 3. Use descriptive activity names to name the activities in the
#   data set
activities <- read.table('data/activity_labels.txt')
# 4. Appropriately labels the data set with descriptive activity
#    names.
train.y <- read.table('data/train/y_train.txt',header=F)
test.y <- read.table('data/test/y_test.txt',header=F)
data$activity <- factor(as.matrix(rbind(train.y,test.y)),levels=1:6,labels=activities[,2])

# 5. Create a second, independent tidy data set with the average
#    of each variable for each activity and each subject.

# This is ambiguous. Since it states "each activity and each subject",
# I chose to interpret it such that I compute the average for
# each subject-activity pairing. An alternate interpretation is
# the average for each activity separately, then each subject
# separately.

# Also, since it states to find the average for each variable,
# I used the original dataset rather than the dataset containing
# extracted means and standard deviations. YMMV.
splitData <- split(data, list(data$subject,data$activity),drop=T)
summaryData <- lapply(splitData, function(x) c(subject=x$subject[1],activity=x$activity[1],colMeans(x[,-which(colnames(x) %in% c("activity","subject"))],na.rm=T)))
summaryData <- data.frame(do.call(rbind,summaryData),row.names=NULL)
# the above mangles the column names so restore them
colnames(summaryData)[3:563] <- features[,2]
summaryData$activity <- factor(as.matrix(summaryData$activity),levels=1:6,labels=activities[,2])
write.csv(summaryData,file='summaryData.txt',row.names=F)

setwd("UCI HAR Dataset/")

#Read info files
feat.info <- read.table("features.txt", colClasses = "character")
activity  <- read.table("activity_labels.txt")

library(data.table)
library(plyr)

#Read training datasets and merge them to one dataset 
#X_tranin has been modified with a third-party software to make it possible to read with 'fread'
setwd("train/")
train.sub <- read.table("subject_train.txt")
train.x   <- fread("mX_train.txt")
train.y   <- read.table("y_train.txt")

trainSet  <- data.frame(train.x, train.sub, train.y)

#Read test datasets and merge them to one dataset
#X_test has been modified with a third-party software to make it possible to read with 'fread'
setwd("../test/")
test.sub <- read.table("subject_test.txt")
test.x   <- fread("mX_test.txt")
test.y   <- read.table("y_test.txt")

testSet  <- data.frame(test.x, test.sub, test.y)

#1. Merges the training and the test sets to create one data set.
Dataset <- rbind(trainSet, testSet)
str(Dataset)
print(object.size(Dataset), units = "Mb")

#2. Extracts only the measurements on the mean and standard deviation for each measurement
hdr.mean.num <- grep("-mean()", feat.info[, 2], fixed = T)
hdr.std.num  <- grep("-std()", feat.info[, 2], fixed = T)
hdr.select   <- sort(c(hdr.mean.num, hdr.std.num))
newData      <- Dataset[,hdr.select]
names(newData) <- feat.info[hdr.select,2]
str(newData)

#3. Uses descriptive activity names to name the activities in the data set
names(activity)        <- c("ActCode", "Activity")
colnames(Dataset)[563] <- "ActCode"
Dataset <- join(Dataset, activity)
str(Dataset[, 563:564])

#4. Appropriately labels the data set with descriptive variable names
names(Dataset)[1:561] <- feat.info[, 2]
names(Dataset)[562]   <- "subject"
str(Dataset)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data <- matrix(numeric(0), ncol = 3)
for(i in 1:561){
  temp <- ddply(Dataset, .(subject, Activity), summarize, value = mean(Dataset[,i], na.rm = T))
  temp <- cbind(temp, variable = names(Dataset)[i])
  data <- rbind(data, temp)
}

setwd("../")
write.table(data, "output.txt", row.names = F, sep = "\t")

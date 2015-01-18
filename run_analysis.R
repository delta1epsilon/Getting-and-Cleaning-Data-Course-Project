library(dplyr)
#reading data sets
train <- read.table("./train/X_train.txt")
train_subject <- read.table("./train/subject_train.txt")
train_act <- read.table("./train/y_train.txt")
test <- read.table("./test/X_test.txt")
test_subject <- read.table("./test/subject_test.txt")
test_act <- read.table("./test/y_test.txt")
act_label <- read.table("activity_labels.txt")
cnames <- read.table("features.txt")

#Let's rename variables
colnames(train) <- make.names(cnames$V2, unique=TRUE)
colnames(test) <- make.names(cnames$V2, unique=TRUE)

#Lets add 'Subject' and 'Activity_id' columns to 'test' and 'train'
train$Subject <- train_subject$V1
train$Activity_id <- train_act$V1
test$Subject <- test_subject$V1
test$Activity_id <- test_act$V1


#Merge the training and the test sets to create one data set
dataset <- rbind(train, test)

#Extracting only the measurements on the mean and standard deviation for each measurement. 
data <- select(dataset, Subject, Activity_id, contains("mean", ignore.case = TRUE), 
               contains("std", ignore.case = TRUE))
                         
#Use descriptive activity names to name the activities in the data set
data$Activity_label <- act_label[data$Activity_id, 2]

#Let's put 'Activity_label' at the beginning of 'data' 
data <- data[ ,c(1,2,89,3:88)]

rm(train, train_subject, train_act, test, test_subject, test_act, act_label, cnames, dataset)

#Appropriately label the data set with descriptive variable names. 
colnames(data) <- gsub("BodyBody", "Body", colnames(data))
colnames(data) <- gsub("[.][.][.]", "_", colnames(data))
colnames(data) <- gsub("[.][.]", "", colnames(data))

#Create independent tidy data set with the average of each variable 
#for each activity and each subject.
data <- transform(data, Subject = factor(Subject), Activity_label = factor(Activity_label))
grouped <- group_by(data, Subject, Activity_label)
newdata <- summarise_each(grouped, funs(mean))

rm(grouped)
#saving 'newdata' into txt file
write.table(newdata, file = "newdata.txt", row.names = FALSE)
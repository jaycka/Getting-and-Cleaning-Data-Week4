library(dplyr)
## 1. Merges the training and the test sets to create one data set.
## 4. Appropriately labels the data set with descriptive variable names.
mergedata <- function (filepath = getwd()){
    ## a. Load and modify both the training and test data sets
    setwd(filepath)
    
    ## b. Add descriptive variable names 
    features <- read.table('./features.txt')
    x_test <- read.table('./test/x_test.txt',col.names = features$V2)
    x_train <- read.table('./train/x_train.txt',col.names = features$V2)
    
    ## c. merge the training and test data sets by rbind().
    merged <-rbind(x_test,x_train)
}

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
extractdata <-function (x){
    extracted <- x[grepl('std\\.\\.',colnames(x)) | grepl('mean\\.\\.',colnames(x))]
}

## 3. Uses descriptive activity names to name the activities in the data set.
activity <- function (x){
    ## a. read both test label files
    y_test <- read.table('./test/y_test.txt')
    y_train <- read.table('./train/y_train.txt')
    
    ## b. merge the two tables into one data frame
    activitylabels <- rbind(y_test,y_train)
    
    ## c. replace test label numbers with actual acitivity  names
    activitylabels$V1 <- gsub('1','WALKING',activitylabels$V1)
    activitylabels$V1 <- gsub('2','WALKING_UPSTAIRS',activitylabels$V1)
    activitylabels$V1 <- gsub('3','WALKING_DOWNSTAIRS',activitylabels$V1)
    activitylabels$V1 <- gsub('4','SITTING',activitylabels$V1)
    activitylabels$V1 <- gsub('5','STANDING',activitylabels$V1)
    activitylabels$V1 <- gsub('6','LAYING',activitylabels$V1)
    
    ## d. add activity names and return the value
    x$Activity <- activitylabels$V1
    return (x)
}

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata <- function(x){
    ## a. read in both test and train subject files
    subject_test <- read.table('./test/subject_test.txt')
    subject_train <- read.table('./train/subject_train.txt')
    
    ## b. combine subject files into one dataframe
    subject <- rbind(subject_test,subject_train)
    
    ## c. add subject attribute to x
    x$Subject <- subject$V1
    
    ## d. tidy data set with the average of each variable for each activity and each subject
    tidydataset <- x %>% group_by(Subject,Activity)%>%summarise_all(mean)
    
    ## e. write output to file TidyData.txt
    write.table(tidydataset,'TidyData.txt',row.name = FALSE)
}


tidydata(activity(extractdata((mergedata()))))

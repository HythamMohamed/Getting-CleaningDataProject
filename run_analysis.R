run_analysis <- function()
{
  # load the dplyr library
  
  library(dplyr)
  
  # this script should run from the top level directory of the ./UCI HAR Dataset
  # it will check to see if the directory exist and if not it will create a directory
  # download and unzip the dataset then proceed with the data cleaning.
  
  if (!file.exists("./UCI HAR Dataset"))
  {
    dir.create("./UCI_data")
    dataset_URL = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(dataset_URL,destfile = "./UCI_data/uciDataSet.zip")
    setwd("./UCI_data")
    unzip("uciDataSet.zip")
  }
  
  
  # loading the dataset files into data frames.
  X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
  subject_test <-
    read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
  subject_train <-
    read.table("./UCI HAR Dataset/train/subject_train.txt")
  
  activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
  features <- read.table("./UCI HAR Dataset/features.txt")
  
  
  # make a complete dataframe for test data by doing the following:
  # 1- subset the variables that have the mean or Sd only ( don't include angle measures)
  # 2- add a column for the subjects
  # 3- add a column for the activities with the appropriate name.
  #
  
  # (1)
  
  # subset the columns based on their names in the features file.
  # include all columns that have mean or std but exclude angle measurments.
  #( i didn't consider it mean values)
  
  finalTestDS <-
    X_test[grep("angle",grep("[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]",features$V2,value = T),invert = T)]
  
  # (2)
  
  
  finalTestDS <- finalTestDS %>% mutate(subject = subject_test[,"V1"])
  
  # (3)
  
  # add a factor to y_test that contain the descriptive names of the activities
  # levels/labels created based on the activity file.
  
  y_test <- mutate(y_test,V2 = factor(V1,labels = activity$V2))
  
  finalTestDS <- mutate(finalTestDS,activity = y_test[,"V2"])
  
  #print(dim(finalTestDS))
  
  # repeat the same steps for the train data set.
  
  # (1)
  
  # subset the columns based on their names in the features file.
  # include all columns that have mean or std but exclude angle measurments.
  #( i didn't consider it mean values)
  
  
  finalTrainDS <-
    X_train[grep("angle",grep("[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]",features$V2,value = T),invert = T)]
  
  # (2)
  
  
  finalTrainDS <-
    finalTrainDS %>% mutate(subject = subject_train[,"V1"])
  
  # (3)
  
  # add a factor to y_train that contain the descriptive names of the activities
  # levels/labels created based on the activity file.
  
  y_train <- mutate(y_train,V2 = factor(V1,labels = activity$V2))
  
  finalTrainDS <- mutate(finalTrainDS,activity = y_train[,"V2"])
  
  #print(dim(finalTrainDS))
  
  # now we will merge the test & train data sets (rows)
  
  finalDs <- rbind(finalTrainDS,finalTestDS)
  
  # Total number of rows should be 10299
  
  
  # now rename the columns
  
  # we will make a function that will replace some of the appreviations
  # with their meaning as per the features.info file
  # also we will use make.names() and gsub() to clean the names from the dots and prackets
  
  input_names = grep(
    "angle",
    grep("[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]",features$V2,value = T)
    ,value = T,invert = T
  )
  
  input_names <- make.names(input_names)
  input_names <- gsub("\\.","",input_names)
  
  descriptive_name <- function(input_names)
  {
    output_names <- sub("BodyBody","Body",input_names)
    output_names <- sub("Acc","Accelerometer",output_names)
    output_names <- sub("Gyro","Gyroscope",output_names)
    output_names <- sub("Mag","Magnitude",output_names)
    output_names <- sub("Freq","Frequency",output_names)
    output_names <- sub("mean","Mean",output_names)
    output_names <- sub("std","StandardDeviation",output_names)
    output_names <- sub("^t","TimeDomain",output_names)
    output_names <- sub("^f","FrequencyDomain",output_names)
  }
  
  
  names_vector <-
    c("Subject","Activity",descriptive_name(input_names))
  
  summaryDs <-
    finalDs %>% group_by(subject,activity) %>% summarise_each(funs(mean))
  
  names(summaryDs) <- names_vector
  
  
  # write the summaryDs to a file
  
  write.table(summaryDs,"./TidyDataSet.txt",row.names = F)
  
  paste("run_analysis completed and file TidyDataSet.txt was generated in this path",getwd())
  
}

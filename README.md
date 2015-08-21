# Getting & Cleaning Data Project

This is the Coursera DS specialization Getting and Cleaning Data project readme file

# Overview

In this project we have processed the raw dataset downloaded from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
to generate a tidy dataset that is representing the wide form of tidy data.

The raw dataset represents data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist.
The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

more information about the structure of the raw dataset is available in the `README.txt` text file that exists inside the raw data directory.

# The run_analysis.R 

#### prerequisites:

1. the `run_analysis.R` script requires the `dplyr` package to be installed.
2. it is recommended to download the dataset and uncompress it before running the script.
3. the working directory should be set to the parent directory of the `UCI HAR Dataset` directory.
4. if the `UCI HAR Dataset` was not found at the curent working directory the script will try to download it.
5. the download process may take long time and may fail if the platform is not `windows` so please try to download the dataset manually.
6. the dataset should remain untouched after download and uncompression.
7. the script will expect the `UCI HAR Dataset` directory to have the below files and subfolders.

   - `UCI HAR Dataset/activity_labels.txt`
   - `UCI HAR Dataset/features.txt`
   - `UCI HAR Dataset/test/X_test.txt`
   - `UCI HAR Dataset/test/y_test.txt`
   - `UCI HAR Dataset/test/subject_test.txt`
   - `UCI HAR Dataset/train/X_train.txt`
   - `UCI HAR Dataset/train/y_train.txt`
   - `UCI HAR Dataset/train/subject_train.txt`
   
   ######there are some other files and folders that exit inside the dataset but they are not used by the run_analysis.R script.

#### How does it work ?

as per the requirements in the project, the run analysis script does the following.

1) load the required packages.

- load the `dplyr` package.

2) locate the raw dataset.

- try to find the `UCI HAR Dataset` directory in the current working directory.
- if it exist then go to step (3).
- if it doesn't exist then create a subdirectory in the current working directory and try to download the dataset.
- uncompress the dataset and then go to step (3).

3) load the data into R

- read the activity_labels and features text files into dataframes.
- read the X, y and subject_test files for the test and train datasets into dataframes.

4) bind the data together.

##### operations on the columns.

- the below steps applies to both test and train datasets.
- first of all reduce the X data set from 561 columns to 79 only by selecting the mean and std measurements only based on the names in the feature file.
- I didn't consider the angle(mean) as a mean value however I have considered the meanFreq as mean value ( the assignment was not very clear about this point).
- bind the subject data with the X data (bind the subject to his measurments).
- create a factor of activity names based on the y files and the activity_labels data.
- bind the activity names to the subject and measurment data to have a total of 81 columns.

##### row operations.

- combine the two dataframes test and train in one big data frame using row binding.

5) giving descriptive names to the columns by manipulating the features file and based on the information in the features info file.

- I applied the same rule of selecting only features that have mean and std and neglected those who have angle in their names.
- I passed the resulting character vector of the above step to the make.names function to handle the special characters.
- remove the dots (.) generated by the make.names using the gsub function.
- the resulting vector was then passed to an internal function that does some substitutions based on the information available in the features info file.

    - BodyBody -> Body
    - Acc -> Accelerometer
    - Gyro -> Gyroscope
    - Mag -> Magnitude
    - Freq -> Frequency
    - mean -> Mean
    - std -> StandardDeviation
    - ^t -> TimeDomain
    - ^f -> FrequencyDomain
    

6) applying the wide form of tidy data and calculating the average of each measurement for each combination of subject/activity.

- first group the big dataframe generated in step (4) by subject and activity using the group_by function of the dplyr package.
- then using the summarise_each fantastic function generate the mean of each variable for each group which is equivalent to subject/activity.
- now apply the descriptive names generated in step (5) to the colnames of the resulting dataframe.
- finally write the dataframe to a file "TidyDataSet.txt" in the current working directory.



#### How to run the run_analysis.R script ?

- `run_analysis.R` contains a code for a function called `run_analysis()` so copying the function definition and content to your R studio or R console will define the `run_analysis()` function.
- you just have to download and uncompress the dataset then set the working directory to the parent directory of th `UCI HAR Dataset` folder and then type `run_analysis()`.
- when the function finish you will see a message that shows the name of the generated file (TidyDataSet.txt) and the path where it was generated.

```{R}
"run_analysis completed and file TidyDataSet.txt was generated in this path C:/your/path/here"
``` 


#### How to read the TidyDataSet.txt to R ?

- if you are still in the same directory where the `TidyDataSet.txt` file was generated then all you have to do is :

```{R}

> setwd("C:/your/path/here")
> TidyDataSet <- read.table("./TidyDataSet.txt",header=T)
> View(TidyDataSet)

```
- you can also read the tidy data set directly from the submitted dataset in the coursera project page.

```{R}

> TidyDS <- read.table("https://s3.amazonaws.com/coursera-uploads/user-a5743e0bf3132e2df0734eea/975115/asst-3/c54294a0481b11e5b3970d3996f36223.txt", header=T)
> View(TidyDS)

```








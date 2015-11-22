
#Project Getting and Cleaning data
# Veroniqalecita
#november 2015


library(data.table)
library(dplyr)

#Program for the analisis of the UCI HAR Dataset
#
##prerequisites: download an unzip the UCI HAR Dataset AND
#               set the ".../UCI HAR Dataset" as your working directory
#
# For download and unzip the data directly from R use the folowing code:
# if(!file.exists("./proyect_data")){dir.create("./proyect_data")}
# fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download.file(fileUrl,destfile="./proyect_data/Dataset.zip")
# unzip(zipfile="./data/Dataset.zip",exdir="./proyect_data")
#
#
## Seting the working directory that contains the unziped files:
# setwd("~/.../UCI HAR Dataset")

#Reading the data into R and Creating variables

library(data.table)
library(dplyr)

#reads the list of all features
features <- fread("./features.txt")

#Extract the name of the features in a vector variable
colVariables <- features$V2 

# Reads the activity names in a data table
activity_labels <- fread("./activity_labels.txt")

# Reads the training measures in a data table named X_train
X_train <-
  fread("./train/X_train.txt",  col.names = colVariables)
# create a new column with the variable setData and assigns 
# the "train" value to all the X_train data
X_train[,setData:= "train"]

# Reads the training labels in a data table
y_train <- fread("./train/y_train.txt")
# create a new column with the variable y_Label and assigns
# the values of the traingn labels to all the X_train data
X_train[,y_Label:= y_train]

# Reads the test measures in a data table named X_test
X_test <-
  fread("./test/X_test.txt", col.names = colVariables)
# create a new column with the variable setData and assigns 
# the "test" value to all the X_test data
X_test[,setData:= "test"]

# Reads the test labels in a data table
y_test <- fread("./test/y_test.txt")
# create a new column with the variable y_Label and assigns
# the values of the test labels to all the X_test data
X_test[,y_Label:= y_test]

# Reads the subject training labels in a data table
subject_train <- fread("./train/subject_train.txt")
# create a new column with the variable subject and assigns
# the values of the test labels to all the X_train data
X_train[,subject:= subject_train]

# Reads the subject test labels in a data table
subject_test <- fread("./test/subject_test.txt")
# create a new column with the variable subject and assigns
# the values of the test labels to all the X_test data
X_test[,subject:= subject_test]


# Create a list containing the test and de train data tables
L1 = list(X_train, X_test)
# Merge the data tables in a data table named allData by binding the rows
allData <- rbindlist(L1)

#Remove objects that are unnecesary now
rm(L1, X_train, X_test, y_train, y_test, features, subject_test, 
   subject_train, colVariables)

#Order the data by subyect and activity class label
arrange(allData, subject, y_Label)

# Generates a new variable with the corresponding activity name of the class labels
for (i in seq_along(activity_labels$V2)) {
  allData[y_Label == i,activity_label:= activity_labels$V2[i]]
}

# Erase the y_Label variable from the data table
allData[,y_Label:= NULL]


# Extracts the variables that contains the measurements on mean and std
# for each measuremen and the subject an activity_label that corresponds
# and generate a new data table with them
ExtractedData <- allData %>%
  select(subject, activity_label, contains("mean()"), contains("std()"))

#Remove objects that are unnecesary now
rm(activity_labels, allData)


#Create a new vactor with the variable names of the extracted data
VariableNames <- variable.names(ExtractedData)

# Creating the new variable names
for (i in 1:length(VariableNames)) 
{
  VariableNames[i] = gsub("\\()","",VariableNames[i])
  VariableNames[i] = gsub("-std","-Std",VariableNames[i])
  VariableNames[i] = gsub("-mean","-Mean",VariableNames[i])
  VariableNames[i] = gsub("^(t)","Time",VariableNames[i])
  VariableNames[i] = gsub("^(f)","FFT",VariableNames[i])
  VariableNames[i] = gsub("([Gg]ravity)","Gravity",VariableNames[i])
  VariableNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",VariableNames[i])
  VariableNames[i] = gsub("[Gg]yro","Gyroscope",VariableNames[i])
  VariableNames[i] = gsub("Acc","Acceleration",VariableNames[i])
  VariableNames[i] = gsub("[Jj]erk","DvDt",VariableNames[i])
  VariableNames[i] = gsub("[Mm]ag","Magnitude",VariableNames[i])
};

# Excange the variable names of the extracted data for the new ones
colnames(ExtractedData) <- VariableNames


#Remove objects that are unnecesary now
rm(VariableNames)

# Calculate the mean of every measure variable of the extracted data
# for every subject andgenerates a new data table
ExtractedDataMeans <- ExtractedData %>%
  group_by(subject, activity_label) %>%
  summarise_each(funs(mean))

#Remove objects that are unnecesary now
rm(ExtractedData)

# For saving in a .txt file, use the code:
 write.table(ExtractedDataMeans, 
           file = "./ExtractedDataMeans.txt", row.names = FALSE)


#Visulase the results
glimpse(ExtractedDataMeans)

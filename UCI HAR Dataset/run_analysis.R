###### set UCI HAR Dataset location after extracting the data as root folder ##########
setwd("~/Coursera/3 Getting and Cleaning Data/Course Project/UCI HAR Dataset")
#######################################################################################

# load the data.table package for faster processing #¿magic¿#
library("data.table")
# Check if root is indeed the extracted data folder, otherwise stop
if (!file.exists("features.txt"))
  stop("The features.txt could not be found. Set the proper directory!")

# For naming the data set with descriptive variable names (columns)
cNames <- fread("features.txt")
# For naming the activities in the data set:
activityLabels <- fread("activity_labels.txt")

### 2. Extracts only the measurements on the mean and standard deviation for each measurement. ###
# Filter on only -mean() and -std() columns. (mean and standard deviation for each measurement) in column 'V2'
cNames <- cNames[grepl("-(mean|std)\\(\\)", V2)]
# Function for reading and cleaning test and train data -> to be ready for merging
fooMerge <- function(data.set = "test") {
  # Collect all the subject IDs (per each observation)
  sData <- fread(sprintf("%s/subject_%s.txt", data.set, data.set))
  # Set column name where Subject IDs are stored
  setnames(sData, names(sData), c("Subject"))
  # Collect all the coordinates stored in y-files
  yData <- fread(sprintf("%s/y_%s.txt", data.set, data.set))
  
  ### 3. Uses descriptive activity names to name the activities in the data set ###
  # Convert yData to a factor vector with descriptive activity labels.
  yData <- factor(yData$V1, activityLabels$V1, activityLabels$V2, ordered = TRUE)
  # Convert to data.table 
  yData <- as.data.table(yData)
  # Set column name where Activity IDs are stored
  setnames(yData, names(yData), c("Activity"))
  # Collect all the coordinates stored in X-files
  xData <- read.table(sprintf("%s/X_%s.txt", data.set, data.set)) #fread doesn't seem to work with this dataset
  # convert to data.table
  xData <- as.data.table(xData)  
  # Filter out unnecessary columns: 
  # Create a list of xData columns, using IDs in cNames$V1
  cols <- names(xData)[cNames$V1]
  # Get only those columns
  xData <- xData[, cols, with=FALSE] #using data.table as data.frame to address 'cols'
  
  ### 4. Appropriately labels the data set with descriptive variable names.  ###
  # Set the column names accordingly
  setnames(xData, names(xData), cNames$V2)
  # Merge everything with cbind
  as.data.table(cbind(yData, sData, xData))
}
# Load Train and Test data
train <- fooMerge("train")
test <- fooMerge("test")

### 1. Merges the training and the test sets to create one data set.###
# Merge rows
data <- rbind(train, test)
# Data.table index (sort by activity and subject)
setkey(data, Activity, Subject)

### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Creating tidy data by grouping Activity and Subject and calculating the mean for the other columns
# .SD = subset of data, which mean is applied to (on columns not included in the groups)
tidydata <- data[, lapply(.SD, mean), by = .(Activity, Subject)]
# Save to .txt file 
write.table(tidydata, file = "TidyDataSet.txt", row.names = FALSE)

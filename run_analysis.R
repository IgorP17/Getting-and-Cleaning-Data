##0 - Preset steps
##Set path to git repo on local, because wd in R proj is different, 
##this step can be skipped
## OR, use Rscript to pass in first argument  
## path to your writable directory, that will be used as home directory

##Use something like this 
##d:\Learning\statistica\3-Data\proj\datasciencecoursera\3-data-course-proj>Rscript ./run_analysis.R "c:/temp/buya"

args<-commandArgs(TRUE)
if (is.na(args[1])){
        #Do nothing, expecting zip file and data dir in wd
        ##subpath <- "datasciencecoursera/3-data-course-proj/"
        ##setwd(paste(getwd(), "/", subpath, sep=""))
}else {setwd(args[1])}
##Download Data file
fileDownloadLink <- "https://d396qusza40orc.cloudfront.net/getdata/projectfiles/UCI%20HAR%20Dataset.zip"
zipFile <- "./UCI HAR Dataset.zip"
if (!file.exists(zipFile)){
        print("Zip file does not exists, downloading...")
        download.file(fileDownloadLink, destfile = zipFile, method = "curl")
        dateDownloaded <- date()
        dateDownloaded
}else print("Zip file exists, okay")

#Unzip files
if (!file.exists("UCI HAR Dataset")){
        print("Unzipped data not found, unzipping...")        
        unzip(zipFile)
}else print("Data seems to be unzipped")


#directoty. We can also get this info from zipFile, for example
directory <- "UCI HAR Dataset"

## 1) Merges the training and the test sets to create one data set
print("=== Running step 1 - load data ...")
path <- paste("./", directory, "/train/X_train.txt", sep="")
trainData <- read.table(path)
print("Train X Data loaded")
path <- paste("./", directory, "/test/X_test.txt", sep="")
testData <- read.table(path)
print("Test X Data loaded")

## activity labels
path <- paste("./", directory, "/activity_labels.txt", sep="")
activityLabels <- read.table(path)
print("Activity labels loaded")

## subject labels
path <- paste("./", directory, "/train/subject_train.txt", sep="")
subjectTrain <- read.table(path)
print("Train Subject Data loaded")
path <- paste("./", directory, "/test/subject_test.txt", sep="")
subjectTest <- read.table(path)
print("Test Subject Data loaded")

## read the test and training y labels
path <- paste("./", directory, "/train/y_train.txt", sep="")
y_train <- read.table(path)
path <- paste("./", directory, "/test/y_test.txt", sep="")
y_test <- read.table(path)
print("all Y Data loaded")

## merge y test and training activity labels
y_trainLabels <- merge(y_train,activityLabels,by="V1")
y_testLabels <- merge(y_test,activityLabels,by="V1")
print("Y data merged by activity labels")

## merge the test and training data and labels together
trainData <- cbind(subjectTrain,y_trainLabels,trainData)
print("Subj, Y and X data merged for train data")
testData <- cbind(subjectTest,y_testLabels,testData)
print("Subj, Y and X data merged for test data")

## merge the test and training data together
wholeMergedData <- rbind(trainData,testData)
print("All data merged")
print("=== Step 1 - load complete")
#head(wholeMergedData)

## 2) Extracts only the measurements on the mean and standard deviation for each measurement
print("=== Running step 2 - extracting data ...")
path <- paste("./", directory, "/features.txt", sep="")
featuresData <- read.table(path)
colnames(wholeMergedData) <- c("Subject","Activity_Id","Activity",as.character(featuresData$V2))
#colnames(wholeMergedData)
#get names for reqired columns
#requiredNamesBoolean <- grepl(".*(mean)|(std).*", colnames(wholeMergedData))
#Get id's of columns
meanColumns <- grep("mean()", colnames(wholeMergedData), fixed=TRUE)
stdColumns <- grep("std()", colnames(wholeMergedData), fixed=TRUE)
filteredCols <- c(meanColumns, stdColumns)
filteredCols <- sort(filteredCols)
#extract data, 1-3 cols are subject and activity
extractedData <- wholeMergedData[,c(1,2,3,filteredCols)]
#dim(extractedData)
#extractedData[1,1:6]
print("=== Step 2 - extraction complete ...")

## 3) Uses descriptive activity names to name the activities in the data set
## 4) Appropriately labels the data set with descriptive activity names
## this is done before

## 5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject

## melt the data
print("=== Running step 3 ...")
require(reshape2)
print("Melting data ...")
meltedData <- melt(extractedData, id=c("Subject","Activity"))
## cast the data back to the tidy data format
tidyData <- dcast(meltedData, formula = Subject + Activity ~ variable, mean)

## write the output into a file
print("Writing file with \t separator ...")
outPath <- "./mytidyset.txt"
write.table(tidyData, file=outPath, sep="\t", row.names=FALSE)
#colnames(tidyData)
#dim(tidyData)
print("=== Step 3 complete")
if (file.exists(outPath)){
        print("=== File exists")        
        print("=== Step 3 complete, All is OK")
} else message("!Something goes wrong, file does not exists")


## check you can read the tidyset 
#tidySet <- read.table("./mytidyset.txt", header=TRUE)
#dim(tidySet)
#colnames(tidySet)


##Here is another way to do step 5
#======#aggr <- aggregate(extractedData, list(Subject = as.numeric(extractedData$Subject), Activity = as.character(extractedData$Activity)), mean)
#In that way no additional cols will be created
aggr <- aggregate(. ~ Subject + Activity, data = extractedData,FUN = mean)
aggrSorted <- aggr[order(aggr$Subject),]
#TODO here we need adjust cols...
write.table(aggrSorted, file="./secondway.txt", sep="\t", row.names=FALSE)

print("Is 2 object created identical inside?")
all(tidyData == aggrSorted)
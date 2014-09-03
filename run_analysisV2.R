#run_analysisV2
#This should be correct script

#setwd("d:/Learning/statistica/git/Getting-and-Cleaning-Data/")

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

#Read variables file
path <- paste("./", directory, "/features.txt", sep="")
features <- read.table(path, stringsAsFactors = FALSE)

##Load Test data and set var names
path <- paste("./", directory, "/test/X_test.txt", sep="")
testData <- read.table(path, stringsAsFactors = FALSE, colClasses = "numeric")
names(testData) <- features$V2


##Load Train data and set var names
path <- paste("./", directory, "/train/X_train.txt", sep="")
trainData <- read.table(path, stringsAsFactors = FALSE, colClasses = "numeric")
names(trainData) <- features$V2


#Activities
path <- paste("./", directory, "/activity_labels.txt", sep="")
activityLabels <- read.table(path, stringsAsFactors = FALSE)

#Subjects
path <- paste("./", directory, "/test/subject_test.txt", sep="")
subjectTest <- read.table(path, stringsAsFactors = FALSE)
names(subjectTest) <- c("Subject")

path <- paste("./", directory, "/train/subject_train.txt", sep="")
subjectTrain <- read.table(path, stringsAsFactors = FALSE)
names(subjectTrain) <- c("Subject")


#Labels
path <- paste("./", directory, "/test/y_test.txt", sep="")
y_test <- read.table(path, stringsAsFactors = FALSE)

path <- paste("./", directory, "/train/y_train.txt", sep="")
y_train <- read.table(path, stringsAsFactors = FALSE)


###Match activity id and activity name
match.idx <- match(y_test$V1, activityLabels$V1)
y_test$Activity <- activityLabels[match.idx, 2]
names(y_test) <- c("ActivityID", "Activity")

match.idx <- match(y_train$V1, activityLabels$V1)
y_train$Activity <- activityLabels[match.idx, 2]
names(y_train) <- c("ActivityID", "Activity")


###Join Test data
allTestData <- cbind(subjectTest, y_test, testData)
allTrainData <- cbind(subjectTrain, y_train, trainData)
allData <- rbind(allTestData, allTrainData)

#посмотрим чего получилось:
addmargins(table(allData$Subject, allData$ActivityID))

#Теперь надо извлечь mean и std колонки
allNames <- names(allData)
#length(grep(".*mean().*", allNames, ignore.case = TRUE))
#length(grep(".*std().*", allNames, ignore.case = TRUE))
#length(grep("(.*std().*)|(.*mean().*)", allNames, ignore.case = TRUE))
reqiredVars <- grep("(.*std().*)|(.*mean().*)", allNames, ignore.case = TRUE)
#+первые 3 столбца
reqiredVars <- c(1,2,3,reqiredVars)

#Extract data
extractedData <- allData[,reqiredVars]

#do mean for Subj and Activity

resultDataSet <- aggregate(by = list(extractedData$Subject, extractedData$Activity), 
                                x = extractedData,
                                FUN = mean)
#length(names(resultDataSet))
#Remove Subject & Activity that has been agregated
resultDataSet <- resultDataSet[,c(1,2,4,6:length(names(resultDataSet)))]
names(resultDataSet)[1:2] <- c("Subject", "Activity") 

#Write data
write.table(resultDataSet, file="./tidyDataSetVersion2.txt", sep="\t", row.names=FALSE)


# ================================================================
# ##Aggregate
# aggrTemp <- aggregate(. ~ Subject + Activity, data = allTestData,
#                       FUN = mean)
# ####HMMMMMMM ????????
# aggrTemp <- aggregate(by = list(allTestData$Subject, allTestData$Activity), 
#                       x = allTestData,
#                       FUN = mean)

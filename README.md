# Initial task:  
You should create one R script called run_analysis.R that does the following.   
  
1. Merges the training and the test sets to create one data set.  
2. Extracts only the measurements on the mean and standard deviation for each measurement.   
3. Uses descriptive activity names to name the activities in the data set  
4. Appropriately labels the data set with descriptive variable names.   
5. Creates a second, independent tidy data set with the average of each  variable for each activity and each subject.   

# What is going on in run_analysis.R script and how to run it
##Running script
The script can be run in 2 ways:  

1. You can run it via Rscript ./run_analysis.R "c:/temp/xxx" - the last string is the parameter to your writable directory.   
This directory will be set to home directory when running script.  
To this directory will be downloaded zip file from:  
https://d396qusza40orc.cloudfront.net/getdata/projectfiles/UCI%20HAR%20Dataset.zip  
And extracted.  
Also, if zip file exists(it should be names as UCI HAR Dataset.zip) in this directory, then it will be not downloaded.   
And if data directory exists - I assume that it was unzipped.  
  
2. You can run it in RStudio.   
In that case you may skip first lines and execute code from line no.16 with comment "#Download Data file".  
Or, you can modify code above this line to set up your home directory  

##Next processing (note - script contains more details comments):  

1. Lines 16 - 30: Download and unzip data if necessary  
2. Lines 31 - 80: Load and merge Activity, Subject, Test and Train (X and Y) data.   
This is step 1 from task    
3. Lines 81 - 100: Extract only variables that contains "mean()" or "std()" in their column names.  
Why not "mean"? Because this approach states in features_info.txt.  
Also, here we set names for variables and activities   
This is steps 2 - 4 from task  
4. Lines 101 - 113: Melting data and calculate means 
This is step 5 from task  
5. Lines 114 - 126: Write data to the file mytidyset.txt with \t separator.  
6. Additional code, Line from 137: Using "aggregate" function do the step 5 again, then write file secondway.txt.  
Also, comapare data from first set to this one (tidyData == aggrSorted)  
   
You can manually compare output files by running file compare, for example on Windows:  
fc mytidyset.txt secondway.txt  
Comparing files mytidyset.txt and SECONDWAY.TXT  
FC: no differences encountered  



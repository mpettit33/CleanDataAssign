
Measure_Header <- read.table("./features.txt",sep=" ",header=FALSE) ##Variable names for measurements
Headers <- as.character(Measure_Header$V2) 
##Read in Training Data
Train_X_Train <- read.fwf("./train/X_train.txt",widths = rep.int(16,561),header=FALSE, col.names=Headers)
Train_Y_Train <- read.fwf("./train/y_train.txt",widths = 1,header=FALSE,col.names = "Activity")
Train_Subject <- read.fwf("./train/subject_train.txt",widths = 2,header=FALSE, col.names = "Subject")
Train_Data <- cbind(Train_Subject,Train_Y_Train,Train_X_Train)
##Read in Test Data
Test_X_Test <- read.fwf("./test/X_test.txt",widths = rep.int(16,561),header=FALSE, col.names=Headers)
Test_Y_Test <- read.fwf("./test/y_test.txt",widths = 1,header=FALSE,col.names = "Activity")
Test_Subject <- read.fwf("./test/subject_test.txt",widths = 2,header=FALSE, col.names = "Subject")
Test_Data <- cbind(Test_Subject,Test_Y_Test,Test_X_Test)
##Combine Data
Combined_Data <- rbind(Train_Data,Test_Data)
##Find indicies of mean() and std() measurements, will be 2 more than index returned because of Activity and Subject indicies
Mean_Std_Headers <- grep(x=Headers,pattern="[Mm]ean\\(\\)|[sS]td\\(\\)")+2 

Summary_Headers <- c(1,2,Mean_Std_Headers)
Summary_Data <- Combined_Data[Summary_Headers]
##Replace numeric activities with actual activities
Summary_Data$Activity <- revalue(as.character(Summary_Data$Activity),c("1" = "WALKING","2" = "WALKING_UPSTAIRS","3" = "WALKING_DOWNSTAIRS","4" =
               "SITTING","5" = "STANDING","6" = "LAYING"))
##Get final subject and activity data frames
Unique_Subject <- read.table(text = as.character(sort(rep(1:30,6))),header=FALSE,col.names="Subject")
Unique_Activity <- read.table(text = rep(c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"),30),
                              header=FALSE,col.names="Activity")
Activity_List <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
##Initialize summary dataframe for means
Unique_Summary <- read.table(text = "",col.names =Mean_Std_Headers)

##Loop through all subjects and each activity for the subject and calculate mean
for(i in 1:30){                    
   for(j in Activity_List){
        Subject_measure <- Summary_Data[(Summary_Data$Subject == i & Summary_Data$Activity == j),3:length(names(Summary_Data))]
        Subject_measure_means <- lapply(Subject_measure,FUN=mean)
        Unique_Summary <- rbind(Unique_Summary,Subject_measure_means)
   }  
     
}
##Join Subject and activity dataframes with means for each subject and activity 
Subject_Summary <- cbind(Unique_Subject,Unique_Activity,Unique_Summary)
##Write out final table to .txt file
write.table(Subject_Summary,file="./Summary.txt",sep= " ",row.names=FALSE)
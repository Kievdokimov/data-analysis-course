main<-function() {

    library(plyr)
    library(dplyr)
    library(reshape2)
    
    ### Download necessary data
    activity_labels<-read.table("./activity_labels.txt")
    features<-read.table("./features.txt")
    
    ### Train Sample
    x_train<-read.table("./train/X_train.txt")
    y_train<-read.table("./train/y_train.txt")
    subject_train<-read.table("./train/subject_train.txt")
        
    ### Test Sample
    x_test<-read.table("./test/X_test.txt")
    y_test<-read.table("./test/y_test.txt")
    subject_test<-read.table("./test/subject_test.txt")
    
    ### Find indexes for mean
    ## Find features with 'mean' in the name
    mean_num<-grep("mean",features[,2])
    ## Find features with 'meanFreq' in the name
    meanFreq_num<-grep("meanFreq",features[,2])
    ## Compose vectore which exludes meanFreq from mean vector
    mean_n<-setdiff(mean_num,meanFreq_num)
    
    ## Find indexes for std
    std_num<-grep("std",features[,2])
    ## Create index vector to pick only variables for mean and std
    mean_std_index<-union(mean_n,std_num)
    
    ## Unite train and test data. Pick only variables for only mean and std
    x<-rbind(x_train[,mean_std_index],x_test[,mean_std_index])
    names(x)<-features[mean_std_index,2]
    
    y<-rbind(y_train,y_test)
    subject<-rbind(subject_train,subject_test)
    names(subject)<-"subject"
    names(y)<-"Activity"
    
    ## Add columnt which correspond to observation number to save order during subsequent operations
    obs_num<-1:nrow(y)   
    data<-cbind(obs_num,subject,y,x)
    
    
    ## Sunstitute activity number by activity name. As merge function rearranges the order of observations
    ## we have arrange data frame into initial order
    data_m<-merge(data,activity_labels,by.x="Activity",by.y="V1")
    data_m <-subset(data_m, select=c(2,70,3:69))
    colnames(data_m)[2]<-"Activity"
    colnames(data_m)[1]<-"Observation_number"   
    data_m<-arrange(data_m,Observation_number)
    
    data_av<-select(data_m,-(Observation_number))
    result<-aggregate(. ~ Activity + subject, data=data_av, FUN=mean)
    
    
    
    
    
    
   
    
    
    
        
}
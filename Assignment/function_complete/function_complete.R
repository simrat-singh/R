#function returns a data frame where the first column is the name of
#the file and second column is number of completely observed cases
#LOGIC APPLIED: Extract rows with no NAs using "completecases", then count
#number of rows, which will give number complete cases.Then create two datasets
# having file IDs and number of completecases respective to each file.
#THen create a data_set using these two vectors

complete<-function(directory, id=1:332){

 files_list<- list.files(directory, full.names = TRUE)
 vec_id<-character(length = 0)   #created an empty character vector 
 vec_nobs<-character(length = 0)
 for(i in id){
   
   data_set<-read.csv(files_list[i])
   no_na_data<-data_set[complete.cases(data_set),] #extracts a data set with no NAs
   nobs<-nrow(no_na_data)  # counts the number of rows in completecases dataset
   vec_id<-c(vec_id, i)   # concatenate al the IDs
   vec_nobs<-c(vec_nobs, nobs)  #concatenate all the nobs
 }
 clean_data_set<-data.frame(id=vec_id, nobs=vec_nobs)  #create a dataset with vectors
 print(clean_data_set)
}
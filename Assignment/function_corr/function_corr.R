#Function takes a diectory and a threshold as arguements
#It calculates correlation for the files having complete cases more than
#the given threshold
#LOGIC: As directed in the assignment function "complete" has been sourced, though
#functionality can be achieved without using it.
#STEP1: Get a list of complete cases and their complete cases from "complete" function
#STEP2: Get a list of complete cases having complete cases greated than the threshold
#STEP3: Get a subset of file IDs and complete cases for the files with complete cases greater than the threshold
#STEP4: Get a list of IDs of the files having complete cases greater than the threshold
#STEP5: Read the files using IDs extracted in STEP4 and find the correlation

corr<-function(directory, threshold=0){
  
  data_frame_completecases<-complete(directory) #Sources function "complete" to get list of files and their respective complete cases
  print("Done calling function 'complete'")
  data_set_th<-as.numeric(as.character(data_frame_completecases$nobs))>threshold #Get list of files having complete cases more than the threshold.
  temp<-subset(data_frame_completecases,data_set_th) # Get subset of files with complete cases more than the threshold
  vec_cr = numeric(length = 0)
  ids<-as.numeric(as.character(temp$id)) #Get vector of IDs of files having complete cases greater than the threshold
  files_list<- list.files(directory, full.names = TRUE)
  for(i in ids){
    data_set<-read.csv(files_list[i])
    data_set<-data_set[complete.cases(data_set),]
    cr<-cor(data_set$sulfate, data_set$nitrate)
    vec_cr<-c(vec_cr, cr)
  }
  vec_cr
}
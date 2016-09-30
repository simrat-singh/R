corr<-function(directory, threshold=0){
  
  data_frame_completecases<-complete(directory)
  print("Done calling function 'complete'")
  data_set_th<-as.numeric(as.character(data_frame_completecases$nobs))>threshold
  temp<-subset(data_frame_completecases,data_set_th)
  vec_cr = numeric(length = 0)
  ids<-as.numeric(as.character(temp$id))
  files_list<- list.files(directory, full.names = TRUE)
  for(i in ids){
    data_set<-read.csv(files_list[i])
    data_set<-data_set[complete.cases(data_set),]
    cr<-cor(data_set$sulfate, data_set$nitrate)
    vec_cr<-c(vec_cr, cr)
  }
  vec_cr
}
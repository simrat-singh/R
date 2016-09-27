pollutantmean<-function(directory, pollutant, id){

  print("Function 'pollutantmean' started")
  files_list<-list.files(directory, full.names = TRUE)
  data_set<-data.frame()
  for(i in id){
    data_set<-rbind(data_set, read.csv(files_list[i]))
  }
  print(nrow(data_set))
  head(data_set)
  sulfate_dataset<-data_set[, "nitrate"]
  mean(sulfate_dataset, na.rm = TRUE)
}
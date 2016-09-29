#The function takes the directory, pollutant and target files as arguements
# and returns the mean pollutant in the target files

#'directory' is a charcter vectorindicating the location of source csv files
#'pollutant' is a charcter vector indictating the name of the pollutant for
#which mean is calculated. Considering the data available it will be either
#'sulfate' or 'nitrate'

#Return value is the mean

pollutantmean<-function(directory, pollutant, id){

  print("Function 'pollutantmean' started")
  files_list<-list.files(directory, full.names = TRUE)
  data_set<-data.frame()
  for(i in id){
    data_set<-rbind(data_set, read.csv(files_list[i]))
  }
  #pollutant_dataset<-data_set[, grep(pollutant, colnames(data_set))]
  pollutant_dataset<-data_set[, which(colnames(data_set)==pollutant)]
  mean(pollutant_dataset, na.rm = TRUE)
}
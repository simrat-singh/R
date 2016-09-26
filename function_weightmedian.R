weightmedian<-function(directory, day){
  
  file_list<-list.files(directory, full.names = TRUE)
  data_frame<-data.frame()
  for(i in 1:5){
    data_frame<-rbind(data_frame, read.csv(file_list[i]))
  }
  #print(data_frame)
  #print("Number of rows in data_frame are:")
  #print(nrow(data_frame))
  temp<-data_frame[, "Day"]           #Subsets all the rows and colmun "Day"
  temp<-which(temp==day)              #Subsets all the rows for value of day
  data_frame_subset<-data_frame[temp,]#Subsets entire data for the value of day
  print(data_frame_subset)            #Prints the final subset to calculate median
  median(data_frame_subset[, "Weight"], na.rm = TRUE)
}
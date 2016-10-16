#Function to get list of hospitals on each state for an specific outcome and rank
#PROBLEM STATEMENT:- The function takes outcome and num as parameters and returns a 2-column data frame
#containing the hospital in each state that has the ranking specified in num.
#For detailed problem statement refer to "4 Ranking hospitals in all states" in ProgAssignment3.pdf in repo
#Logic applied:-
#1.Read the data and get list of unique states and sort it alphbatecially
#2.Iterate the list and for each state call findhospital method of function_rankhospital to get hospital
#  in each state that has ranking specified in num
#3.rbind the state name and hospital to create the final data frame


source('helperFunctions.R')
rankall<-function(outcome, num='best'){
  
  dataset<-read.csv("outcome-of-care-measures.csv")
  #hosp_outcome<-head(dataset[,c(2,7,11,17,23)],10)
  hosp_outcome<-dataset[,c(2,7,11,17,23)]
  
  #changing names of columns to short and precise
  names(hosp_outcome)[1]<-"hospital_name"
  names(hosp_outcome)[2]<-"state"
  names(hosp_outcome)[3]<-"heart_attack"
  names(hosp_outcome)[4]<-"heart_failure"
  names(hosp_outcome)[5]<-"pneumonia"
  
  state_list<-sort(unique(hosp_outcome[,2], incomparables = FALSE)) #Getting list of states and sort is alphabatically
  outcome_list<-c("heart attack", "heart failure", "pneumonia") #Create a list of possible outcomes
  options(warn = -1) #Supress warnings
  result<-data.frame()
  
  for (state in state_list) {
    
    #Source findhospitals() method of function_rankhospital.R
    hospital<-findhospital(as.character(state), outcome, num, hosp_outcome)
    result<-rbind(result, data.frame("hospital"=hospital, "state"=as.character(state)))
  }
  result
}
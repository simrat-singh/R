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
  
  #Call helper function to load and create relevant subset of the data  
  hospitals<-readAndCleanData('outcome-of-care-measures.csv')  
  
  state_list<-sort(unique(hospitals[,2], incomparables = FALSE)) #Getting list of states and sort is alphabatically
  outcome_list<-c("heart attack", "heart failure", "pneumonia") #Create a list of possible outcomes
  options(warn = -1) #Supress warnings
  result<-data.frame()
  
  for (state in state_list) {
    
    #Source findhospitals() method of function_rankhospital.R
    hospital<-findhospital(as.character(state), outcome, num, hospitals)
    result<-rbind(result, data.frame("hospital"=hospital, "state"=as.character(state)))
  }
  result
}

#Test cases
#head(rankall("heart attack", 20), 10)
#Expected outcome
#hospital                             state
# <NA>                                AK
#D W MCMILLAN MEMORIAL HOSPITAL       AL
#ARKANSAS METHODIST MEDICAL CENTER    AR
#JOHN C LINCOLN DEER VALLEY HOSPITAL  AZ
#SHERMAN OAKS HOSPITAL                CA
#SKY RIDGE MEDICAL CENTER             CO
#MIDSTATE MEDICAL CENTER              CT
# <NA>                                DC
# <NA>                                DE
#SOUTH FLORIDA BAPTIST HOSPITAL       FL
#Finding best, worst or an hospital with specific rank for a gievn outcome in given state
#PROBLEM STATEMENT:- The function takens state, outcome and a num as arguements to give an hopistal name
#If num = best, hospital with lowest mortality rate for outcome in the given state is returned
#If num = worst hospital with highest mortality rate for outcome in the given state is returned
#If num = <rank>, hospital with mortality rate as per the rank for outcome in the given state is returned
#For detailed problem statement refer to "3 Ranking hospitals by outcome in a state" in ProgAssignment3.pdf in repo

#Logic Applied
#1. The function sources methods from helperFunctions.R to get a subset of hospitals in a particular state 
#   ordered by mortality rate for the given ourcome
#2. Return hospital in 1st row for num=best, last row for num=worst and as per the rank for num=rank

source('helperFunctions.R')

rankhospital<-function(state, outcome, num='best'){
    
    #Call helper function to load and create relevant subset of the data  
    hospitals<-readAndCleanData('outcome-of-care-measures.csv')  
    
    state_list<-unique(hospitals[,2], incomparables = FALSE) #Getting list of states
    outcome_list<-c("heart attack", "heart failure", "pneumonia") #Create a list of possible outcomes
    temp<-matrix() # A temporary matrix to store intermediate results
    options(warn = -1) #Supress warnings
    
    #Validation for state
    if(!state %in% state_list){
      stop("Invalid State")
    }
    
    #Validation for outcome
    if(!outcome %in% outcome_list){
      stop("Invalid outcome")
    }
    
    hospital<-findhospital(state, outcome,num,hospitals)
    hospital
    
}

#Test cases
#Expected outcome->FORT DUNCAN MEDICAL CENTER

#rankhospital("TX", "heart failure", "best")
#Expected outcome->
#rankhospital("TX", "heart failure", 4)
#Expected outcome->DETAR HOSPITAL NAVARRO

#rankhospital("MD", "heart attack", "worst")
#Expected outcome->HARFORD MEMORIAL HOSPITAL

#rankhospital("MN", "heart attack", 5000)
#Expected outcome-> NA

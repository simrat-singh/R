#Finding best, worst or an hospital with specific rank for a gievn outcome in given state
#PROBLEM STATEMENT:- The function takens state, outcome and a num as arguements to give an hopistal name
#If num = best, hospital with lowest mortality rate for outcome in the given state is returned
#If num = worst hospital with highest mortality rate for outcome in the given state is returned
#If num = <rank>, hospital with mortality rate as per the rank for outcome in the given state is returned
#For detailed problem statement refer to "3 Ranking hospitals by outcome in a state" in ProgAssignment3.pdf in repo

#Logic Applied
#1. The function sources methods from function_best.R to get a subset of hospitals in a particular state 
#   ordered by mortality rate for the given ourcome
#2. Return hospital in 1st row for num=best, last row for num=worst and as per the rank for num=rank

source('function_best.R')

rankhospital<-function(state, outcome, num='best'){
    dataset<-read.csv("outcome-of-care-measures.csv")
    #hosp_outcome<-head(dataset[,c(2,7,11,17,23)],10)
    hosp_outcome<-dataset[,c(2,7,11,17,23)]
    
    #changing names of columns to short and precise
    names(hosp_outcome)[1]<-"hospital_name"
    names(hosp_outcome)[2]<-"state"
    names(hosp_outcome)[3]<-"heart_attack"
    names(hosp_outcome)[4]<-"heart_failure"
    names(hosp_outcome)[5]<-"pneumonia"
    
    state_list<-unique(hosp_outcome[,2], incomparables = FALSE) #Getting list of states
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
    
    hospital<-findhospital(state, outcome,num,hosp_outcome)
    hospital
    
}

findhospital<-function(state, outcome, num, hosp_outcome){
  
    if(outcome=='heart attack'){
      hosp_outcome_heartattack<-heartAttack(hosp_outcome)
      bestHospital_subset<-findBestHosp(hosp_outcome_heartattack, state)
    }
    
    if(outcome=='heart failure'){
      
      hosp_outcome_heartfailure<-heartFailure(hosp_outcome)
      bestHospital_subset<-findBestHosp(hosp_outcome_heartfailure, state)
    }
    
    if(outcome=='pneumonia'){
      
      hosp_outcome_pneumonia<-pneumonia(hosp_outcome)
      bestHospital_subset<-findBestHosp(hosp_outcome_pneumonia, state)
    }
    
    if(num=='best'){
      result<-as.character(bestHospital_subset[1,1]) #Returns first hospital of the ordered list
    }else if(num=='worst'){
      result<-as.character(bestHospital_subset[nrow(bestHospital_subset),1])#Returns last hospital of the ordered list
    }else{
      result<-as.character(bestHospital_subset[num,1])# Returns hospital as per the rank in num
    }
  
    result
}
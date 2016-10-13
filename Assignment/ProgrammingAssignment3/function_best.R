#Finding the hospital with lowest mortality rate for Heart Attack, Heart Failure and Pneumonia
#Problem Statement:-
#The function reads a csv, takes outcome and state as parameter
#and returns the hospital in the state with lowest mortality rate for the outcome
#For detailed problem statement refer to "2 Finding the best hospital in a state" in ProgAssignment3.pdf in the repo  

# Logic Applied - 
#1. Read the file and create list of valid states and outomes
#2. Validate inputs against valid states and outcomes, if not valid throw back error message.
#3. Create a subset of state, hospital and outcome to ensure smallest possible dataset is being processed
#4. Create a further smaller subset(of one created in step 3) just to inclide rows specific to the state arguement
#5. Remove NAs from the subset created and step 4 and pass it on to another function to find the best hospital
#6. THe function findBestHosp(), retuns the hospital with lowest mortality rate


best<-function(state, outcome){
  
  dataset<-read.csv("outcome-of-care-measures.csv")
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
  
  if(!state %in% state_list){
    stop("Invalid State")
  }
  
  if(!outcome %in% outcome_list){
    stop("Invalid outcome")
  }
  
  if(outcome=='heart attack'){
    hosp_outcome_heartattack<-heartAttack(hosp_outcome)
    bestHospital<-findBestHosp(hosp_outcome_heartattack, state)
  }
  
  if(outcome=='heart failure'){
    
    hosp_outcome_heartfailure<-heartFailure(hosp_outcome)
    bestHospital<-findBestHosp(hosp_outcome_heartfailure, state)
  }
  
  if(outcome=='pneumonia'){
    
    hosp_outcome_pneumonia<-pneumonia(hosp_outcome)
    bestHospital<-findBestHosp(hosp_outcome_pneumonia, state)
  }
  bestHospital[1,][1]
}


pneumonia<-function(hosp_outcome){
  
  #Getting subset of hospitnal name, state and heat_attack columns
  hosp_outcome_pneumonia<-hosp_outcome[,c("hospital_name", "state","pneumonia")]
  
  #Converting factor to numeric in columns
  hosp_outcome_pneumonia$pneumonia<-as.numeric(as.character(hosp_outcome_pneumonia$pneumonia))
  
  #Creating a temporary subset exclusing NAs
  hosp_outcome_pneumonia_noNAs<-subset(hosp_outcome_pneumonia, !is.na(hosp_outcome_pneumonia$pneumonia))
  hosp_outcome_pneumonia_noNAs
}

heartFailure<-function(hosp_outcome){
  
  #Getting subset of hospitnal name, state and heat_attack columns
  hosp_outcome_heartfailure<-hosp_outcome[,c("hospital_name", "state","heart_failure")]
  
  #Converting factor to numeric in columns
  hosp_outcome_heartfailure$heart_failure<-as.numeric(as.character(hosp_outcome_heartfailure$heart_failure))
  
  #Creating a temporary subset exclusing NAs
  hosp_outcome_heartfailure_noNAs<-subset(hosp_outcome_heartfailure, !is.na(hosp_outcome_heartfailure$heart_failure))
  hosp_outcome_heartfailure_noNAs
}

heartAttack<-function(hosp_outcome){
  #Getting subset of hospitnal name, state and heat_attack columns
  hosp_outcome_heartattack<-hosp_outcome[,c("hospital_name", "state","heart_attack")]
  
  #Converting factor to numeric in columns
  hosp_outcome_heartattack$heart_attack<-as.numeric(as.character(hosp_outcome_heartattack$heart_attack))
  
  #Creating a temporary subset exclusing NAs
  hosp_outcome_heartattack_noNAs<-subset(hosp_outcome_heartattack, !is.na(hosp_outcome_heartattack$heart_attack))
  hosp_outcome_heartattack_noNAs
}


findBestHosp<-function(hosp_shorlisted, state){
  
  #Creating a subset from temporary subset to have rows specific to state
  hosp_outcome_state<-hosp_shorlisted[hosp_shorlisted$state==state,]
  
  #Order the temp subset to break ties, first level of sorting is column heart_attack and second is name of the hospital
  ordered_rows<-(order(hosp_outcome_state[,3], hosp_outcome_state[,1]))
  
  #Getting a subset on the basis of rows returned by order
  final_subset<-hosp_outcome_state[ordered_rows,]
  
}
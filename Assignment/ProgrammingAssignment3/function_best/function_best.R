best<-function(state, outcome){
  
  dataset<-read.csv("C:/DS/R/Assignment/ProgrammingAssignment3/function_best/outcome-of-care-measures.csv")
  hosp_outcome<-dataset[,c(2,7,11,17,23)]
  
  #changing names of columns to short and precise
  names(hosp_outcome)[1]<-"hospital_name"
  names(hosp_outcome)[2]<-"state"
  names(hosp_outcome)[3]<-"heart_attack"
  names(hosp_outcome)[4]<-"heart_failure"
  names(hosp_outcome)[5]<-"Pneumonia"
  
  state_list<-unique(hosp_outcome[,2], incomparables = FALSE) #Getting list of states
  outcome_list<-c("heart attack", "heart failure", "Pneumonia") #Create a list of possible outcomes
  temp<-matrix() # A temporary matrix to store intermediate results
  options(warn = -1) #Supress warnings
  
  if(!state %in% state_list){
    stop("Invalid State")
  }
  
  if(!outcome %in% outcome_list){
    print("checking outcome")
    stop("Invalid outcome")
  }
  
  if(outcome=='heart attack'){
    hosp_outcome_heartattack<-heartAttack(hosp_outcome)
    bestHospital<-findBestHosp(hosp_outcome_heartattack, state)
  }
  
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
  final_subset[1,][1]
}
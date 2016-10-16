#Finding the hospital with lowest mortality rate for Heart Attack, Heart Failure and Pneumonia
#PROBLEM STATEMENT:-
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

source('helperFunctions.R')
best<-function(state, outcome){
    
    #Call helper function to load and create relevant subset of the data  
    hospitals<-readAndCleanData('outcome-of-care-measures.csv')  
    
    state_list<-unique(hospitals[,2], incomparables = FALSE) #Getting list of states
    outcome_list<-c("heart attack", "heart failure", "pneumonia") #Create a list of possible outcomes
    options(warn = -1) #Supress warnings
    
    #Validation for state
    if(!state %in% state_list){
      stop("Invalid State")
    }
    
    #Validation for outcome
    if(!outcome %in% outcome_list){
      stop("Invalid outcome")
    }
    
    
    hospitals_outcome<-getHospOutcome(hospitals,outcome)
    bestHospital<-findBestHosp(hospitals_outcome, state)
    bestHospital[1,][1]
  }
  

#Test cases
#best("TX", "heart attack")
#best("MD", "heart failure")
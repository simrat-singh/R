  readAndCleanData<-function(fileName){
    
    dataset<-read.csv(fileName)
    data_subset<-dataset[,c(2,7,11,17,23)]
    
    #changing names of columns to short and precise
    names(data_subset)[1]<-"hospital_name"
    names(data_subset)[2]<-"state"
    names(data_subset)[3]<-"heart_attack"
    names(data_subset)[4]<-"heart_failure"
    names(data_subset)[5]<-"pneumonia"
    
    data_subset
  }
  
  getHospOutcome<-function(dataset, outcome){
    
      if(outcome == 'pneumonia'){
    
        #Converting factor to numeric in columns
        outcome_subset<-dataset[,c("hospital_name", "state","pneumonia")]
        outcome_subset$pneumonia<-as.numeric(as.character(outcome_subset$pneumonia))
        outcome_subset_noNAs<-subset(outcome_subset, !is.na(outcome_subset$pneumonia))
      }else if(outcome=='heart attack'){
        
        #Converting factor to numeric in columns
        outcome_subset<-dataset[,c("hospital_name", "state","heart_attack")]
        outcome_subset$heart_attack<-as.numeric(as.character(outcome_subset$heart_attack))
        outcome_subset_noNAs<-subset(outcome_subset, !is.na(outcome_subset$heart_attack))
      }else if(outcome=='heart failure'){
        
        #Converting factor to numeric in columns
        outcome_subset<-dataset[,c("hospital_name", "state","heart_failure")]
        outcome_subset$heart_failure<-as.numeric(as.character(outcome_subset$heart_failure))
        outcome_subset_noNAs<-subset(outcome_subset, !is.na(outcome_subset$heart_failure))
      }
  }
  
  findBestHosp<-function(hosp_shorlisted, state){
    
    #Creating a subset from temporary subset to have rows specific to state
    hosp_outcome_state<-hosp_shorlisted[hosp_shorlisted$state==state,]
    
    #Order the temp subset to break ties, first level of sorting is column heart_attack and second is name of the hospital
    ordered_rows<-(order(hosp_outcome_state[,3], hosp_outcome_state[,1]))
    
    #Getting a subset on the basis of rows returned by order
    final_subset<-hosp_outcome_state[ordered_rows,]
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
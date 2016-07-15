rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
	states_factor <- factor(data$State)
	states_levels <- levels(states_factor)
	    
	outcome_vector <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
		       
	## Check that state and outcome are valid
	if (!(state %in% states_levels)) {
	    stop("invalid state")
	}
	    
	outcome_col <- outcome_vector[outcome]
	if (is.na(outcome_col)) {
	    stop("invalid outcome")
	} 
	
	
	## Return hospital name in that state with the given rank 30-day death rate
	sub_data <- subset(data, State == state, select = c(2, outcome_col))
	colnames(sub_data) <- c("hospital", "outcome")
	sub_data <- sub_data[order(sub_data$outcome, sub_data$hospital, na.last = NA), ]
    
    if(num == "best") {
    	return(sub_data[1, "hospital"])
    } else if(num == "worst") {
    	return(sub_data[nrow(sub_data), "hospital"]) 
    }
    
    return(sub_data[num, "hospital"])
    	
}
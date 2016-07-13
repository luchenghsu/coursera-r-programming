rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character" )	    
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
	sub_data[, 2] <- as.numeric(sub_data[, 2])	
	ranks <- sub_data[order(sub_data[, 2], sub_data[, 1], na.last = NA), ]
    worst <- nrow(ranks)
    
    if(num == "best") {
    	return(ranks[1,1])
    } else if(num == "worst") {
    	return(ranks[worst, 1]) 
    }
    
    return(ranks[num, 1])
    	
}
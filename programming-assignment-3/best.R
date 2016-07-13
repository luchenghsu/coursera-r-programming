best <- function(state, outcome) {
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
	    
	    ## Return hospital name in that state with lowest 30-day death rate
	    sub_data <- subset(data, State == state, select = c(2, outcome_col))
	    sub_data[, 2] <- as.numeric(sub_data[, 2])
	    min_rate <- min(sub_data[, 2], na.rm = TRUE)
        name <- sort(as.character(subset(sub_data, sub_data[, 2] == min_rate, select= c(1))[, 1]))[1] 
        name    
}
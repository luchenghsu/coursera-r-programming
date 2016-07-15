rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
	states_factor <- factor(data$State)
	states_levels <- levels(states_factor)

	
	## Check that state and outcome are valid
	outcome_vector <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    outcome_col <- outcome_vector[outcome]
	if (is.na(outcome_col)) {
	    stop("invalid outcome")
	} 
	
	## For each state, find the hospital of the given rank
	sub_data <- subset(data, select = c(2, 7, outcome_col))
	colnames(sub_data) <- c("hospital", "state", "outcome")
	sub_data <- sub_data[order(sub_data$state, sub_data$outcome, sub_data$hospital, na.last = NA), ]
	sub_data <- split(sub_data, sub_data$state)
	
	list_names <- list()
    if(num == "best") {
    	list_names <- lapply(sub_data, function(x) x[1, "hospital"])
    } else if(num == "worst") {
    	list_names <- lapply(sub_data, function(x) x[nrow(x), "hospital"])
    } else {
    	list_names <- lapply(sub_data, function(x) x[num, "hospital"])
    }
	
	## Return a data frame with the hospital names and the (abbreviated) state name
	rank_all = data.frame(hospital = unlist(list_names), state = names(list_names), row.names = names(list_names))
	rank_all
}
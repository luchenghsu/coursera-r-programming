pollutantmean <- function(directory, pollutant, id = 1:332) {
    file_list <- list.files(path=directory, pattern=".csv")
    
    data <- data.frame()

    for(i in id) {
    	temp = read.csv(paste(directory, file_list[i], sep="/"), header = TRUE)
        data <- rbind(data, temp)
    }

    sub <- data[, pollutant]
    round(mean(sub, na.rm = TRUE), 3)	
}
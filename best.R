
outcomefilename <- "outcome-of-care-measures.csv"
#outcome can be "heart attack", "heart failure" and "pneumonia".

best <- function(state, outcome) {
	data <- read.csv(outcomefilename, colClasses = "character")
	frame <- data[which(data$State == state), ]
	indices <- c()
	finalcut <- NULL
	if(!dim(frame)[1]) { stop("invalid state") }
	if(outcome == "heart attack")
	{
		finalcut <- data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack		
	} else if(outcome == "heart failure") {
		finalcut <- data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
	} else if(outcome == "pneumonia") {
		finalcut <- data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia	
	} else {
		stop("invalid outcome")
	}
	finalcut <- as.numeric(finalcut)
	for(i in 1:length(finalcut))
	{
		if(length(indices) == 0)
		{
			indices <- c(i)
		} else if(is.na(finalcut[i])) {
			#ignore because value is NA
		} else if(finalcut[i] < finalcut[indices[1]]) {
			indices <- c(i)
		} else if(finalcut[i] == finalcut[indices[1]]) {
			indices <- c(indices, i)	
		} else if(finalcut[i] > finalcut[indices[1]]) {
			
		} else if(should_not_go_here <- TRUE) {
			print("Something is wrong :(")
		}
	}
	#print(indices)
	#lowest <- c()
	#for(i in indices)
	#{
	#	lowest <- c(lowest, frame[i])
	#}		
	#lowest
	indices
}

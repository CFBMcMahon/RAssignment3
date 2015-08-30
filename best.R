
outcomefilename <- "outcome-of-care-measures.csv"
#outcome can be "heart attack", "heart failure" and "pneumonia".

best <- function(state, outcome) {
	data <- read.csv(outcomefilename, colClasses = "character")
	checkIfStateExists <- data[which(data$State == state), ]
	indices <- c(-1)
	finalcut <- NULL
	if(!dim(checkIfStateExists)[1]) { stop("invalid state") }
	if(outcome == "heart attack")
	{
		#attackfinalcut <- data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack		
		finalcut <- data$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
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
		if(!is.na(finalcut[i])) {
			if(length(indices) == 0)
			{
				indices <- c(i)
			} else {
				if(finalcut[i] < finalcut[indices[1]])
				{
					indices <- c(i)
				} else if(finalcut[i] == finalcut[indices[1]]) 
				
				{
					indices <- c(indices, i )
				}
			}
					
		}
	}
	lowest <- c()
	for(i in indices)
	{
		lowest <- c(lowest, data$Hospital.Name[i])
	}		
	lowest <- lowest[order(lowest)]
	lowest
	
}

justFrame <- function(state, outcome) {
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
}
#		if(is.na(finalcut[i])) {
#			#ignore because value is NA
#		 if(finalcut[i] < finalcut[indices[1]]) {
#			indices <- c(i)
#			print("indices < : " + indices)
#		} else if(finalcut[i] == finalcut[indices[1]]) {
#			indices <- c(indices, i)	
#			print("Indices == : " + indices)
#		} else if(finalcut[i] > finalcut[indices[1]]) {
#			
#		} else if(should_not_go_here <- TRUE) {
#			print("Something is wrong :(")
#		}
#	}

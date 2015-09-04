outcomefilename <- "outcome-of-care-measures.csv"
#outcome can be "heart attack", "heart failure" and "pneumonia".
rankall <- function(outcome, num = "best") {
	rank <- NULL
	data <- read.csv(outcomefilename, colClasses = "character")
#	if(is.numeric(num)){
#		rank <- num
#	} else if(is.character(num)){
#		if(num == "best") {
#			rank <- 1
#		} else if(num == "worst"){
#			rank <- nrow(inState)
#		}
#	} else {
#		stop("invalid rank")
#	}
	states <- split(data, data$State)		
	hospitalsInState <- data.frame(hospital=c(), state=c())
	for( state in states ) {
		inState <- state
		statetitle <- NULL
		for(statename in state$State )
		{
			if(!is.na(statename)) { statetitle <- statename; break}
		}
		if(outcome == "heart attack")
		{
			inState <- inState[order(as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), state$Hospital.Name), ]
			inState <- inState[which(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available"), ]
		#datum[which(datum$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available"), ]
		} else if(outcome == "heart failure") {
			inState <- inState[order(as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), inState$Hospital.Name) , ]
			inState <- inState[which(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available"), ]
		} else if(outcome == "pneumonia") {
			inState <- inState[order(as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), inState$Hospital.Name), ]
			inState <- inState[which(inState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available"), ]
		} 
	temp <- data.frame(hospital=inState$Hospital.Name[num], state=statetitle)
	hospitalsInState <- rbind(hospitalsInState, temp)
	}
	hospitalsInState	
}

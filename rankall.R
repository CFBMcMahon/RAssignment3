outcomefilename <- "outcome-of-care-measures.csv"
#outcome can be "heart attack", "heart failure" and "pneumonia".
rankall <- function(outcome, num = "best") {
	data <- read.csv(outcomefilename, colClasses = "character")
	states <- split(data, data$State)		
	hospitalsInState <- data.frame(hospital=c(), state=c())
	for( state in states ) {
		inState <- state
		statetitle <- NULL
		rank <- NULL
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
			inState <- inState[order(as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), state$Hospital.Name) , ]
			inState <- inState[which(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available"), ]
		} else if(outcome == "pneumonia") {
			inState <- inState[order(as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), state$Hospital.Name), ]
			inState <- inState[which(inState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available"), ]
		} else {
			stop("invalid outcome") 
		}
		if(is.numeric(num)){
			rank <- num
		} else if(is.character(num)){
			if(num == "best") {
				rank <- 1
			} else if(num == "worst"){
				rank <- nrow(inState)
			}
		} else {
			print("Error!")
		}

		temp <- data.frame(hospital=inState$Hospital.Name[rank], state=statetitle)
		hospitalsInState <- rbind(hospitalsInState, temp)
	}
	hospitalsInState	
}

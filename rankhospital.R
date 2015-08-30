
outcomefilename <- "outcome-of-care-measures.csv"
#outcome can be "heart attack", "heart failure" and "pneumonia".

rankhospital <- function(state, outcome, num = "best") {
	rank <- NULL
	if(num == "best")
	{
		rank <- 1
	}
	data <- read.csv(outcomefilename, colClasses = "character")
	inState <- data[which(data$State == state), ]
	ranked <- NULL	
	if(!dim(checkIfStateExists)[1]) { stop("invalid state") }
	if(outcome == "heart attack")
	{
		ranked <- checkIfStateExists[order(as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), ]
	} else if(outcome == "heart failure") {
		ranked <- checkIfStateExists[order(as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), ]
	} else if(outcome == "pneumonia") {
		ranked <- checkIfStateExists[order(as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), ]
	} else {
		stop("invalid outcome")
	}
	ranked
#	
#	finalcut <- as.numeric(finalcut)
#	lowest <- NULL
#	indices <- NULL
#	for(i in 1:length(finalcut))
#	{
#		if(!is.na(finalcut[i])) {
#			rowState <- data$State[i]
#			rowRate <- finalcut[i]
#			if(rowState == state){
#				if(is.null(lowest))
#				{
#					lowest <- rowRate
#					indices <- c(i)
#				} else {
#					if(rowRate < lowest)
#					{
#						lowest <- rowRate
#						indices <- c(i)
#					} else if(rowRate == lowest){
#						indices <- c(indices, i)
#					} else {
#	
#					}	
#				}
#			}
#		}
#	}
#	lowestNames <- c()
#	for(i in indices)
#	{
#		lowestNames <- c(lowestNames, data$Hospital.Name[i])
#	}		
#	lowestNames <- lowestNames[order(lowestNames)]
#	lowestNames[1]
	
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

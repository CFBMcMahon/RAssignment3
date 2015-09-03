
outcomefilename <- "outcome-of-care-measures.csv"
#outcome can be "heart attack", "heart failure" and "pneumonia".
rankhospital <- function(state, outcome, num = "best") {
	rank <- NULL
	data <- read.csv(outcomefilename, colClasses = "character")
	inState <- data[which(data$State == state, ), ]
	if(dim(inState)[2] < 1)
	{
		return(NA)
	}
	print("point: 1")
	
	columnName <- if(outcome == "heart attack")
	{
		"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	} else if(outcome == "heart failure") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	} else if(outcome == "pneumonia") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	} else {
		NA
	}
	if(is.na(columnName))
	{
		return (NA)
	}

	#na.last
	str(inState[columnName])
	return(inState[columnName])
	
#	print("checkpoint1")
#	return(order(inState[columnName, ], as.factor(inState[columnName, ])))
}

#if(outcome == "heart attack")
#	{
#		heartAttacks <- as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
#		output <- inState[which(!is.na(heartAttacks)), ]
#		outputNames <- output$Hospital.Names
#		outputRates <- output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
#	} else if(outcome == "heart failure") {
#		heartFailures <- as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
#		output <- inState[which(!is.na(heartFailures)), ]
#		outputNames <- output$Hospital.Names
#		outputRates <- output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
#	} else if(outcome == "pneumonia"){
#		pneumonias <- as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
#		output <- inState[which(!is.na(Pneumonias)), ]
#		outputNames <- output$Hospital.Names
#		outputRates <- output$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
#	} else {
#		stop("invalid outcome")
#	}
#	
	#get rank
#	for(i in 2:length(outputNames))
#	{
#		print(i)
#		beforename <- outputNames[i - 1]
#		print(beforename)
#		beforedeathrate <- outputRates[i - 1]
#		currentname <- outputNames[i]
#		currentdeathrate <- outputRates[i] 
#		if(beforedeathrate == currentdeathrate){
#			if(order(c(beforename, currentname))[1] != 1){
#				outputNames[i - 1] <- currentname
#				outputNames[i] <- beforename 
#			}
#		}
#	}
#	output[, rank]


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

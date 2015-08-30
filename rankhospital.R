
outcomefilename <- "outcome-of-care-measures.csv"
#outcome can be "heart attack", "heart failure" and "pneumonia".
rankhospital <- function(state, outcome, num = "best") {
	rank <- NULL
	data <- read.csv(outcomefilename, colClasses = "character")
	inState <- data[which(data$State == state, ), ]
	output <- NULL
	if(outcome == "heart attack")
	{
		heartAttacks <- as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
		output <- inState[which(!is.na(heartAttacks)), ]
		output <- output[order(as.numeric(output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), ]
		c(output$Hospital.Name, as.numeric(output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
	} else if(outcome == "heart failure") {
		heartFailures <- as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
		output <- inState[which(!is.na(heartFailures)), ]
		output <- output[order(as.numeric(output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), ]
		c(output$Hospital.Name, as.numeric(output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
	} else if(outcome == "pneumonia"){
		pneumonias <- as.numeric(inState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
		output <- inState[which(!is.na(Pneumonias)), ]
		output <- output[order(as.numeric(output$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), ]
		c(output$Hospital.Name, as.numeric(output$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
	} else {
		stop("invalid outcome")
	}

	#get rank
	if(num == "best")
	{
		rank <- 1
	} else if(num == "worst") {
		rank <- length(output$Hospital.Name)
	} else if(is.numeric(num)) {
		if(num > length(inState) | num < 1)
		{
			return(NA)
		} else {
			rank <- num
		}
	} else {
		
	}
	for(i in 2:length(output))
	{
		beforename <- output[1, i - 1]
		beforedeathrate <- output[2, i - 1]
		currentname <- output[1, i]
		currentdeathrate <- output[2, i] 
		if(beforedeathrate == currentdeathrate){
			if(order(c(beforename,  currentname))[1] != 1){
				output[1, i - 1] <- currentname
				output[1, i] <- beforename 
			}
		}
	}
	output
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

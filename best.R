
outcomefilename <- "outcome-of-care-measures.csv"
#outcome can be "heart attack", "heart failure" and "pneumonia".

best <- function(state, outcome) {
	
	data <- read.csv(outcomefilename, colClasses = "character")
	frame <- data[which(data$State == state), ]
	if(!dim(frame)[1]) { stop("invalid state") }
	if(outcome == "heart attack")
	{
		as.numeric(justHeartAttack)
	} else if(outcome == "heart failure") {
		
	} else if(outcome == "pneumonia") {

	} else {
		stop("invalid outcome")
	}
}

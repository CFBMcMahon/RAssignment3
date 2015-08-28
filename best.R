
outcomefilename <- "outcome-of-care-measures.csv"
#outcome can be "heart attack", "heart failure" and "pneumonia".

best <- function(state, outcome) {
	data <- read.csv(outcomefilename)
	frame <- data[which(data$State == state), ]
	frame 
}

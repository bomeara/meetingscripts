data <- read.delim("SSB.tsv", stringsAsFactors=FALSE)
library(plyr)
library(gender)
library(R.utils)

MakePrettyTable <- function(x) {
	return(t(t(sort(table(x), decreasing=TRUE))))	
}


first <-(t(t(sort(table(data$Top.choice.for.workshop), decreasing=TRUE))))
first.df <- data.frame(wksp = rownames(first), first=first, stringsAsFactors=FALSE)
second <-(t(t(sort(table(data$Second.choice.for.workshop), decreasing=TRUE))))
second.df <- data.frame(wksp = rownames(second), second=second, stringsAsFactors=FALSE)
third <-(t(t(sort(table(data$Third.choice.for.workshop), decreasing=TRUE))))
third.df <- data.frame(wksp = rownames(third), third=third, stringsAsFactors=FALSE)
workshop.counts <- join(join(third.df, second.df), first.df)
workshop.counts[is.na(workshop.counts)] <- 0
workshop.counts <- workshop.counts[order(workshop.counts$first, decreasing=TRUE),]

GetFirstName <- function(x) {
	return(strsplit(x, " ")[[1]][1])	
}

data$first.name <- sapply(as.character(data$Name), GetFirstName)
gender.records <- unlist(unname(sapply(tolower(data$first.name), gender, years=c(1950, 1995), certainty=FALSE)[2,]))
data$gender <- gender.records

print(MakePrettyTable(data$gender))
print(MakePrettyTable(data$Career.stage))

print(workshop.counts)

workshops <- unique(workshop.counts$wksp)
for (workshop.index in sequence(length(workshops))) {
	data.local <- data[which(data$Top.choice.for.workshop==workshops[workshop.index]),]
	if(dim(data.local)[1]>0) {
		print(workshops[workshop.index])
		print(MakePrettyTable(data.local$gender)/sum(table(data.local$gender)))	
		print(MakePrettyTable(data.local$Career.stage)/sum(table(data.local$gender)))	
	}
	
}

data.no.rabo <- data[!grepl("Computational Macroevolution", data$Top.choice.for.workshop),] #b/c Dan's is a full day
data.no.rabo <- data.no.rabo[!grepl("No workshop", data.no.rabo$Top.choice.for.workshop),]
data.no.rabo <- data.no.rabo[which(nchar(data.no.rabo$Top.choice.for.workshop)>0),]
workshops.no.rabo <- workshops[!grepl("Computational Macroevolution",workshops)]
workshops.no.rabo <- workshops.no.rabo[!grepl("No workshop", workshops.no.rabo)]
workshops.no.rabo <- workshops.no.rabo[which(nchar(workshops.no.rabo)>0)]

AssignToWorkshops <- function(workshop.schedule, preferences) {
	workshop.assignments <- rep(NA, 2)
	workshop.assignments[1] <- preferences[1]
	score <- 1
	available.workshops <- names(workshop.schedule)[which(workshop.schedule != workshop.schedule[which(names(workshop.schedule)==workshop.assignments[1])])]
	second.match <- which(available.workshops==preferences[2])
	third.match <- which(available.workshops==preferences[3])
	if (length(second.match)>0) {
		workshop.assignments[2] <- available.workshops[second.match]
		score <- score + 2
	} else if(length(third.match)>0) {
		workshop.assignments[3] <- available.workshops[third.match]
		score <- score + 3
	} else {
		score <- score + 5 #significant penalty	
	}
	return(list(score=score, workshop.assignments=workshop.assignments))
}

ConvertLineToPreferences <- function(x) {
	return(c(x$Top.choice.for.workshop, x$Second.choice.for.workshop, x$Third.choice.for.workshop))	
}

intToBinWithPadding <- function(x, length.out=6) {
	int <- intToBin(x)
	if(nchar(int)>length.out) {
		stop("int was too long")
	}
	if(nchar(int)==length.out) {
		return(int)	
	} else {
		return(paste(paste(rep(0, length.out - nchar(int)), collapse="", sep=""), int, sep=""))	
	}
}

SplitBinaryString <- function(x) {
	result <- strsplit(x, "")[[1]]
	result <- as.numeric(result)	
	return(result)
}


score.df <- data.frame()

possible.assignments <- sequence(2^6)
possible.assignments <- possible.assignments[-length(possible.assignments)] #since the last one is too high
for (assignment.number in possible.assignments) {
	print(assignment.number)
	score.total <- 0
	workshop.schedule <- SplitBinaryString(intToBinWithPadding(assignment.number))
	names(workshop.schedule) <- workshops.no.rabo
	for (person in sequence(dim(data.no.rabo)[1])) {
		score.total <- 	score.total + AssignToWorkshops(workshop.schedule, ConvertLineToPreferences(data.no.rabo[person,]))$score
	}
	temp.df <- cbind(data.frame(assignment.number=assignment.number), data.frame(t(workshop.schedule)), data.frame(score=score.total))
	if(assignment.number == 1) {
		score.df <- temp.df	
	} else {
		score.df <- rbind(score.df, temp.df)	
	}
	
}

best.arrangement <- which.min(score.df$score)
print(t(score.df[best.arrangement,]))

#Set to where it is on your PC
setwd("~/Desktop/F1")
df<-read.csv("Schumacher.csv")

#View data
df 


#Histogram of any numerical results.
hist(as.numeric(df$Result))
library(fitdistrplus)

x<-as.numeric(df$Result)
x<-x[!is.na(x)]
plotdist(x, histo = TRUE, demp = TRUE)


stats<-descdist(x) #This ascualtl shows it as a Beta distribution, but I just used normal.
simulated<-rnorm(10000,mean=stats$mean,sd=stats$sd)
simulated<-as.integer(simulated[!simulated<1])


#Compare
positionBreaks=c(0,1,2,3,4,5,6,7,9,10,11,12,13,14,15,16)
par(mfrow = c(1, 2))
hist(as.numeric(df$Result),main="Wikpedia Data",breaks=positionBreaks)
hist(simulated, breaks=positionBreaks)

# Would Renault Schumacher beat Benetton Schmacher
# Needs more data but you need to add to CSV so only one Ferrari season for this.

renault<-df[64:82,] 
ford<-df[12:64,] 

hist(as.numeric(renault$Result),main="Renault",breaks=positionBreaks)
hist(as.numeric(ford$Result), main="Ford", breaks=positionBreaks)

#Remove NAs created by converting DNF etc to number, which isn't possible.
#Generally you don't want NAs around, they cause problems.
resultsRenault<-as.numeric(renault$Result)[!is.na(as.numeric(renault$Result))]
resultsFord<-as.numeric(ford$Result)[!is.na(as.numeric(ford$Result))]

statsRenault<-descdist(as.numeric(resultsRenault))
statsFord<-descdist(as.numeric(resultsFord))

#Simulate
simulatedRenault<-rnorm(10000,mean=statsRenault$mean,sd=statsRenault$sd)
simulatedRenault<-as.integer(simulatedRenault[!simulatedRenault<1])

simulatedFord<-rnorm(10000,mean=statsFord$mean,sd=statsFord$sd)
simulatedFord<-as.integer(simulatedFord[!simulatedFord<1])

hist(simulatedRenault,main="Simulated Renault",breaks=positionBreaks)
hist(simulatedFord, main="Simulated Ford", breaks=positionBreaks)

#We can now make position differences.
#We have a slight issue, removing negative results above, means the lists 
#are not the same length.
#We can just use the shorter length then.

simulations<-min(length(simulatedRenault),length(simulatedFord))

combinedResults<-cbind(simulatedRenault[1:simulations],simulatedFord[1:simulations])

colnames(combinedResults)<-c('Renault','Ford')

#Removed draws as not possible
combinedResults<-combinedResults[!combinedResults[,'Renault']==combinedResults[,'Ford'],]
#Create new matrix for Scores
combinedScore<-combinedResults

#Convert Position to score - will break if position > 16
score<-matrix(c(18,15,12,10,8,6,4,2,1,0,0,0,0,0,0),nrow=16)

combinedScore[,'Renault']<-score[combinedResults[,'Renault']]
combinedScore[,'Ford']<-score[combinedResults[,'Ford']]


#Okay now to run some virtual races
NoOfRaces<-5
raceResults<-combinedScore[sample(1:nrow(combinedScore),NoOfRaces),]

pointsDifference<-raceResults[,'Renault']-raceResults[,'Ford']

#Look at if if you want
pointsDifference

#If negative Ford one these five races.
sum(pointsDifference)

#Lets model sessions of five races.

pointsDifferenceTable<-vector(length=1000)
for (n in 1:1000) {
  NoOfRaces<-5
  raceResults<-combinedScore[sample(1:nrow(combinedScore),NoOfRaces),]
  pointsDifferenceTable[n]<-sum(raceResults[,'Renault']-raceResults[,'Ford'])
}

hist(pointsDifferenceTable)

#Not had time to do DNFs, bet that can be modeled into combined score matrix
#As just getting 0 score x% or time. 


rm(list = ls())
library(lubridate)
library(maptools)
library( rgdal)
library(sp)
 library(censusapi)


if ( dir.exists("C:/Users/Administrator/Documents/311") )	(setwd(
	"C:/Users/Administrator/Documents/311")) else if (
	dir.exists("G:/math/311") )	{setwd("G:/math/311")} else {	
	setwd("C:/Users/michael/Documents/311") }

calls<-read.csv("311v2.csv",header=T)

head(calls)
as.data.frame(sort(table(calls$SERVICECODEDESCRIPTION)))

potholes<-calls[which(calls$SERVICECODEDESCRIPTION == "Streetlight Repair Investigation" ),]
head(potholes)
table(round(potholes$duration))
cor(potholes[which(potholes$duration > 15),24:42])

as.data.frame(names(potholes))

ncol(potholes)

durlist<-list()

for ( i in 32:108){

	aaa<-as.character(as.data.frame(sort(table(
		calls$SERVICECODEDESCRIPTION)))[i,1])
	potholes<-calls[which(calls$SERVICECODEDESCRIPTION == aaa ),]
	Q <- max(2)
	durlist[[i-31]]<-as.numeric( cor( potholes[which(potholes$duration < 
		Q ),24:ncol(potholes)])[,1] )
}



lapply( durlist, function(x)	x>.3 )


































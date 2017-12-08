rm(list = ls())
library(lubridate)
library(maptools)
library( rgdal)
library(sp)
 
 


if (dir.exists("C:/Users/Administrator/Documents/311")){
	setwd("C:/Users/Administrator/Documents/311")} else
	{setwd("G:/math/311")}

calls<-read.csv("calls.csv",header=T)

#laptop


 for(i in 88853:167484	){

	calls$block[i]<-as.numeric(latlong2fips(latitude=calls$LATITUDE[i], 
		longitude=calls$LONGITUDE[i])) /1000

}


(167485:nrow(calls))



#work
 for(i in 167485:nrow(calls)	){

	calls$block[i]<-as.numeric(latlong2fips(latitude=calls$LATITUDE[i], 
		longitude=calls$LONGITUDE[i])) /1000

}
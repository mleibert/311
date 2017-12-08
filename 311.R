
if (dir.exists("C:/Users/Administrator/Documents/311")){
	setwd("C:/Users/Administrator/Documents/311")} else
	{setwd("G:/math/311")}
	

calls<-read.csv("311a.csv",header=T)
#remove unresolved cases
calls<-(	calls[-which(calls$RESOLUTIONDATE == ""),]	)
head(calls$X)


head(calls)


#Remove these columns
#dnw<-c("CITY","STATE","SERVICEORDERDATE","DETAILS","STREETADDRESS",
#	"SERVICECALLCOUNT")
# calls<-calls[,-which(names(calls) %in% dnw )]
 
##write.csv(calls,"311.csv")
#if(!require(RCurl)){    install.packages("RCurl")
#    library(RCurl)} else {library(RCurl)}

head(calls)

##Zip to tract
tractzip<-read.csv("tractzip.csv",header=T)
head(tractzip);nrow(tractzip)
tractzip<-(tractzip[which(tractzip[,2] == 11),])
##write.csv(tractzip,"tractzip.csv")



## Commerical density
comden<-read.csv("comden.csv",header=T)
comdeninfo<-read.csv("BP_2015_00CZ1_metadata.csv",header=T)
names(comden)[2]<-"ZIPCODE"
head(comden)
dnw<-c("GEO.display.label","NAICS.id","NAICS.display.label",
	"YEAR.id","GEO.id","PAYQTR1")
 comden<-comden[,-which(names(comden) %in% dnw )]
head(comden)
names(comden)[3]<-"paid.employees";names(comden)[4]<-"annual.payroll"


calls<-merge(calls,comden,"ZIPCODE")
head(calls) 

#files<-list.files()
#files<-grep("ACS", files,value=TRUE)
#filess<-paste0(substr(files,1,nchar(files)-4),"_with_ann.csv")
#for(i in 1:length(files)){unzip(files[i],filess[i] )}



files<-list.files()
files<-grep("acs", files,value=TRUE)
censusdf<-list()

#CALLS<-calls
#calls<-CALLS

for( i in 1:length(files)){ 	censusdf[[i]]<-read.csv(files[i],header=T)}
for( i in 1:length(files)){ calls<-merge(calls,censusdf[[i]],"ZIPCODE")	}
head(censusdf[[i]])
head(calls)

calls<-(calls[order(calls$X),] )
names(calls)[26:length(names(calls))]

#write.csv(calls,"311.csv")

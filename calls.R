rm(list = ls())
library(lubridate)
library(maptools)
library( rgdal)
library(sp)
 
 


if (dir.exists("C:/Users/Administrator/Documents/311")){
	setwd("C:/Users/Administrator/Documents/311")} else
	{setwd("G:/math/311")}
	

calls<-read.csv("311.csv",header=T)
head(calls)
names(calls)
a<-seconds_to_period(head(
	ymd_hms(calls$RESOLUTIONDATE )-ymd_hms(calls$ADDDATE  )))
head(a)
(a)


calls$duration<-as.numeric(ymd_hms(calls$RESOLUTIONDATE )-
	ymd_hms(calls$ADDDATE ))/60/60/24



#Remove these columns
dnw<-c("XCOORD","YCOORD","STATUS_CODE","PRIORITY","INSPECTORNAME",
	"MARADDRESSREPOSITORYID")
calls<-calls[,-which(names(calls) %in% dnw )]

calls<-(calls[,-c(4:5)])
calls$X<-1:nrow(calls);names(calls)[3]<-"ID"

#######


head(calls)
calls$block<-NA

10222+78631
for(i in  76462:88853	){

	calls$block[i]<-as.numeric(latlong2fips(latitude=calls$LATITUDE[i], 
		longitude=calls$LONGITUDE[i])) /1000

}














#####

calls$ontime<-as.numeric(ymd_hms(calls$SERVICEDUEDATE)-
	ymd_hms(calls$RESOLUTIONDATE))/60/60/24
calls$Iontime<-ifelse(calls$ontime > 0,1,0)
calls$Iontime<-as.factor(calls$Iontime)
calls$paid.employees<-as.numeric(calls$paid.employees)
calls$annual.payroll<-as.numeric(calls$annual.payroll)

head(calls)
names(calls)
test<-calls[ which(calls$ZIPCODE == 20024), ]
head(test)
sapply(w8, class)

plot((w8$population),w8$duration)

null= lm(w8$duration ~ 1, data=w8)
full <- ( lm(w8$duration ~.,data=w8) )
step(null, scope=list(lower=null, upper=full), direction="forward")

plot(jitter(w8$population,22),w8$duration)
jitter(1,12)

meanz<-rep(0,length(unique(calls$ZIPCODE)))

for(i in 1:length(meanz)){
	meanz[i]<-mean(calls[ which(calls$ZIPCODE == unique(calls$ZIPCODE )[i]
		), 		]$duration	)
}
length(18:30)
test<-data.frame(meanz)

for( i in 1:nrow(test)){
	for(j in 2:length(17:30)){
		test[i,j]<-calls[ (which(calls$ZIPCODE == unique(calls$ZIPCODE
			 )[i])[1]), j+16]
}}



head(test)

names(test)[2:ncol(test)]<-names(calls)[18:30]
sapply(test, class)


test<-test[,-which(names(test) %in% "duration" )]
test<-test[ which(test$population>1 ),]


 
null= lm(meanz ~ 1, data=test)
full <- ( lm(meanz ~.,data=test) )
step(null, scope=list(lower=null, upper=full), direction="forward")


cor(test)

#write.csv(calls,"311b.csv")
#CALLS<-calls

head(calls)
calls[88853+1,]

callz<-read.csv("311dd.csv",header=T)

calls[88852:88862,]
calls[137484:nrow(callz),ncol(calls)]<-callz[137484:nrow(callz),2]

callz[137484,]


137484

which(is.na(calls$block))




rm(list = ls())
library(lubridate)
library(maptools)
library( rgdal)
library(sp)
 library(censusapi)

 


if (dir.exists("C:/Users/Administrator/Documents/311")){
	setwd("C:/Users/Administrator/Documents/311")} else
	{setwd("G:/math/311")}
	
censuskey<-"4d33360124eb6a83fbc841960cfc66ff8eb13a8d"
calls<-read.csv("311.csv",header=T)
head(calls)
names(calls)

 
files<-list.files();files<-grep("ACS", files,value=TRUE)
files<-gsub('ACS_15_5YR_', '', files);files<-gsub('.zip', '', files)
files<-files[-(length(files)-2)]

  
 dnw<-c("paid.employees", "annual.payroll", "Gini", "Households", 
		"vechicles","Median.income", 	"population" ,"traveltime")
calls<-calls[,-which(names(calls) %in% dnw )]
head(calls)

####	connect blocks to tracts
blktract<-read.csv("ACS_15_5YR_B08301_with_ann.csv",header=T)
blktract<-blktract[-1,2:3]
head(blktract)

blktract[,2]<-gsub(", District of Columbia, District of Columbia", '', 
	blktract[,2])
blktract[,2]<-gsub('Bl.+ct','',blktract[,2])
blktract[,2]<-gsub(" ", '', 	blktract[,2])
blktract[,2]<-as.character(blktract[,2])
blktract[,1]<-as.character(blktract[,1])
 calls$block<-as.character(round(as.numeric(calls$block,0)))

head(calls)
calls$tract<-rep(NA,nrow(calls))
for(i in 1:nrow(blktract)){
	calls[which(calls$block == blktract[i,1] ),ncol(calls)]<-blktract[i,2]
}

#border calls
nrow(calls[which(is.na(calls$tract)),])


def<-read.csv("def.csv",header=F)
def
census<-list()


###########

for(i in 1:nrow(def)){
	BB<-def[i,1];options(stringsAsFactors = FALSE) 
	dat<-read.csv(paste0("ACS_15_5YR_",BB,"_with_ann.csv"),header=T)
 	if( identical(grep('Margin', as.character(dat[1,]), value=TRUE), 
		character(0)) == F ){
	dat<-dat[, -which((grepl("Margin", dat[1,]))) ]}
	blerg<-as.character(dat[1,]);blerg<-gsub('Estimate; ', '', blerg)
	names(dat)<-blerg;rm(blerg)
	dat<-dat[-1,]
	head(dat)
	census[[i]]<-dat;rm(dat)
}


head(census[[1]])
census[[1]]<-census[[1]][,-1]
census[[1]][,2]<-gsub(", District of Columbia, District of Columbia", '', 
	census[[1]][,2])
census[[1]][,2]<-gsub('Bl.+ct','',census[[1]][,2])
census[[1]][,2]<-gsub(" ", '', 	census[[1]][,2])

for(i in 3:ncol(census[[1]])){census[[1]][,i]<-as.numeric(census[[1]][,i])}

census[[1]]$Pdrive<-(census[[1]][,4])/census[[1]][,3]
dat<-census[[1]][,c(1,2,3,4,ncol(census[[1]]))]
head(dat);names(dat)[3:5]<-c("Commuters","Drivers","Pdrive")
head(calls)
names(dat)[1]<-"block"

calls<-merge(calls,dat, "block")
attach(calls);calls<-(calls[order(ID),] );detach(calls)
head(calls)

#
def
head(census[[2]])	#Skip

#
def;k=3;def[k,]
head(census[[k]])	 
census[[k]]<-census[[k]][,-1]
census[[k]][,2]<-gsub(", District of Columbia, District of Columbia", '', 
	census[[k]][,2])
census[[k]][,2]<-gsub('Bl.+ct','',census[[k]][,2])
census[[k]][,2]<-gsub(" ", '', 	census[[k]][,2])

names(census[[k]])[3]<-"Income"

census[[k]][,3]<-(gsub("-", "0", census[[k]][,3]))
census[[k]][,3]<-as.numeric(gsub("250,000\\+", "250000", census[[k]][,3]))
names(census[[k]])[1]<-"block"

calls<-merge(calls,census[[k]][,-2], "block")
attach(calls);calls<-(calls[order(ID),] );detach(calls)
head(calls)


#
k=4;def[k,]
head(census[[k]])	 

head(census[[k]])	 
census[[k]]<-census[[k]][,-1]
census[[k]][,2]<-gsub(", District of Columbia, District of Columbia", '', 
	census[[k]][,2])
census[[k]][,2]<-gsub('Bl.+ct','',census[[k]][,2])
census[[k]][,2]<-gsub(" ", '', 	census[[k]][,2])

census[[k]][,3]<-(gsub("-", "0", census[[k]][,3]))
census[[k]][,3]<-as.numeric(gsub("3,500\\+", "3500", census[[k]][,3]))

names(census[[k]])[1]<-"block"
names(census[[k]])[3]<-"Rent"

calls<-merge(calls,census[[k]][,-2], "block")
attach(calls);calls<-(calls[order(ID),] );detach(calls)
head(calls)


 

# By Tract
k=5;def[k,]
head(census[[k]])	 

census[[k]]<-census[[k]][,-1]
census[[k]][,2]<-gsub(", District of Columbia, District of Columbia", '', 
	census[[k]][,2])
census[[k]][,2]<-gsub('C.+ct','',census[[k]][,2])
census[[k]][,2]<-gsub(" ", '', 	census[[k]][,2])

census[[k]][,3]<-as.numeric(gsub("-", "0", census[[k]][,3]))

names(census[[k]])[2]<-"tract"
names(census[[k]])[3]<-"Gini"

calls<-merge(calls,census[[k]][,-1], "tract")
attach(calls);calls<-(calls[order(ID),] );detach(calls)
head(calls)


 
#
k=6;def[k,]
head(census[[k]])	 

census[[k]]<-census[[k]][,-1]
census[[k]][,2]<-gsub(", District of Columbia, District of Columbia", '', 
	census[[k]][,2])
census[[k]][,2]<-gsub('Bl.+ct','',census[[k]][,2])
census[[k]][,2]<-gsub(" ", '', 	census[[k]][,2])

for(j in 3:ncol(census[[k]])){census[[k]][,j]<-as.numeric(census[[k]][,j])}


names(census[[k]])[1]<-"block"
names(census[[k]])[3]<-"Households"
names(census[[k]])[4]<-"Occupied"
names(census[[k]])[5]<-"Vacant"


calls<-merge(calls,census[[k]][,-2], "block")
attach(calls);calls<-(calls[order(ID),] );detach(calls)
head(calls)


#
k=7;def[k,]
head(census[[k]])	 


census[[k]]<-census[[k]][,-1]
census[[k]][,2]<-gsub(", District of Columbia, District of Columbia", '', 
	census[[k]][,2])
census[[k]][,2]<-gsub('C.+ct','',census[[k]][,2])
census[[k]][,2]<-gsub(" ", '', 	census[[k]][,2])

census[[k]][which(census[[k]][,3] == "N"),]
#62.02: 102,110
#68.04: 68.01, 68.02


census[[k]][which(census[[k]][,2] == "62.02"),3]<-mean(c(
	as.numeric(census[[k]][which(census[[k]][,2] == "102"),3]) ,
	as.numeric(census[[k]][which(census[[k]][,2] == "110"),3])))
census[[k]][which(census[[k]][,2] == "68.04"),3]<-mean(c(
	as.numeric(census[[k]][which(census[[k]][,2] == "68.01"),3]) ,
	as.numeric(census[[k]][which(census[[k]][,2] == "68.02"),3])))


census[[k]][,3]<-as.numeric( census[[k]][,3] )


names(census[[k]])[2]<-"tract"
names(census[[k]])[3]<-"commute.time"

calls<-merge(calls,census[[k]][,-1], "tract")
attach(calls);calls<-(calls[order(ID),] );detach(calls)
head(calls)

 

#
k=8;def[k,]
head(census[[k]])	 

census[[k]]<-census[[k]][,-1]
census[[k]][,2]<-gsub(", District of Columbia, District of Columbia", '', 
	census[[k]][,2])
census[[k]][,2]<-gsub('Bl.+ct','',census[[k]][,2])
census[[k]][,2]<-gsub(" ", '', 	census[[k]][,2])


names(census[[k]])[1]<-"block"
names(census[[k]])[3]<-"Population"

census[[k]][,3]<-as.numeric( census[[k]][,3] )

calls<-merge(calls,census[[k]][,-2], "block")
attach(calls);calls<-(calls[order(ID),] );detach(calls)
head(calls)



#
k=9;def[k,]
head(census[[k]] )	 

names(census[[k]])	 


census[[k]]<-census[[k]][,-1]
census[[k]][,2]<-gsub(", District of Columbia, District of Columbia", '', 
	census[[k]][,2])
census[[k]][,2]<-gsub('C.+ct','',census[[k]][,2])
census[[k]][,2]<-gsub(" ", '', 	census[[k]][,2])



blerg<-c(which(as.character(census[[k]][1,1:ncol(census[[k]])]) == "44.8"),
which(as.character(census[[k]][1,1:ncol(census[[k]])]) == "38.5"),
which(as.character(census[[k]][1,1:ncol(census[[k]])]) == "2565"))
blerg<-blerg[-4]


census[[k]]<-(census[[k]][,c(1,2,blerg)])	 


names(census[[k]])[2]<-"tract"
names(census[[k]])[3]<-"hours.worked"
names(census[[k]])[4]<-"worker.age"
names(census[[k]])[5]<-"full.time.workers"

census[[k]][80,]
census[[k]][which(census[[k]][,2] == "62.02"),3]<-mean(c(
	as.numeric(census[[k]][which(census[[k]][,2] == "102"),3]) ,
	as.numeric(census[[k]][which(census[[k]][,2] == "110"),3])))


for(j in 3:ncol(census[[k]])){census[[k]][,j]<-as.numeric(census[[k]][,j])}

 



calls<-merge(calls,census[[k]][,-1], "tract")
attach(calls);calls<-(calls[order(ID),] );detach(calls)
head(calls)


head(calls[which(calls$tract== "1"),],1)


#
k=10;def[k,]
head(census[[k]] )	 

names(census[[k]])	 


census[[k]]<-census[[k]][,-1]
census[[k]][,2]<-gsub(", District of Columbia, District of Columbia", '', 
	census[[k]][,2])
census[[k]][,2]<-gsub('C.+ct','',census[[k]][,2])
census[[k]][,2]<-gsub(" ", '', 	census[[k]][,2])



census[[k]]<-census[[k]][,-which((grepl("ale", names(census[[k]]), 
	ignore.case = FALSE)))]

for(j in 3:ncol(census[[k]])){census[[k]][,j]<-as.numeric(census[[k]][,j])}

census[[k]]$new.residents<-as.numeric(
	rowSums(census[[k]][,5:ncol(census[[k]])]))


names(census[[k]])[2]<-"tract"
names(census[[k]])[3]<-"pop.tract"

census[[k]]<-census[[k]][,-(5:(ncol(census[[k]])-1))	 ]

names(census[[k]])[2]<-"tract"
names(census[[k]])[3]<-"pop.tract"
names(census[[k]])[4]<-"old.residents"

census[[k]]$Pmoved<-census[[k]][,5]/census[[k]][,3]

calls<-merge(calls,census[[k]][,-1], "tract")
attach(calls);calls<-(calls[order(ID),] );detach(calls)
head(calls)

calls<-calls[,-24]
write.csv(calls[,],"311v2.csv")

 

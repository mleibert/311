getwd()
setwd("g:/math/311")

calls<-read.csv("311v3b.csv")

head(calls)
nrow(calls)

table(calls$block)

calls$block<-rep("na",nrow(calls))
CALLS<-read.csv("311v2.csv")
head(CALLS)
CALLS<-CALLS[,c(3,8)]
head(CALLS)

CALLs<-read.csv("getblocks.csv")
head(CALLs)
CALLs<-CALLs[,c(ncol(CALLs),3)]
head(CALLs)

calls<-calls[,-2]

CALLS<-rbind(CALLs,CALLS)

calls<-merge(calls,CALLS,"OBJECTID")

write.csv(calls,"311v3b.csv")

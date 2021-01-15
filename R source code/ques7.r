library(readxl)
library(utf8)
library(dplyr)
library(lubridate)
library("zoo")
a<-read_xlsx("filename.xlsx")
c=data.frame(a)
names(c)[1]<-"ID"
names(c)[2]<-"Status"
names(c)[3]<-"Stime"
names(c)[4]<-"Etime"
names(c)[5]<-"Time"
names(c)[6]<-"Diem"
names(c)[7]<-"Q1"
names(c)[8]<-"Q2"
names(c)[9]<-"Q3"
names(c)[10]<-"Q4"
names(c)[11]<-"Q5"
names(c)[12]<-"Q6"
names(c)[13]<-"Q7"
names(c)[14]<-"Q8"
names(c)[15]<-"Q9"
names(c)[16]<-"Q10"
c=subset(c,!is.na(c$ID))
c$ID=as.numeric(c$ID)
c$Etime=as.POSIXct(c$Etime, format = "%d %B %Y %I:%M %p")
c$Stime=as.POSIXct(c$Stime, format = "%d %B %Y %I:%M %p")
ID=as.factor(c$ID)
ID <- ID[!is.na(ID)]
ID=levels(ID)
ID=as.numeric(ID)
solannop=rep(0,times=length(ID))
for(i in 1:length(ID)){
  for(j in 1:length(c$ID)){
    if(c$ID[j]== ID[i]) {solannop[i]=solannop[i]+1}
  }
}
data=data.frame(ID,solannop)
tmax=max(c$Etime,na.rm = T)
tmin=min(c$Etime,na.rm = T)
hours=as.period(interval(tmin, tmax), unit = "hours")
sogiocongthem=hour(hours)-round(hour(hours)/3)
t2=tmin+hours(sogiocongthem)
b=subset(c,Etime>t2)
landaunop=c()
for(i in 1:length(ID)){
  for(j in 1:length(c$ID)){
    if(ID[i]==c$ID[j]){
      landaunop[i]=c$Etime[j]
      break
    }
  }
}
landaunop=as.POSIXct(landaunop,tz = "GMT",origin, format = "%d %B %Y %I:%M %p")
data=cbind(data,landaunop)
data7=subset(data,landaunop>t2)
for(i in 1:length(c$Diem)){
  if(!is.na(c$Diem[i])){
    if(c$Diem[i]=='-'){c$Diem[i]=NA}
  }
}
c$Diem=scan(text=c$Diem, dec=",", sep=".")
diem=c()
for(i in 1:length(data7$ID)){
  k=0
  for(j in 1:length(c$ID)){
    if(data7$ID[i]==c$ID[j]){
      if(!is.na(c$Diem[j])){
        if(k==0){
          k=k+1
          diem[i]=c$Diem[j] 
        }
        else{
          if(c$Diem[j]>diem[i]){diem[i]=c$Diem[j]}
        }
      }
    }
  }
}
data7=cbind(data7,diem)
data7=filter(data7,!is.na(diem))
sosvdoipho=length(data7$ID)
hist(data7$diem,main="PHO DIEM SINH VIEN HOC DOI PHO", xlab="DIEM",ylab = "SO SINH VIEN")
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
sogiocongthem=hour(hours)-round(hour(hours)*2/3)
t1=tmin+hours(sogiocongthem)
landaunop=c()
for(i in 1:length(ID)){
  for(j in 1:length(c$ID)){
    if(ID[i]==c$ID[j]){
      landaunop[i]=c$Etime[j]
      j=length(c$ID)
    }
  }
}
landaunop=as.POSIXct(landaunop,tz = "GMT",origin, format = "%d %B %Y %I:%M %p")
data=cbind(data,landaunop)
diemlandau=c()
for(i in 1:length(data$ID)){
  for(j in 1:length(c$ID)){
    if(data$ID[i]==c$ID[j]){
      diemlandau[i]=c$Diem[j]
      break
    }
  }
}
for(i in 1:length(data$ID)){
  for(j in 1:length(c$ID)){
    if(data$ID[i]==c$ID[j] && diemlandau[i]=="-"){
      diemlandau[i]=c$Diem[j]
      break
    }
  }
}
data=cbind(data,diemlandau)
diemlanhai=c()
for(i in 1:length(data$ID)){
  k=0
  for (j in 1:length(c$ID)){
    if(data$solannop[i]>1){
      if(!is.na(c$Diem[j])){
        if(data$ID[i]==c$ID[j]){k=k+1}
        if(k==2){
          if(c$Diem[j]=='-'){diemlandai[i]=0}
          else{diemlanhai[i]=c$Diem[j]}
          break
        }
      }
    }
    else{diemlanhai[i]=0}
  }
}
data=cbind(data,diemlanhai)
for(i in 1:length(data$diemlandau)){
  if(!is.na(data$diemlandau[i])){
    if(data$diemlandau[i]=='-'){data$diemlandau[i]=0}
  }
}
for(i in 1:length(data$diemlanhai)){
  if(!is.na(data$diemlanhai[i])){
    if(data$diemlanhai[i]=='-'){data$diemlanhai[i]=0}
  }
}
data$diemlandau=scan(text=data$diemlandau, dec=",", sep=".")
data$diemlanhai=scan(text=data$diemlanhai, dec=",", sep=".")
ID10=c()
k=0
for(i in 1:length(data$ID)){
  if((data$landaunop[i]<t1||data$diemlandau[i]>=8||data$diemlanhai[i]>=8)&&data$solannop[i]>1){
    k=k+1
    ID10[k]=data$ID[i]
  }
}
for(i in 1:length(c$Diem)){
  if(!is.na(c$Diem[i])){
    if(c$Diem[i]=='-'){c$Diem[i]=0}
  }
}
c$Diem=scan(text=c$Diem, dec=",", sep=".")
diem=c()
for(i in 1:length(ID10)){
  k=0
  for(j in 1:length(c$ID)){
    if(ID10[i]==c$ID[j]){
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
data10=data.frame(ID10,diem)
sosinhvienchudong=length(data10$ID10)
hist(data10$diem,main="PHO DIEM SINH VIEN CHU DONG", xlab="DIEM",ylab = "SO SINH VIEN")
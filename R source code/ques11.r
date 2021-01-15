library(VennDiagram)
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
tmax=max(c$Etime,na.rm = T)
tmin=min(c$Etime,na.rm = T)
hours=as.period(interval(tmin, tmax), unit = "hours")
sogiocongthem=hour(hours)-round(hour(hours)/3)
t2=tmin+hours(sogiocongthem)
data7=subset(data,landaunop>t2)
names(data7)[1]<-"ID7"
sosvdoipho=length(data7$ID7)
for(i in 1:length(c$Diem)){
  if(!is.na(c$Diem[i])){
    if(c$Diem[i]=='-'){c$Diem[i]=0}
  }
}
c$Diem=scan(text=c$Diem, dec=",", sep=".")
ID9=c()
k=0
for(i in 1:length(data$ID)){
  if(data$diemlandau[i]>=8||data$diemlanhai[i]>=8){
    k=k+1
    ID9[k]=data$ID[i]
  }
}
sosinhvienthongminh=length(ID9)
hours=as.period(interval(tmin, tmax), unit = "hours")
sogiocongthem=hour(hours)-round(hour(hours)*2/3)
t1=tmin+hours(sogiocongthem)
ID10=c()
k=0
for(i in 1:length(data$ID)){
  if((data$landaunop[i]<t1||data$diemlandau[i]>=8||data$diemlanhai[i]>=8)&&data$solannop[i]>1){
    k=k+1
    ID10[k]=data$ID[i]
  }
}
sosinhvienchudong=length(ID10)
g79=c()
k=0
for(i in 1:length(data7$ID7)){
  for(j in 1:length(ID9)){
    if(data7$ID7[i]==ID9[j]){
      k=k+1
      g79[k]=ID9[j]
    }
  }
}
n79=length(g79)
g710=c()
k=0
for(i in 1:length(data7$ID7)){
  for(j in 1:length(ID10)){
    if(data7$ID7[i]==ID10[j]){
      k=k+1
      g710[k]=ID10[j]
    }
  }
}
n710=length(g710)
g910=c()
k=0
for(i in 1:length(ID9)){
  for(j in 1:length(ID10)){
    if(ID9[i]==ID10[j]){
      k=k+1
      g910[k]=ID10[j]
    }
  }
}
n910=length(g910)
g7910=c()
k=0
for(i in 1:length(data7$ID7)){
  for(j in 1:length(g910)){
    if(data7$ID7[i]==g910[j]){
      k=k+1
      g7910[k]=data7$ID7[i]
    }
  }
}
n7910=length(g7910)
grid.newpage()
draw.triple.venn(area1=sosvdoipho,area2=sosinhvienthongminh,area3=sosinhvienchudong,n12=n79,n23=n910,n13=n710,n123=n7910,fill=c("brown","blue","green"),lty="blank",category=c("Doi pho","Thong minh","Chu dong"))

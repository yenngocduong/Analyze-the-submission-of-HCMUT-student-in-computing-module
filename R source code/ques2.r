library("readxl")
library("ggplot2")
library("dplyr")
library(e1071)
c <- read_xlsx("filename.xlsx")
names(c)[1] <- "ID"
names(c)[2] <- "Status"
names(c)[3] <- "Stime"
names(c)[4] <- "Etime"
names(c)[5] <- "Time"
names(c)[6] <- "Diem"
names(c)[7] <- "Q1"
names(c)[8] <- "Q2"
names(c)[9] <- "Q3"
names(c)[10] <- "Q4"
names(c)[11] <- "Q5"
names(c)[12] <- "Q6"
names(c)[13] <- "Q7"
names(c)[14] <- "Q8"
names(c)[15] <- "Q9"
names(c)[16] <- "Q10"
c$Diem=as.numeric(gsub(",", ".", gsub("\\.", "", c$Diem)))
c$Q1=as.numeric(gsub(",", ".", gsub("\\.", "", c$Q1)))
c$Q2=as.numeric(gsub(",", ".", gsub("\\.", "", c$Q2)))
c$Q3=as.numeric(gsub(",", ".", gsub("\\.", "", c$Q3)))
c$Q4=as.numeric(gsub(",", ".", gsub("\\.", "", c$Q4)))
c$Q5=as.numeric(gsub(",", ".", gsub("\\.", "", c$Q5)))
c$Q6=as.numeric(gsub(",", ".", gsub("\\.", "", c$Q6)))
c$Q7=as.numeric(gsub(",", ".", gsub("\\.", "", c$Q7)))
c$Q8=as.numeric(gsub(",", ".", gsub("\\.", "", c$Q8)))
c$Q9=as.numeric(gsub(",", ".", gsub("\\.", "", c$Q9)))
c$Q10=as.numeric(gsub(",", ".", gsub("\\.", "", c$Q10)))
n=nrow(c)
New<-c
New<-New[-c(n),]
sapxep_ID=arrange(New, ID)
sapxep_ID=subset(sapxep_ID,!is.na(sapxep_ID[["Diem"]]))
ID<-c(sapxep_ID[["ID"]][1])
NoS<-c(1)
Mark<-c(sapxep_ID[["Diem"]][1])
f=1
f0=1
for(i in 2:(nrow(sapxep_ID))){
  if(sapxep_ID[["ID"]][i]!=sapxep_ID[["ID"]][i-1]){
    ID<-c(ID,sapxep_ID[["ID"]][i])
    NoS<-c(NoS,f)
    Mark<-c(Mark,0)
    f=1
    f0=f0+1
  }
  else {
    f=f+1
  }
  if(Mark[f0]<=sapxep_ID[["Diem"]][i]){
    Mark[f0]<-sapxep_ID[["Diem"]][i]
  }
}
Submissions<-data.frame(ID,NoS,Mark)

for(i in 1:(n-1)){
  if(is.na(New[["Diem"]][i])){
    New[["Diem"]][i]=0
  }
}
sapxep_Diem=arrange(New,Diem)

Marklist<-c()
for(i in 1:(n-1)){
  if(!is.na(c[["Diem"]][i])){
    Marklist<-c(Marklist,c[["Diem"]][i])
  }
}

#2a
print(New[["Diem"]])

#2b
print(min(Marklist))

#2c
df_2c=subset(c,Diem==min(Marklist))
print(df_2c[["ID"]])

#2d
n2c=sum(!is.na(df_2c[["ID"]]))
n2c
df_2d<-data.frame()
for (i in 1:n2c){
  df_2d<-rbind(df_2d,c(filter(Submissions,ID==df_2c[["ID"]][i])))
}
ggplot(df_2d,aes(NoS)) +
  geom_histogram(binwidth = 0.5, fill = "blue", col = "black") +
  xlab("So lan nop bai") +
  ylab("so luong sinh vien")  +
  ggtitle("Pho theo so lan nop bai cua cac sinh vien co it nhat mot lan nop bai co so diem it nhat")

#2e
print(min(Submissions[["Mark"]]))

#2f
df_2f=subset(Submissions,Mark==min(Submissions[["Mark"]]))
print(df_2f[["ID"]])

#2g
ggplot(df_2f,aes(NoS)) +
  geom_histogram(binwidth = 0.5, fill = "blue", col = "black") +
  xlab("so lan nop bai") +
  ylab("so luong sinh vien")  +
  ggtitle("Pho theo so lan nop bai cua cac sinh vien co diem so tong ket thap nhat")

#2h
print(max(Marklist))

#2i
df_2i=subset(Submissions,Mark==max(Marklist))
print(df_2i[["ID"]])

#2j
ggplot(df_2i,aes(NoS)) +
  geom_histogram(binwidth = 0.5, fill = "blue", col = "black") +
  xlab("so lan nop bai") +
  ylab("So luong sinh vien")  +
  ggtitle("Pho theo so lan nop bai cua cac sinh vien co it nhat mot bai co so diem cao nhat")

#2k
print(max(Submissions[["Mark"]]))

#2l
df_2l=subset(Submissions,Mark==max(Submissions[["Mark"]]))
print(df_2l[["ID"]])

#2m
ggplot(df_2l,aes(NoS)) +
  geom_histogram(binwidth = 0.5, fill = "blue", col = "black") +
  xlab("so lan nop bai") +
  ylab("so luong sinh vien")  +
  ggtitle("Pho theo so lan nop bai cua cac sinh vien co diem so tong ket cao nhat")

#2n
print(round(mean(New[["Diem"]]), digits = 1))

#2o
f=0 
for(i in 1:(n-1)){
  if(New[["Diem"]][i]==mean(New[["Diem"]])){
    f=f+1
  }
}
print(f)
#2p
print(median(New[["Diem"]]))
print(max(New[["Diem"]]))
print(min(New[["Diem"]]))

#2q
print(var(New[["Diem"]]))
print(sd(New[["Diem"]]))

#2r
print(skewness(New[["Diem"]]))
print(kurtosis(New[["Diem"]]))

#2s
print(quantile(New[["Diem"]])[2])
print(quantile(New[["Diem"]])[4])

#2t
arrangedMark<-c(sapxep_Diem[["Diem"]][1])
for(i in 2:(n-1)){
  if(sapxep_Diem[["Diem"]][i]!=sapxep_Diem[["Diem"]][i-1]){
    arrangedMark<-c(arrangedMark,sapxep_Diem[["Diem"]][i])
  }
}
nMark=sum(!is.na(arrangedMark))
print(nrow(filter(Submissions,(Mark==arrangedMark[nMark])|(Mark==arrangedMark[nMark-1]))))

#2u
ggplot(filter(Submissions,(Mark==arrangedMark[nMark])|(Mark==arrangedMark[nMark-1])),aes(NoS)) +
  geom_histogram(binwidth = 0.5, fill = "blue", col = "black") +
  xlab("so lan nop bai") +
  ylab("so luong sinh vien")  +
  ggtitle("Pho theo so lan nop bai cua cac sinh vien co diem so tong ket o hai muc diem cao nhat")

#2v
k=3
print(nrow(filter(Submissions,Mark==arrangedMark[nMark-k+1])))
#2w

Submissions %>%
  filter(Mark==arrangedMark[nMark-k+1]) %>%
  ggplot(aes(NoS)) +
  geom_histogram(binwidth = 0.5, fill = "blue", col = "black") +
  xlab("so lan nop bai") +
  ylab("so luong sinh vien")  +
  ggtitle("Pho theo so lan nop bai cua cac sinh vien co diem so tong ket o muc diem cao thu k voi k cho truoc")
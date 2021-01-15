library(utf8)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(gridExtra)

c<-read_xlsx("filename.xlsx")
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
c=subset(c,!is.na(c$ID))
c$ID=as.numeric(c$ID)


c$Diem = as.numeric(gsub(",",".", gsub("\\.","", c$Diem)))
c$Q1 = as.numeric(gsub(",",".", gsub("\\.","", c$Q1)))
c$Q2 = as.numeric(gsub(",",".", gsub("\\.","", c$Q2)))
c$Q3 = as.numeric(gsub(",",".", gsub("\\.","", c$Q3)))
c$Q4 = as.numeric(gsub(",",".", gsub("\\.","", c$Q4)))
c$Q5 = as.numeric(gsub(",",".", gsub("\\.","", c$Q5)))
c$Q6 = as.numeric(gsub(",",".", gsub("\\.","", c$Q6)))
c$Q7 = as.numeric(gsub(",",".", gsub("\\.","", c$Q7)))
c$Q8 = as.numeric(gsub(",",".", gsub("\\.","", c$Q8)))
c$Q9 = as.numeric(gsub(",",".", gsub("\\.","", c$Q9)))
c$Q10 = as.numeric(gsub(",",".", gsub("\\.","", c$Q10)))

sapxep_ID=arrange(c,ID) #t???o m???t dataframe s???p x???p d??? li???u theo th??? t??? tãng d???n
n=nrow(sapxep_ID) #S??? bài làm
IDlist=c(c[["ID"]][1])
for(i in 1:(n-2)){
  if(sapxep_ID[["ID"]][i]!=sapxep_ID[["ID"]][i+1]){
    IDlist <- c(IDlist,sapxep_ID[["ID"]][i+1])
  }
}
n_sv=length(IDlist)#s??? sinh viên n???p bài
n_sv

#bài 3
numOfSubs=c()
dem = 1
f=1
for(i in 1:(n-2)){
  if(sapxep_ID[["ID"]][i]!=sapxep_ID[["ID"]][i+1]){
    numOfSubs <- c(numOfSubs,f)
    f=1
    if(i==n-2){
      numOfSubs <- c(numOfSubs,f)
    }
  }
  else{
    f=f+1
    if(i==n-2){
      numOfSubs <- c(numOfSubs,f)
    }
  }
}
New_list <-data.frame(IDlist, numOfSubs)
#New_list = arrange(New_list,numOfSubs)


#bai 5

k=6
LanNop = matrix('NULL', nrow = n_sv, ncol = k)#T???o m???t ma tr???n lýu ði???m s??? m???i l???n n???p bài
i=0
for(l in 1: n_sv){
  
  for(j in 1:numOfSubs[l] ){
    i=i+1
    if(is.na(sapxep_ID[["Diem"]][i])== TRUE){
      LanNop[l,j] = 0
    }
    else {
      LanNop[l,j] <- sapxep_ID[["Diem"]][i]
    }
  }
  for(j in (numOfSubs[l]+1):k){
    if(is.na(sapxep_ID[["Diem"]][i])== TRUE){
      LanNop[l,j] = 0
    }
    LanNop[l,j] = 0
    
  }
}

colnames(LanNop) <- c("L1","L2","L3","L4","L5","L6")


LanNop=data.frame(LanNop)
LanNop$L1 = as.numeric(LanNop$L1)
LanNop$L2 = as.numeric(LanNop$L2)
LanNop$L3 = as.numeric(LanNop$L3)
LanNop$L4 = as.numeric(LanNop$L4)
LanNop$L5 = as.numeric(LanNop$L5)
LanNop$L6 = as.numeric(LanNop$L6)

DiemCuoiCung =c()
for(i in 1:n_sv){
  DiemCuoiCung <- c(DiemCuoiCung,max(LanNop[i,]))
}

DiemCuoiCung
LanNop=cbind(LanNop,DiemCuoiCung)


#bài 5b



k=3
LanNop2=data.frame(LanNop$L1,LanNop$L2,LanNop$L3)
DiemCuoiCung2 =c()
for(i in 1:n_sv){
  DiemCuoiCung2 <- c(DiemCuoiCung2,max(LanNop2[i,]))
}
LanNop2=data.frame(LanNop2,DiemCuoiCung2)


#5c
tb1= mean(LanNop$L1)
tb2= mean(LanNop$L2)
tb3= mean(LanNop$L3)
tb4= mean(LanNop$L4)
tb5= mean(LanNop$L5)
tb6= mean(LanNop$L6)

tb=c(tb1,tb2,tb3,tb4,tb5,tb6)
L=c(1,2,3,4,5,6)
tbk=data.frame(L,tb)

barplot(tbk$tb,width = 1,space = 2,main = "Diem trung binh qua moi lan nop bai",names.arg = tbk$L,
        xlab = "k",ylab = "Diem",col = "blue")

#5d
TrungBinhDiem=mean(LanNop$DiemCuoiCung)
TrungBinhDiem

#bi???u ð??? phân b??? ði???m

p = ggplot(LanNop,aes(x=DiemCuoiCung))+geom_density(alpha =0.5,col="red")+labs(title = "Ði???m t???ng h???p sau k=6 l???n n???p bài",x="Ði???m t???ng h???p",y="T??? tr???ng")
p #5a

p = ggplot(LanNop2,aes(x=DiemCuoiCung2))+geom_density(alpha =0.5,col="red")+labs(title = "Ði???m t???ng h???p sau k=3 l???n n???p bài",x="Ði???m t???ng h???p",y="T??? tr???ng")
p #5b



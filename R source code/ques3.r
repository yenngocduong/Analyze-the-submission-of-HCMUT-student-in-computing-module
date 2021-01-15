library(xlsx)
library(readxl)
library(dplyr)
library(lubridate)
library(utf8)
library(ggplot2)
library(e1071)
c <- read_xlsx("filename.xlsx");
c <- rename(c, c("ID", "Status", "Stime", "Etime", "Time", "Diem", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"));
names(c)[1] <- "ID";
names(c)[2] <- "Status";
names(c)[3] <- "Stime";
names(c)[4] <- "Etime";
names(c)[5] <- "Time";
names(c)[6] <- "Diem";
names(c)[7] <- "Q1";
names(c)[8] <- "Q2";
names(c)[9] <- "Q3";
names(c)[10] <- "Q4";
names(c)[11] <- "Q5";
names(c)[12] <- "Q6";
names(c)[13] <- "Q7";
names(c)[14] <- "Q8";
names(c)[15] <- "Q9";
names(c)[16] <- "Q10";
c$Diem=as.numeric(gsub(",",".", gsub("\\.","", c$Diem)));
c$Q1=as.numeric(gsub(",",".", gsub("\\.","", c$Q1)));
c$Q2=as.numeric(gsub(",",".", gsub("\\.","", c$Q2)));
c$Q3=as.numeric(gsub(",",".", gsub("\\.","", c$Q3)));
c$Q4=as.numeric(gsub(",",".", gsub("\\.","", c$Q4)));
c$Q5=as.numeric(gsub(",",".", gsub("\\.","", c$Q5)));
c$Q6=as.numeric(gsub(",",".", gsub("\\.","", c$Q6)));
c$Q7=as.numeric(gsub(",",".", gsub("\\.","", c$Q7)));
c$Q8=as.numeric(gsub(",",".", gsub("\\.","", c$Q8)));
c$Q9=as.numeric(gsub(",",".", gsub("\\.","", c$Q9)));
c$Q10=as.numeric(gsub(",",".", gsub("\\.","", c$Q10)));

#==================================================================

mydata = subset(c, c$ID != "NA")
mydata$Diem[is.na(mydata$Diem)] <- 0
sapxep_ID=arrange(mydata,ID)
n=nrow(mydata)
IDlist=c(mydata[["ID"]][1])
for(i in 1:(n-1)){
  if(sapxep_ID[["ID"]][i]!=sapxep_ID[["ID"]][i+1])
    IDlist<-c(IDlist, sapxep_ID[["ID"]][i+1])
}
#==============================================================

numOfSubs=c() #Vecto dem so lan nop bai
f=1 #Bien dem so lan nop bai
for(i in 1:(n-1)){
  if(sapxep_ID[["ID"]][i]!=sapxep_ID[["ID"]][i+1]){
    numOfSubs <- c(numOfSubs, f)
    f=1
    if(i==n-1){
      numOfSubs<-c(numOfSubs, f)
    }
  }
  else{
    f = f+1
    if(i==n-1){
      numOfSubs<-c(numOfSubs, f)
    }
  }
}
#Tao 1 data frame chua ID va so lan nop bai
New_list <- data.frame(IDlist, numOfSubs)
#Sap xep theo thu tu so lan nop bai tang dan
New_list = arrange(New_list, numOfSubs)
#So lan nop bai it nhat la so lan nop bai o dong dau tien
numOfSubs_min = New_list[["numOfSubs"]][1]
numOfSubs_min


#======================================================================

df_3b = subset(New_list, New_list$numOfSubs == numOfSubs_min)
df_3b[["IDlist"]]


#==================================================================



New_list1 = c()
New_list1 = subset(New_list, numOfSubs == numOfSubs_min)
min_spec = data.frame()
for (i in 1:nrow(New_list1))
{
  for (j in 1:nrow(mydata))
  {
    if (mydata[[1]][j] == New_list1[[1]][i])
    {
      add = c(mydata[["Diem"]][j])
      min_spec = rbind(min_spec, add)
    }
  }
}
colnames(min_spec) = c("Diem")

ggplot(min_spec, aes(Diem))+geom_histogram(binwidth = 0.5, fill ="blue", col = "black")+ ggtitle("Pho diem thi sinh nop it nhat")

#==========================================================

numOfSubs_max = New_list[["numOfSubs"]][nrow(New_list)]
numOfSubs_max

#=======================================================

df_3e = subset(New_list, New_list$numOfSubs == numOfSubs_max)
df_3e[["IDlist"]]

#=====================================================


New_list4 = c()
New_list4 = subset(New_list, numOfSubs == numOfSubs_max)
min_spec2 = data.frame()
for (i in 1:nrow(New_list4))
{
  for (j in 1:nrow(mydata))
  {
    if (mydata[[1]][j] == New_list4[[1]][i])
    {
      add2 = c(mydata[["Diem"]][j])
      min_spec2 = rbind(min_spec2, add2)
    }
  }
}
colnames(min_spec2) = c("Diem")
ggplot(min_spec2, aes(Diem))+geom_histogram(binwidth = 0.5, fill ="blue", col = "black")+ ggtitle("Pho diem thi sinh nop nhieu nhat")


#=======================================================



averagesubs = mean(New_list$numOfSubs)
averagesubs

#====================================================

average_stus = c()
for (i in 1:numofStus){
  if (New_list[["numOfSubs"]][i] == averagesubs){
    average_stus = c(average_stus, New_list[["IDlist"]][i])
  }
}
average_num = length(average_stus)
average_num

#==========================================================

New_list_average = c()
New_list_average = subset(New_list, numOfSubs == averagesubs)
while(nrow(New_list_average)!=0){
  min_spec3 = data.frame()
  for (i in 1:nrow(New_list_average))
  {
    for (j in 1:nrow(mydata))
    {
      if (mydata[[1]][j] == New_list_average[[1]][i])
      {
        add3 = c(mydata[["Diem"]][j])
        min_spec3 = rbind(min_spec3, add3)
      }
    }
  }
  colnames(min_spec3) = c("Diem")
  ggplot(min_spec3, aes(Diem))+geom_histogram(binwidth = 0.5, fill ="blue", col = "black")+ ggtitle("Pho diem thi sinh co so lan nop trung binh")

}

#===========================================================

sd(New_list$numOfSubs)

#==============================================================

skewness(New_list$numOfSubs)
kurtosis(New_list$numOfSubs)

#===========================================================

quantile(New_list$numOfSubs)

#=========================================================

#Lap danh sach so cac lan nop bai
substime = c(New_list[["numOfSubs"]][1])
for(i in 1:numofStus){
  if(New_list[["numOfSubs"]][i]!=New_list[["numOfSubs"]][i+1]){
    substime <- c(substime, New_list[["numOfSubs"]][i+1])
  }
}
#Tim so lan nop bai nhieu thu nhi
second = substime[length(substime)-1]
#Lap danh sach sinh vien nop nhieu nhi
second_stus = c()
second_stus = subset(New_list, New_list$numOfSubs == second)
View(second_stus)

#=========================================================

New_list_second = c()
New_list_second = subset(New_list, numOfSubs == second)
min_spec4 = data.frame()
for (i in 1:nrow(New_list_second))
{
  for (j in 1:nrow(mydata))
  {
    if (mydata[[1]][j] == New_list_second[[1]][i])
    {
      add4 = c(mydata[["Diem"]][j])
      min_spec4 = rbind(min_spec4, add4)
    }
  }
}
colnames(min_spec4) = c("Diem")
ggplot(min_spec4, aes(Diem))+geom_histogram(binwidth = 0.5, fill ="blue", col = "black")+ ggtitle("Pho diem thi sinh nop nhieu nhi")

#===============================================================

first = substime[length(substime)]
first_stus = c()
first_stus = subset(New_list, New_list$numOfSubs == first)
firstandsecond_stus = c()
firstandsecond_stus = rbind(second_stus, first_stus)
View(firstandsecond_stus)


#==============================================================




numof_firstandsecond = nrow(firstandsecond_stus)
numof_firstandsecond


#===================================================================


New_list_secfi = c()
New_list_secfi = subset(New_list, numOfSubs == second | numOfSubs == first)
min_spec5 = data.frame()
for (i in 1:nrow(New_list_secfi))
{
  for (j in 1:nrow(mydata))
  {
    if (mydata[[1]][j] == New_list_secfi[[1]][i])
    {
      add5 = c(mydata[["Diem"]][j])
      min_spec5 = rbind(min_spec5, add5)
    }
  }
}
colnames(min_spec5) = c("Diem")
ggplot(min_spec5, aes(Diem))+geom_histogram(binwidth = 0.5, fill ="blue", col = "black")+ ggtitle("Pho diem thi sinh nop nhieu nhat va nhieu nhi")


#================================================================



New_list_dec = c()
New_list_dec = arrange(New_list, desc(numOfSubs))
onethree_stus = c(New_list_dec[["IDlist"]][1])
tam = 2
while(tam <= numofStus/3){
  onethree_stus <- c(onethree_stus, New_list_dec[["IDlist"]][tam])
  tam = tam+1
}
View(onethree_stus)


#==================================================================


onethree_count = length(onethree_stus)
onethree_count

#===========================================================


#New_list6 = data.frame(onethree_stus)
min_spec6 = data.frame()
for (i in 1:length(onethree_stus))
{
  for (j in 1:nrow(mydata))
  {
    if (mydata[[1]][j] == onethree_stus[i])
    {
      add6 = c(mydata[["Diem"]][j])
      min_spec6 = rbind(min_spec6, add6)
    }
  }
}
colnames(min_spec6) = c("Diem")
ggplot(min_spec6, aes(Diem))+geom_histogram(binwidth = 0.5, fill ="blue", col = "black")+ ggtitle("Pho diem thi sinh nam trong nhom mot phan ba dau theo thu tu so lan nop bai giam dan")


#============================================================


k = scan()
substime = sort(substime, decreasing = TRUE)
New_list_k = c()
for (i in 1:k){
  for (j in 1:nrow(New_list))
  {
    if (New_list[[2]][j]==substime[i])
    {
      add_k = c(New_list[["IDlist"]][j])
      New_list_k = c(New_list_k, add_k)
    }
  }
}

min_spec7 = data.frame()
for (i in 1:length(New_list_k))
{
  for (j in 1:nrow(mydata))
  {
    if (mydata[[1]][j] == New_list_k[i])
    {
      add7 = c(mydata[["Diem"]][j])
      min_spec7 = rbind(min_spec7, add7)
    }
  }
}



colnames(min_spec7) = c("Diem")
ggplot(min_spec7, aes(Diem))+geom_histogram(binwidth = 0.5, fill ="blue", col = "black")+ ggtitle("Pho diem thi sinh co so lan nop thuoc k nhom dau")























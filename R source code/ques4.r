library(readxl)
library(dplyr)
library(lubridate)
library(utf8)
library(e1071)
library(ggplot2)
c <- read_xlsx("filename.xlsx")
colnames(c) <- c("ID", "Status", "Stime", "Etime", "Time", "Diem", "Q1", "Q2",
                  "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")
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
print(c)
sapxep_ID=arrange(c, ID) 
n=nrow(c) 
IDlist=c()
IDlist <- c(IDlist, sapxep_ID[["ID"]][1])
for( i in 1:(n-2)){
  if(sapxep_ID[["ID"]][i]!=sapxep_ID[["ID"]][i+1])
    IDlist <- c(IDlist, sapxep_ID[["ID"]][i+1])
}
print(IDlist)
n_sv=length(IDlist) 
n_sv 


numOfSubs=c()
dem=1
f=1 
time1=c() 
time2=c()
time1<-c(time1, sapxep_ID[["Etime"]][1])
for( i in 1:(n-2)){
  if(sapxep_ID[["ID"]][i]!=sapxep_ID[["ID"]][i+1]){
    numOfSubs <- c(numOfSubs, f)
    f=1
    if(i==n-2)  {
      numOfSubs <- c(numOfSubs, f)
    }
    time1 <- c(time1, sapxep_ID[["Etime"]][i+1])
    time2 <- c(time2, sapxep_ID[["Etime"]][i])
    if(i==n-2)  {
      time2 <- c(time2, sapxep_ID[["Etime"]][i+1])
    }
  }
  else{
    f=f+1
    if(i==n-2)  {
      numOfSubs <- c(numOfSubs, f)
      time2 <- c(time2, sapxep_ID[["Etime"]][i+1])
    }
  }
}

time1 <- dmy_hm(time1) 
time2 <- dmy_hm(time2)
T=c()
T=difftime(time2, time1, units="secs")
New_list_4<-data.frame(IDlist, numOfSubs, time1, time2, T)
View(New_list_4)

# 4b
df_4b=arrange(New_list_4, T)
df_4b$T[is.na(df_4b$T)] <- 0
T_4b=c()
t_4b=1 
numT_4b=c()
T_4b <- c(T_4b, df_4b$T[1])
for(i in 1:(nrow(New_list_4)-1)){
  if(df_4b[["T"]][i]!=df_4b[["T"]][i+1]){
    T_4b <- c(T_4b, df_4b[["T"]][i+1])
    numT_4b <- c(numT_4b, t_4b)
    t_4b=1
    if(i==(nrow(New_list_4)-1))  {
      numT_4b <- c(numT_4b, t_4b)
    }
  }
  else{
    t_4b=t_4b+1
    if(i==(nrow(New_list_4)-1))  {
      numT_4b <- c(numT_4b, t_4b)
    }
  }
}
barplot(numT_4b, T_4b, width=1, space=0.2,
        xlab="Thoi gian tu lan nop dau den lan nop cuoi", 
        ylab="So luong sinh vien",
        main="Pho thoi gian lam viec")
# 4c
fre=c() # Vector tan suat
fre=as.numeric(T)/numOfSubs
New_list_4=cbind(New_list_4, fre)
View(New_list_4)
# 4d
New_list_4=arrange(New_list_4, fre)
df_4d=subset(New_list_4, fre==New_list_4[["fre"]][1])
df_4d[["IDlist"]]
# 4e
diem_4e=c("00.0", "00.5", "01.0", "01.5", "02.0", "02.5", "03.0", "03.5", "04.0",
          "04.5","05.0", "05.5", "06.0", "06.5", "07.0", "07.5", "08.0", "08.5",
          "09.0", "09.5", "10.0", "NA")
New_list_4=arrange(New_list_4, fre)
df_4e=subset(New_list_4, fre==New_list_4[["fre"]][1])
df_4e1=subset(c, as.numeric(ID) %in% as.numeric(df_4e[["IDlist"]]))
f_4e=c()
for(i in 0:20){
  df_4e2=subset(df_4e1, Diem==0.5*i)
  f_4e <- c(f_4e, nrow(df_4e2))
}
df_4e2=subset(df_4e1, is.na(Diem))
f_4e <- c(f_4e, nrow(df_4e2))
df_4e3=data.frame(diem_4e, f_4e)
df_4e3 %>%
  ggplot(aes(y=as.numeric(f_4e), x=diem_4e, group=1))+
  geom_line(color="steelblue")+geom_point()+labs(x="Diem so",
  y="So luong", title="Pho diem cua cac sinh vien co tan suat nop bai it nhat")
# 4f
New_list_4=arrange(New_list_4, desc(fre))
df_4f=subset(New_list_4, fre==New_list_4[["fre"]][1])
nrow(df_4f)
# 4g
df_4f[["IDlist"]]

# 4h

diem_4h=c("00.0", "00.5", "01.0", "01.5", "02.0", "02.5", "03.0", "03.5", "04.0",
          "04.5","05.0", "05.5", "06.0", "06.5", "07.0", "07.5", "08.0", "08.5",
          "09.0", "09.5", "10.0", "NA")
New_list_4=arrange(New_list_4, desc(fre))
df_4h=subset(New_list_4, fre==New_list_4[["fre"]][1])
df_4h1=subset(c, as.numeric(ID) %in% as.numeric(df_4h[["IDlist"]]))
f_4h=c()
for(i in 0:20){
  df_4h2=subset(df_4h1, Diem==0.5*i)
  f_4h <- c(f_4h, nrow(df_4h2))
}
df_4h2=subset(df_4h1, is.na(Diem))
f_4h <- c(f_4h, nrow(df_4h2))
df_4h3=data.frame(diem_4h, f_4h)
df_4h3 %>%
  ggplot(aes(y=as.numeric(f_4h), x=diem_4h, group=1))+
  geom_line(color="steelblue")+geom_point()+labs(x="Diem so",
  y="So luong", title="Pho diem cua cac sinh vien co tan suat nop bai nhieu nhat")

# 4i
df_4i=subset(New_list_4, fre==New_list_4[["fre"]][nrow(df_4f)+1])
df_4i[["IDlist"]]
# 4j
c_4j1=df_4f[["IDlist"]]
c_4j2=df_4i[["IDlist"]]
c_4j=c(c_4j1, c_4j2)
c_4j
# 4k
sumT=0
t3 <- dmy_hm(sapxep_ID$Etime)
for( i in 1:(n-2)){
  if(sapxep_ID[["ID"]][i]==sapxep_ID[["ID"]][i+1]){
    if(is.na(t3[i+1])!=TRUE && is.na(t3[i])!=TRUE) sumT=abs(as.numeric(difftime(t3[i+1], t3[i], units="secs")))+sumT
  }
}
mT=as.numeric(sumT)/(n-1+n_sv)
mT
# 4l
diem_4l=c("00.0", "00.5", "01.0", "01.5", "02.0", "02.5", "03.0", "03.5", "04.0",
          "04.5","05.0", "05.5", "06.0", "06.5", "07.0", "07.5", "08.0", "08.5",
          "09.0", "09.5", "10.0", "NA")
f_4l=c()

sapxep_ID<-sapxep_ID[1:(n-1), ]
for(i in 0:20){
    df_4l=subset(sapxep_ID, Diem==0.5*i)
    f_4l <- c(f_4l, nrow(df_4l))
}
df_4l=subset(sapxep_ID, is.na(Diem))
f_4l <- c(f_4l, nrow(df_4l))
View(f_4l)
df_4lmno=data.frame(diem_4l, f_4l)
F_4l=c() # Vector tần suất
F_4l=as.numeric(df_4lmno$f_4l)/(n-1)
df_4lmno=cbind(df_4lmno, F_4l)
cfre_4l=cumsum(df_4lmno$F_4l)
df_4lmno=cbind(df_4lmno, cfre_4l)
View(df_4lmno)

# 4m

barplot( df_4lmno$f_4l, width = 1, space=2, main="Tan so",
         names.arg=df_4lmno$diem_4l, xlab="Diem so", ylab="Tan so", col="blue")

#4n

barplot( df_4lmno$F_4l, width = 1, space=2, main="Tan suat",
         names.arg=df_4lmno$diem_4l, xlab="Diem so", ylab="Tan suat", col="blue")

# 4o

df_4o=data.frame(df_4lmno$diem_4l, df_4lmno$cfre_4l)
df_4o=arrange(df_4o, cfre_4l)
df_4o %>%
ggplot(aes(y=as.numeric(df_4lmno.cfre_4l), x=df_4lmno.diem_4l, group=1))+
  geom_line(color="steelblue")+geom_point()+labs(x="Diem so",
                                y="Tan suat tich luy",
                                title="Bieu do tan suat tich luy")
    
# 4p

summary(New_list_4$fre)

#4q

New_list_4$fre[is.na(New_list_4$fre)] <- 0
var(New_list_4$fre) 
sd(New_list_4$fre) 
 
# 4r
skewness(New_list_4$fre)
kurtosis(New_list_4$fre)


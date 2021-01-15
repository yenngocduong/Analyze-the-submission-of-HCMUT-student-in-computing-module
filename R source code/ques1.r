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




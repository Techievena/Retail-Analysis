setwd("~/R/Working Directory/Retail-Analysis")
library(data.table)
library(plyr)
library(xts)
library(ggplot2)
inputtable<-read.csv("Retail Analytics.csv")

date_unit<-ddply(inputtable,~Date,summarise,Sum_Unit=sum(Units))
dt<-as.data.frame(table(inputtable$Date))
date_unit$Frequency<-dt$Freq
date_unit$Date<-as.Date(date_unit$Date)
s_u<-c()
f_u<-c()
d_u<-c()
j=1
k=0
s.date<-date_unit$Date[1]
e.date<-date_unit$Date[length(date_unit$Date)]
for(i in s.date:e.date){
  if(i==date_unit$Date[j]){
    s_u<-append(s_u,date_unit$Sum_Unit[j])
    f_u<-append(f_u,date_unit$Frequency[j])
    j<-j+1
  }
  else{
    s_u<-append(s_u,0.0)
    f_u<-append(f_u,0)
  }
  d_u<-append(d_u,s.date+k)
  k<-k+1
}
date_unit<-data.frame(Date=d_u, Sum_Unit=s_u, Frequency=f_u)
time_series_unit<-xts(date_unit$Sum_Unit,date_unit$Date)
time_series_frequency<-xts(date_unit$Frequency,date_unit$Date)
rm(dt,d_u,f_u,s_u,e.date,s.date,i,j,k)

customer_group<-ddply(inputtable,~Customer.code)
write.csv(customer_group,"Customer Grouping.csv", row.names=F)
customer_unit<-ddply(customer_group,~Customer.code,summarise,Sum_Unit=sum(Units))
cust<-as.data.frame(table(customer_group$Customer.code))
customer_unit$Frequency<-cust$Freq
rm(cust)
write.csv(customer_unit,"Units per Customer.csv", row.names=F)

item_group<-ddply(customer_group,~Item.code)
write.csv(item_group,"Item Grouping.csv", row.names=F)
item_unit<-ddply(item_group,~Item.code,summarise,Sum_Unit=sum(Units))
itm<-as.data.frame(table(item_group$Item.code))
item_unit$Frequency<-itm$Freq
rm(itm)
write.csv(item_unit,"Units per Item.csv", row.names=F)

rm<-c()
item_customer_unit_date<-data.table(item_group)
item_customer_unit_date<-item_customer_unit_date[,Sum_Unit:=sum(Units), by=list(Customer.code,Item.code)]
item_customer_unit<-item_customer_unit_date[,c(-1,-4)]
for(i in 1:(length(item_customer_unit$Customer.code)-1)){
if(item_customer_unit$Customer.code[i]==item_customer_unit$Customer.code[i+1] 
	&& item_customer_unit$Item.code[i]==item_customer_unit$Item.code[i+1]){
      rm<-append(rm,i+1)
}}
for(i in 1:length(rm)){
  item_customer_unit[(rm[i]),]=NA
}
item_customer_unit <- item_customer_unit[rowSums(is.na(item_customer_unit))<ncol(item_customer_unit),]
rm(rm)

x <- as.POSIXct(inputtable$Date)
mo <- strftime(x, "%m")
yr <- strftime(x, "%Y")
unit<-inputtable$Units
dd <- data.frame(mo, yr, unit)
dd$count<-1
sale_per_month<-aggregate(unit ~ mo + yr, dd, FUN = sum)
sale_per_month1<-aggregate(count ~ mo + yr, dd, FUN = sum)
sale_per_month$count<-sale_per_month1$count
write.csv(sale_per_month,"Sales per month.csv", row.names=F)

trainingdata<-inputtable[1:433,]
write.csv(trainingdata,"Tranining dataset.csv", row.names=F)

testingdata<-inputtable[434:659,]
write.csv(testingdata,"Testing dataset.csv", row.names=F)

setwd("~/R/Working Directory/Retail Analysis/Item Dataset")
items<-levels(item_unit$Item.code)
for(i in 1:length(items)){
 x=items[i]
 y<-assign(x,inputtable[(as.character(inputtable$Item.code)==x),])
 write.csv(y, paste0(x,".csv"), row.names=F)
}

setwd("~/R/Working Directory/Retail Analysis/Customer Dataset")
customers<-levels(customer_unit$Customer.code)
for(i in 1:length(customers)){
  x=customers[i]
  y<-assign(x,inputtable[(as.character(inputtable$Customer.code)==x),])
  write.csv(y, paste0(x,".csv"), row.names=F)
}

ggplot(NULL, aes(Date, Customer.code))+geom_point(data=I1,color="red")+geom_point(data=I2,color="green")+geom_point(data=I3,color="blue")+geom_point(data=I4,color="yellow")+geom_point(data=I5,color="brown")+geom_point(data=I6,color="orange")+geom_point(data=I7,color="violet")+geom_point(data=I8,color="black")
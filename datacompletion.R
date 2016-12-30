setwd("~/R/Working Directory/Retail-Analysis")
library(xlsx)

retailanalytics <- read.xlsx("Retail Analytics.xlsx", sheetIndex = 1)
retailanalytics<-retailanalytics[!is.na(retailanalytics$Item.code),]
test<-retailanalytics
retailanalytics$Date<-as.Date(retailanalytics$Date, "%d/%m/%Y")
dates<-as.vector(test$Date)
dates<-as.numeric(dates)
dates<-as.Date(dates, origin = "1899-12-30")
for(i in 1:length(retailanalytics$Date)){
  if(is.na(retailanalytics$Date[i])){
    retailanalytics$Date[i]=dates[i]
  }
}
date<-NULL
customer<-NULL
for (i in 1:length(retailanalytics$Customer.code)) {
	data<-retailanalytics$Customer.code[i]
	if(is.na(data)){
		retailanalytics$Date[i]<-date
		retailanalytics$Customer.code[i]<-customer
	}
	date<-retailanalytics$Date[i]
	customer<-retailanalytics$Customer.code[i]
}

write.csv(retailanalytics,"Retail Analytics.csv", row.names=F)

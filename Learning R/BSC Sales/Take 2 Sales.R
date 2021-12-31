#load packages
library(tidyverse)
library(lubridate)
#filter non-sales
take.two<-filter(bsc.df,Agreement!="Ambassador Program Invoiced")
take.two<-filter(take.two,Agreement!="Corporate Passport Memberships Participation")
take.two<-filter(take.two,Agreement!="Elite Conversions 45 Day Cxl")
take.two<-filter(take.two,Agreement!="Elite Commit $99.99 Annual $69.99 Upgrade")
take.two<-filter(take.two,Agreement!="Elite MTM $109.99 Annual $69.99 Upgrade")
take.two<-filter(take.two,Agreement!="Employee Agreement")
take.two<-filter(take.two,Agreement!="First-Class Agreement")
take.two<-filter(take.two,Agreement!="Guest Agreement")
take.two<-filter(take.two,Agreement!="Guest Agreement Online Only")
take.two<-filter(take.two,Agreement!="Locker Rental")
take.two<-filter(take.two,Agreement!="Passport Commit $89.99 Annual $69.99 Upgrade")
take.two<-filter(take.two,Agreement!="Passport Conversions")                           
take.two<-filter(take.two,Agreement!="Passport Conversions 45 Day Cxl")
take.two<-filter(take.two,Agreement!="Personal Training 1 Pack")
take.two<-filter(take.two,Agreement!="Personal Training 12 Pack")
take.two<-filter(take.two,Agreement!="Personal Training 24 Pack")
take.two<-filter(take.two,Agreement!="Personal Training 4 Pack")
take.two<-filter(take.two,Agreement!="Personal Training 8 Pack")
take.two<-filter(take.two,Agreement!="Personal Training Elite 4 Pack")
take.two<-filter(take.two,Agreement!="Personal Training Membership 1 Session")
take.two<-filter(take.two,Agreement!="Personal Training Membership 12 Sessions")
take.two<-filter(take.two,Agreement!="Personal Training Membership 2 Session")
take.two<-filter(take.two,Agreement!="Personal Training Membership 4 Sessions")
take.two<-filter(take.two,Agreement!="Personal Training Membership 4 Sessions 6 Mth")
take.two<-filter(take.two,Agreement!="Personal Training Membership 8 Sessions")
take.two<-filter(take.two,Agreement!="Personal Training Membership 8 Sessions 6 Mth")
take.two<-filter(take.two,Agreement!="Personal Training Membership Elite 1 Session")
take.two<-filter(take.two,Agreement!="Personal Training Membership Elite 4 Sessions")
take.two<-filter(take.two,Agreement!="Personal Training Membership Elite 8 Sessions")
take.two<-filter(take.two,Agreement!="Premier Babysitting")
take.two<-filter(take.two,Agreement!="Premier Conversions")
take.two<-filter(take.two,Agreement!="Premier Conversions 45 Day Cxl")
take.two<-filter(take.two,Agreement!="PTM 4 Sessions $100.00 Off 1st Month")
take.two<-filter(take.two,Agreement!="PTM 8 Sessions $200.00 Off 1st Month")
take.two<-filter(take.two,Agreement!="PTM Elite 12 Sessions $200.00 Off 1st Month")
take.two<-filter(take.two,Agreement!="PTM Elite 4 Sessions $100.00 Off 1st Month")
take.two<-filter(take.two,Agreement!="Reg Passport Commit 64.99 BSC Annual 69.99 Upgrade")
take.two<-filter(take.two,Agreement!="Reg Passport Commit 69.99 BSC Annual 69.99 Upgrade")
take.two<-filter(take.two,Agreement!="Reg Passport MTM $74.99 BSC Annual $69.99 Upgrade")
take.two<-filter(take.two,Agreement!="Reg Passport MTM $79.99 BSC Annual $69.99 Upgrade")
take.two<-filter(take.two,Agreement!="Reg Passport MTM $84.99 BSC Annual $69.99 Upgrade")
take.two<-filter(take.two,Agreement!="Regional Passport Conversions")
take.two<-filter(take.two,Sale.Type!="Rewrite")
#new data set for 18/19 only
take.two$Sale.Date<-as.Date(take.two$Sale.Date,format="%m/%d/%Y")
half.take.two<-filter(take.two,Sale.Date>="2019-01-01")
#make new data frame with frequencies
counts<-table(half.take.two$Sale.Date)
counts
counts.df<-as.data.frame(counts)
#might cut it down to 2019
take.two$Sale.Date<-as.Date(take.two$Sale.Date,format="%m/%d/%Y")
half.take.two<-filter(take.two,Sale.Date>="2019-01-01")
counts<-table(half.take.two$Sale.Date)
counts.df<-as.data.frame(counts)
colnames(counts.df)[1]<-"Sale.Date"
counts.df$Sale.Date<-as.Date(counts.df$Sale.Date,format="%Y-%m-%d")
all.dates<-data.frame(x=seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="days"),y=0)
all.dates.df<-merge(counts.df,all.dates,by.x="Sale.Date",by.y="x",all.y=TRUE)
all.dates.df[is.na(all.dates.df)]<-0
all.dates.df<-all.dates.df[-3]
#plot histogram
hist(all.dates.df$Freq,breaks=25,col="red")
plot(density(all.dates.df$Freq))
#split by months category
months.df<-month(as.POSIXlt(months$Sale.Date, format="%Y-%m-%d"))
months<-all.dates.df
months[3]<-months.df
month.names<-c("1"="January","2"="February","3"="March",
               "4"="April","5"="May","6"="June","7"="July",
               "8"="August","9"="September","10"="October",
               "11"="November","12"="December")
months$V3<-as.character(month.names[months$V3])
#plot with split
library("lattice")
barchart(Freq ,data=months,
         group=V3,
         scales=list(cex=0.5),
         layout=c(3,4))
colnames(months)[2]<-"Sales"
colnames(months)[3]<-"Month"
months.df<-months[-1]
library(ggplot2)
plot<-ggplot(months.df,aes(x=Sales,color=Month))+
  geom_density()
plot

library(KFAS)
library(forecast)
library(mFilter)
library(ggplot2)
library(lubridate)
library(reshape)


#########################################
###--- Read in SP500 Data: Index
#---    i=1: By Week
#---    i=2: By Month
#---    n.step: Choices are 12 (month), 52 (year), 
#---              or 12 (periods out)
i<-2
if(i==1){
  stdpoor<-read.csv("~/Desktop/Big Data/Project 3/SP500Index/SP500weekly.csv")
  freq<-52
  }else{
    stdpoor<-read.csv("~/Desktop/Big Data/Project 3/SP500Index/SP500monthly.csv")
    freq<-12
}
train.sep<-1:(dim(stdpoor)[1]-freq)
test.sep<-(dim(stdpoor)[1]-freq+1):dim(stdpoor)[1]
train.comp<-1:(dim(stdpoor)[1]-12)
test.comp<-(dim(stdpoor)[1]-12+1):dim(stdpoor)[1]
#---    Test Setter
#---     j=1: Forecast at seperate frequencies
#---     j=2: Compare forecast at N-periods ahead
j<-2
if(k==1){
  train<-train.sep
  test<-test.sep
}else{
  train<-train.comp
  test<-test.comp
}
stdpoor$DATE<-ymd(mdy(stdpoor$DATE))
stdpoor<-data.frame(stdpoor$DATE,stdpoor$VALUE,
                    ts(stdpoor$VALUE,frequency=freq),
                    stdpoor$LOGVALUE,
                    ts(stdpoor$LOGVALUE,frequency=freq))
###--- Column Names
colnames(stdpoor)<-c("DATE","SP500","SP500ts","logSP500","logSP500ts")
###--- ggplot
if(i==1){
  print(ggplot(data=stdpoor,aes(x=DATE,y=SP500))+geom_line()+
    ggtitle("S&P 500 (Weekly)"))
  print(ggplot(data=stdpoor,aes(x=DATE,y=logSP500))+geom_line()+
    ggtitle("Logged S&P 500 (Weekly)")+ylab("log(SP500)"))
}else{
  print(ggplot(data=stdpoor,aes(x=DATE,y=SP500))+geom_line()+
    ggtitle("S&P 500 (Monthly)"))
  print(ggplot(data=stdpoor,aes(x=DATE,y=logSP500))+geom_line()+
    ggtitle("Logged S&P 500 (Monthly)")+ylab("log(SP500)"))
}

############################
###--- Ga Filter Fish ---###

#--- 1. Holt-Winters Filter

###--- Single Run ---###
#--- 1. Holt Winters Fit
HW<-HoltWinters(ts(stdpoor$SP500ts[train],frequency=freq))
HW.log<-HoltWinters(ts(stdpoor$logSP500ts[train],frequency=freq))
#--- 2. Holt Winters Forecast
#---  i. k=1: Forecast "freq" periods ahead
#---  ii. k=2: Forecast "12" periods ahead
k<-1
if(k==1){
  n.step<-freq
}else{
  n.step<-12
}
HWf<-forecast(HW,n.step)
HWf.log<-forecast(HW.log,n.step)
#--- 3. Holt Winters Plot
plot(HWf)
plot(HWf.log)
HWf.df<-data.frame(stdpoor$DATE[test],HWf[4],HWf[[5]][,2],
                   HWf[[6]][,2],stdpoor$SP500ts[test])
colnames(HWf.df)<-c("Date","Forecast","Lo95","Hi95","Observed")
mean<-mean(HWf[[4]]-stdpoor$SP500ts[test])
HWf.log.df<-data.frame(stdpoor$DATE[test],HWf.log[4],
                       HWf.log[[5]][,2],HWf.log[[6]][,2],
                       stdpoor$logSP500ts[test])
colnames(HWf.log.df)<-c("Date","Forecast","Lo95","Hi95","Observed")
mean.log<-mean(HWf.log[[4]]-stdpoor$logSP500ts[test])
#--- 4. ggplot Holt-Winter Forecast
#---  i. Non-logged Data
ggplot(data=HWf.df,aes(x=Date))+
  geom_line(aes(y=Forecast,colour="Forecast"))+
  geom_line(aes(y=Observed,colour="Observed"))+
  geom_ribbon(aes(ymin=Lo95,ymax=Hi95),alpha=0.3)+
  ggtitle("Forecast v. Observed")
#---  ii. Logged Data
ggplot(data=HWf.log.df,aes(x=Date))+
  geom_line(aes(y=Forecast,colour="Forecast"))+
  geom_line(aes(y=Observed,colour="Observed"))+
  geom_ribbon(aes(ymin=Lo95,ymax=Hi95),alpha=0.3)+
  ggtitle("Forecast v. Observed (Log)")

# ----- Load Packages and Download Data -----

# --- Load Packages ---
library(ggplot2)
library(quantmod)
library(neuralnet)
library(caret)
library(forecast)
library(scales)
library(plyr)
library(xts)
library(fGarch)
library(reshape2)
library(nlme)
library(KFAS)
library(mFilter)
library(lubridate)
library(reshape)

# 
# -------Neural Net Approach -------
#

# --- Create vector of ETF names to extract ---
ETF <- c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU")

# --- Get S&P Data ---
# This data spans Jan 1, 2000 to present day.  This will be used to compare 
# against forecasted values.
getSymbols("SPY", src = "yahoo", return.class = "matrix",
           from = "2000-01-01", 
           to = Sys.Date())

# The getSymbols command automatically saves the data as SPY.  We'll save the 
# data as SPY1 so we don't overwrite the data later.
SPY1 <- SPY

# --- Get ETF data ---
getSymbols(ETF, src = "yahoo", return.class = "matrix",
             from = "2000-01-01", 
             to = "2014-12-31")

# --- Get SPY data
getSymbols("SPY", src = "yahoo", return.class = "matrix",
           from = "2000-01-01", 
           to = "2014-12-31")

# This vector of names may have been helpful
# ETF.weekly <- c("XLY.weekly", "XLP.weekly", "XLE.weekly", "XLF.weekly",
#                  "XLV.weekly", "XLI.weekly", "XLB.weekly", "XLK.weekly",
#                  "XLU.weekly")

# ----- Preparing Data -----
# Rather than use daily data, we'll use weekly and monthly data
XLY.weekly <- to.weekly(XLY)
XLP.weekly <- to.weekly(XLP)
XLE.weekly <- to.weekly(XLE)
XLF.weekly <- to.weekly(XLF)
XLV.weekly <- to.weekly(XLV)
XLI.weekly <- to.weekly(XLI)
XLB.weekly <- to.weekly(XLB)
XLK.weekly <- to.weekly(XLK)
XLU.weekly <- to.weekly(XLU)
SPY.weekly <- to.weekly(SPY)
SPY1.weekly <- to.weekly(SPY1)

XLY.monthly <- to.monthly(XLY)
XLP.monthly <- to.monthly(XLP)
XLE.monthly <- to.monthly(XLE)
XLF.monthly <- to.monthly(XLF)
XLV.monthly <- to.monthly(XLV)
XLI.monthly <- to.monthly(XLI)
XLB.monthly <- to.monthly(XLB)
XLK.monthly <- to.monthly(XLK)
XLU.monthly <- to.monthly(XLU)
SPY.monthly <- to.monthly(SPY)
SPY1.monthly <- to.monthly(SPY1)

stock.data.weekly <- data.frame(XLY.weekly$XLY.Close, XLP.weekly$XLP.Close,
                                XLE.weekly$XLE.Close, XLF.weekly$XLF.Close,
                                XLV.weekly$XLV.Close, XLI.weekly$XLI.Close,
                                XLB.weekly$XLB.Close, XLK.weekly$XLK.Close,
                                XLU.weekly$XLU.Close, SPY.weekly$SPY.Close)

stock.data.monthly <- data.frame(XLY.monthly$XLY.Close, XLP.monthly$XLP.Close,
                                 XLE.monthly$XLE.Close, XLF.monthly$XLF.Close,
                                 XLV.monthly$XLV.Close, XLI.monthly$XLI.Close,
                                 XLB.monthly$XLB.Close, XLK.monthly$XLK.Close,
                                 XLU.monthly$XLU.Close, SPY.monthly$SPY.Close)

close <- c("XLY.close", "XLP.close", "XLE.close", "XLF.close",
           "XLV.close", "XLI.close", "XLB.close", "XLK.close",
           "XLU.close", "SPY.close")

colnames(stock.data.weekly) <- close
colnames(stock.data.monthly) <- close

# ----- Plotting Data -----
# Plot weekly data with some polynomial fits to see what it looks like
b <- seq(1, 783, by = 52)
ggplot(data = SPY.weekly, aes(x = row.names(SPY.weekly), y = SPY.Close)) +
  labs(title = "Weekly S&P Data", x = "Date", y = "Closing Price") +
  scale_x_discrete(breaks = row.names(SPY.weekly)[b]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_line(data = SPY.weekly, aes(x = row.names(SPY.weekly), 
                                    y = SPY.Close, group = 1)) +
  stat_smooth(data = SPY.weekly, aes(x = row.names(SPY.weekly), 
                                      y = SPY.Close, group = 1), 
              method = "gam", formula =  y ~ poly(x, 1), se = F) +
  stat_smooth(data = SPY.weekly, aes(x = row.names(SPY.weekly), 
                                      y = SPY.Close, group = 1), color = "red", 
              method = "gam", formula =  y ~ poly(x, 2), se = F) + 
  stat_smooth(data = SPY.weekly, aes(x = row.names(SPY.weekly), 
                                      y = SPY.Close, group = 1), color = "purple", 
              method = "gam", formula =  y ~ poly(x, 5), se = F) +
  stat_smooth(data = SPY.weekly, aes(x = row.names(SPY.weekly), 
                                      y = SPY.Close, group = 1), color = "green", 
              method = "gam", formula =  y ~ poly(x, 6), se = F)

#  Plot Monthly Data
# b <- seq(1, 180, by = 4)
# ggplot(data = SPY.monthly, aes(x = row.names(SPY.monthly), y = SPY.Close)) +
#   labs(title = "Monthly S&P Data", x = "Date", y = "Closing Price") +
#   #scale_x_discrete(breaks = row.names(SPY.monthly)[b]) +
#   #theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   geom_line(data = SPY.monthly, aes(x = row.names(SPY.monthly), 
#                                    y = SPY.Close, group = 1)) +
#   stat_smooth(data = SPY.monthly, aes(x = row.names(SPY.monthly), 
#                                      y = SPY.Close, group = 1), 
#               method = "gam", formula =  y ~ poly(x, 1), se = F) +
#   stat_smooth(data = SPY.monthly, aes(x = row.names(SPY.monthly), 
#                                      y = SPY.Close, group = 1), color = "red", 
#               method = "gam", formula =  y ~ poly(x, 2), se = F) + 
#   stat_smooth(data = SPY.monthly, aes(x = row.names(SPY.monthly), 
#                                      y = SPY.Close, group = 1), color = "purple", 
#               method = "gam", formula =  y ~ poly(x, 5), se = F) +
#   stat_smooth(data = SPY.monthly, aes(x = row.names(SPY.monthly), 
#                                      y = SPY.Close, group = 1), color = "green", 
#               method = "gam", formula =  y ~ poly(x, 6), se = F)

# ----- Forecast using Neural Net -----
# Weekly Data
SPY.WC <- SPY.weekly$SPY.Close
fit <- nnetar(SPY.WC, p=5, repeats = 25)
fcast <- forecast(SPY.WC, h = 26, level = c(80, 85, 90, 95, 99))
df.fcast <- data.frame(fcast)
SPY1.WC <- SPY1.weekly$SPY1.Close
df.SPY1.WC <- data.frame(SPY1.WC[779:804])

# Plot it
ggplot(data = df.fcast, aes(x = row.names(df.fcast2), y = Point.Forecast)) +
  labs(title = "Weekly S&P Forecast with \n 90% and 95% Confidence Bands", 
       x = "Weeks since 2000", y = "Closing Price") +
  geom_line(data = df.fcast, aes(x = row.names(df.fcast), 
                                  y = Point.Forecast, group = 1)) + 
  geom_line(data = df.fcast, aes(x = row.names(df.fcast), 
                                  y = Lo.90, group = 1)) + 
  geom_line(data = df.fcast, aes(x = row.names(df.fcast), 
                                  y = Hi.90, group = 1)) + 
  geom_line(data = df.fcast, aes(x = row.names(df.fcast), 
                                  y = Lo.95, group = 1)) +
  geom_line(data = df.fcast, aes(x = row.names(df.fcast), 
                                  y = Hi.95, group = 1)) +
  geom_line(data = df.SPY1.WC, aes(x = c(1:26), y = SPY1.WC.779.804.))

# Monthly Data
SPY.MC <- SPY.monthly$SPY.Close
fit2 <- nnetar(SPY.MC, p=5, repeats = 25)
fcast2 <- forecast(SPY.MC, h = 6, level = c(80, 85, 90, 95, 99))
df.fcast2 <- data.frame(fcast2)
SPY1.MC <- SPY1.monthly$SPY1.Close
df.SPY1.MC <- data.frame(SPY1.MC[180:185])

# Plot it
ggplot(data = df.fcast2, aes(x = row.names(df.fcast2), y = Point.Forecast)) +
  labs(title = "Monthly S&P Forecast with \n 90% and 95% Confidence Bands", 
       x = "Months since 2000", y = "Closing Price") +
  geom_line(data = df.fcast2, aes(x = row.names(df.fcast2), 
                                  y = Point.Forecast, group = 1)) + 
  geom_line(data = df.fcast2, aes(x = row.names(df.fcast2), 
                                  y = Lo.90, group = 1)) + 
  geom_line(data = df.fcast2, aes(x = row.names(df.fcast2), 
                                  y = Hi.90, group = 1)) + 
  geom_line(data = df.fcast2, aes(x = row.names(df.fcast2), 
                                  y = Lo.95, group = 1)) +
  geom_line(data = df.fcast2, aes(x = row.names(df.fcast2), 
                                  y = Hi.95, group = 1)) +
  geom_line(data = df.SPY1.MC, aes(x = c(1:6), y = SPY1.MC.180.185.))








# ---------- Garch Model ----------


symbols <- c("XLE","XLU","XLV","XLP","XLB","XLY","XLK","XLF","XLI","^GSPC")
getSymbols(symbols)

## Change everything to monthly 
XLB_monthly<-to.monthly(XLB)
XLE_monthly<-to.monthly(XLE)
XLU_monthly<-to.monthly(XLU)
XLV_monthly<-to.monthly(XLV)
XLP_monthly<-to.monthly(XLP)
XLY_monthly<-to.monthly(XLY)
XLK_monthly<-to.monthly(XLK)
XLF_monthly<-to.monthly(XLF)
XLI_monthly<-to.monthly(XLI)
GSPC_monthly<-to.monthly(GSPC)

GSPC_monthly_GF <- garchFit(GSPC.Close~garch(2,1),data=GSPC_monthly[1:96])

XLB_monthly_GF_close <- garchFit(XLB.Close~arma(1,1)+garch(1,1),
                                 data=XLB_monthly[1:96])
XLE_monthly_GF_close <- garchFit(XLE.Close~arma(1,1)+garch(1,1),
                                 data=XLE_monthly[1:96])
XLU_monthly_GF_close <- garchFit(XLU.Close~arma(1,1)+garch(1,1),
                                 data=XLU_monthly[1:96])
XLV_monthly_GF_close <- garchFit(XLV.Close~garch(1,1),
                                 data=XLV_monthly[1:96],trace=FALSE)
XLP_monthly_GF_close <- garchFit(XLP.Close~garch(1,1),
                                 data=XLP_monthly[1:96])
XLY_monthly_GF_close <- garchFit(XLY.Close~garch(1,1),
                                 data=XLY_monthly[1:96])  
XLK_monthly_GF_close <- garchFit(XLK.Close~garch(1,1),
                                 data=XLK_monthly[1:96])
XLF_monthly_GF_close <- garchFit(XLF.Close~garch(1,1),
                                 data=XLF_monthly[1:96])
XLI_monthly_GF_close <- garchFit(XLI.Close~arma(1,1)+garch(1,1),
                                 data=XLI_monthly[1:96])

## Creating prediction for the different sectors
XLB_pred<-predict(XLB_monthly_GF_close,n.ahead=6) 
XLE_pred<-predict(XLE_monthly_GF_close,n.ahead=6)
XLU_pred<-predict(XLU_monthly_GF_close,n.ahead=6) 
XLV_pred<-predict(XLV_monthly_GF_close,n.ahead=6) 
XLP_pred<-predict(XLP_monthly_GF_close,n.ahead=6) 
XLY_pred<-predict(XLY_monthly_GF_close,n.ahead=6)  
XLK_pred<-predict(XLK_monthly_GF_close,n.ahead=6) 
XLF_pred<-predict(XLF_monthly_GF_close,n.ahead=6) 
XLI_pred<-predict(XLI_monthly_GF_close,n.ahead=6) 

## Our test set of predictions from GARCH(1,1) fit
testdata <- data.frame(XLB_pred$meanForecast,XLE_pred$meanForecast,
                       XLF_pred$meanForecast,XLI_pred$meanForecast,
                       XLK_pred$meanForecast,XLP_pred$meanForecast,
                       XLU_pred$meanForecast,XLV_pred$meanForecast,
                       XLY_pred$meanForecast)
names(testdata)<-c("XLB","XLE","XLF","XLI","XLK","XLP","XLU","XLV","XLY")

## Loading GLM models and fitting and predicting

close_data<-data.frame(GSPC_monthly$GSPC.Close[1:96],XLB_monthly$XLB.Close[1:96]
                       ,XLE_monthly$XLE.Close[1:96],XLF_monthly$XLF.Close[1:96],
                       XLI_monthly$XLI.Close[1:96],XLK_monthly$XLK.Close[1:96],
                       XLP_monthly$XLP.Close[1:96],XLU_monthly$XLU.Close[1:96],
                       XLV_monthly$XLV.Close[1:96],XLY_monthly$XLY.Close[1:96])


dates <- row.names(close_data)
close_data$date <-dates

#### graphs of the time series of the sectors
names(close_data)<-c("GSPC","XLB","XLE","XLF","XLI","XLK",
                     "XLP","XLU","XLV","XLY","date")

close_melt <- melt(close_data,id.vars="date")
ggplot(close_melt,aes(x=date,y=value,group=variable))+
  geom_line()+
  facet_wrap(~variable,scales='free_y',ncol=1)+
  theme_bw()

### our predictions form our GLS model 
model <- gls(GSPC~XLB+XLE+XLF+XLI+XLK+XLP+XLU+XLV+XLY,
             data=close_data,correlation=corAR1(0.5,form=~1))
pred_model<-predict(model,testdata,se.fit=TRUE)
t(GSPC_monthly$GSPC.Close[97:101])





# ---------- Holt Winters Filter ----------
#########################################
###--- Read in SP500 Data: Index
#---    i=1: By Week
#---    i=2: By Month
#---    n.step: Choices are 12 (month), 52 (year), 
#---              or 12 (periods out)
i<-2
if(i==1){
  stdpoor<-read.csv("SP500weekly.csv")
  freq<-52
}else{
  stdpoor<-read.csv("SP500monthly.csv")
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

#----- 4. ggplot Holt-Winter Forecast

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

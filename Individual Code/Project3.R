library(quantmod)
library(plyr)
library(xts)
library(fGarch)
symbols <- c("XLE","XLU","XLV","XLP","XLB","XLY","XLK","XLF","XLI","^GSPC")
getSymbols(symbols)
## Changfe everything to monthly 
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

XLB_monthly_GF_close <- garchFit(XLB.Close~arma(1,1)+garch(1,1),data=XLB_monthly[1:96])
XLE_monthly_GF_close <- garchFit(XLE.Close~arma(1,1)+garch(1,1),data=XLE_monthly[1:96])
XLU_monthly_GF_close <- garchFit(XLU.Close~arma(1,1)+garch(1,1),data=XLU_monthly[1:96])
XLV_monthly_GF_close <- garchFit(XLV.Close~garch(1,1),data=XLV_monthly[1:96],trace=FALSE)
XLP_monthly_GF_close <- garchFit(XLP.Close~garch(1,1),data=XLP_monthly[1:96])
XLY_monthly_GF_close <- garchFit(XLY.Close~garch(1,1),data=XLY_monthly[1:96])  
XLK_monthly_GF_close <- garchFit(XLK.Close~garch(1,1),data=XLK_monthly[1:96])
XLF_monthly_GF_close <- garchFit(XLF.Close~garch(1,1),data=XLF_monthly[1:96])
XLI_monthly_GF_close <- garchFit(XLI.Close~arma(1,1)+garch(1,1),data=XLI_monthly[1:96])
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
testdata <- data.frame(XLB_pred$meanForecast,XLE_pred$meanForecast,XLF_pred$meanForecast,XLI_pred$meanForecast,
                  XLK_pred$meanForecast,XLP_pred$meanForecast,XLU_pred$meanForecast,XLV_pred$meanForecast,
                  XLY_pred$meanForecast)
names(testdata)<-c("XLB","XLE","XLF","XLI","XLK","XLP","XLU","XLV","XLY")
## Loading GLM models and fitting and predicting
library(nlme)
close_data<-data.frame(GSPC_monthly$GSPC.Close[1:96],XLB_monthly$XLB.Close[1:96],XLE_monthly$XLE.Close[1:96]
,XLF_monthly$XLF.Close[1:96],XLI_monthly$XLI.Close[1:96],XLK_monthly$XLK.Close[1:96],XLP_monthly$XLP.Close[1:96]
,XLU_monthly$XLU.Close[1:96],XLV_monthly$XLV.Close[1:96],XLY_monthly$XLY.Close[1:96])


dates <- row.names(close_data)
close_data$date <-dates

#### graphs of the time series of the sectors
names(close_data)<-c("GSPC","XLB","XLE","XLF","XLI","XLK","XLP","XLU","XLV","XLY","date")
library(ggplot2)
library(reshape2)
close_melt <- melt(close_data,id.vars="date")
ggplot(close_melt,aes(x=date,y=value,group=variable))+
  geom_line()+
  facet_wrap(~variable,scales='free_y',ncol=1)+
  theme_bw()

### our predictions form our GLS model 
model <- gls(GSPC~XLB+XLE+XLF+XLI+XLK+XLP+XLU+XLV+XLY,data=close_data,correlation=corAR1(0.5,form=~1))
pred_model<-predict(model,testdata,se.fit=TRUE)
t(GSPC_monthly$GSPC.Close[97:101])

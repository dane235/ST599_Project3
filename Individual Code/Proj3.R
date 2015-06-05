# ----- Load Packages and Download Data -----

# --- Load Packages ---
library(ggplot2)
library(quantmod)
library(neuralnet)
library(caret)
library(forecast)
library(scales)

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

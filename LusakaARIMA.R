
library(tseries)
library(forecast)
library(MASS)
library(ggplot2)

setwd("E:\\ResearchProject\\Najmul Bhai\\Lusaka")

LusakaData <- read.csv("DataTemp.csv", header = T)
head(LusakaData)
data <- data.frame(date = as.Date(LusakaData$Date), #2020-01-01
                   value = LusakaData$T2M)
head(data)

data_new1 <- data                                   # Duplicate data
data_new1$year <- strftime(data_new1$date, "%Y")    # Create year column
data_new1$month <- strftime(data_new1$date, "%m")   # Create month column
head(data_new1)    





data_aggr1 <- aggregate(value ~ month + year,       # Aggregate data
                        data_new1,
                        FUN = mean)
head(data_aggr1)                                    # Head of aggregated data



Firstdecade <- data_aggr1[which(data_aggr1$year<='2000'), ]
NROW(Firstdecade)
min(Firstdecade$value)
max(Firstdecade$value)
mean(Firstdecade$value)
sd(Firstdecade$value)

Lastdecade <- data_aggr1[which(data_aggr1$year>'2000'), ]
NROW(Lastdecade)
min(Lastdecade$value)
max(Lastdecade$value)
mean(Lastdecade$value)
sd(Lastdecade$value)


library("lubridate")

data_new2 <- data                                   # Duplicate data
data_new2$year_month <- floor_date(data_new2$date,  # Create year-month column
                                   "month")
head(data_new2)                                     # Head of updated data


library("dplyr")                                    # Load dplyr
data_aggr2 <- data_new2 %>%                         # Aggregate data
  group_by(year_month) %>% 
  dplyr::summarize(value = mean(value)) %>% 
  as.data.frame()
head(data_aggr2)                                    # Head of aggregated data


library(tseries)
tsData = ts(data_aggr2$value, start = c(1981,6), frequency = 12)

auto.arima(tsData)

Fit<-Arima(tsData, order=c(0,0,1), seasonal=list(order=c(2,1,1),period=12))
summary(Fit)

fcast <- forecast(Fit, h=12)

m <- lm(data_aggr2$value ~ data_aggr2$year_month, data = data_aggr2)

n <- autoplot(fcast)  +
  xlab("Years") + ylab(expression("Monthly mean temperature ("^o*"C)")) +ggtitle("Lusaka, Zambia (SARIMA Model)")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18))+  
  geom_abline(slope = m$coefficients[2], 
              intercept = m$coefficients[1],
              colour = "black")
n

#Menn kendal
library(Kendall)
library(trend)

t.test(tsData)$"conf.int"
mean(tsData)

MannKendall(tsData)
sens.slope(tsData, conf.level = 0.95)


rn <- NROW(tsData)
#R2
SSE <- sum((resid(Fit[1:rn]))^2)
SST <- sum((tsData[1:rn] - mean(tsData[1:rn]))^2)
R_square <- 1 - SSE / SST
R_square













LusakaData <- read.csv("DataRain.csv", header = T)
head(LusakaData)
data <- data.frame(date = as.Date(LusakaData$Date), #2020-01-01
                   value = LusakaData$T2M)
head(data)

data_new1 <- data                                   # Duplicate data
data_new1$year <- strftime(data_new1$date, "%Y")    # Create year column
data_new1$month <- strftime(data_new1$date, "%m")   # Create month column
head(data_new1)    





data_aggr1 <- aggregate(value ~ month + year,       # Aggregate data
                        data_new1,
                        FUN = sum)
head(data_aggr1)                                    # Head of aggregated data



Firstdecade <- data_aggr1[which(data_aggr1$year<='2000'), ]
NROW(Firstdecade)
min(Firstdecade$value)
max(Firstdecade$value)
mean(Firstdecade$value)
sd(Firstdecade$value)

Lastdecade <- data_aggr1[which(data_aggr1$year>'2000'), ]
NROW(Lastdecade)
min(Lastdecade$value)
max(Lastdecade$value)
mean(Lastdecade$value)
sd(Lastdecade$value)


library("lubridate")

data_new2 <- data                                   # Duplicate data
data_new2$year_month <- floor_date(data_new2$date,  # Create year-month column
                                   "month")
head(data_new2)                                     # Head of updated data


library("dplyr")                                    # Load dplyr
data_aggr2 <- data_new2 %>%                         # Aggregate data
  group_by(year_month) %>% 
  dplyr::summarize(value = sum(value)) %>% 
  as.data.frame()
head(data_aggr2)                                    # Head of aggregated data


library(tseries)
tsData = ts(data_aggr2$value, start = c(1981,6), frequency = 12)

auto.arima(tsData)

Fit<-Arima(tsData, order=c(0,0,0), seasonal=list(order=c(1,1,2),period=12))
summary(Fit)

fcast <- forecast(Fit, h=12)

m <- lm(data_aggr2$value ~ data_aggr2$year_month, data = data_aggr2)

o <- autoplot(fcast)  +
  xlab("Years") + ylab("Monthly total rainfall (mm)") +ggtitle("Lusaka, Zambia (SARIMA Model)")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18))+  
  geom_abline(slope = m$coefficients[2], 
              intercept = m$coefficients[1],
              colour = "black")
o

#Menn kendal
library(Kendall)
library(trend)

t.test(tsData)$"conf.int"
mean(tsData)

MannKendall(tsData)
sens.slope(tsData, conf.level = 0.95)


rn <- NROW(tsData)
#R2
SSE <- sum((resid(Fit[1:rn]))^2)
SST <- sum((tsData[1:rn] - mean(tsData[1:rn]))^2)
R_square <- 1 - SSE / SST
R_square


tiff("TS.tiff", units="in", width=16, height=10, res=300)
gridExtra::grid.arrange(n,o)
dev.off()









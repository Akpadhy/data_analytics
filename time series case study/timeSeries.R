require(tidyr)
require(dplyr)
require(raster)
require(lubridate)
require(graphics)
require(tseries)
library(forecast)

#importing the DS
global_store<-read.csv("Global Superstore.csv")


str(global_store)

#Looking at the problem statement
#We can cut down on the number of columns required

global_store_final<-global_store[c(3,8,13,16,19,20,21,22)]
global_store_final$Order.Date <- as.Date(global_store_final$Order.Date,"%d-%m-%Y")

#head(global_store_final$Order.Date)

str(global_store_final)

unique(global_store_final$Segment)
unique(global_store_final$Market)


#checking for NA values in the DS
sapply(global_store_final,function(x) sum(is.na(x)))


#Checking for duplicate rows in DS
sum(duplicated(global_store_final))


#Data Understanding:
#The data currently has the transaction level data, 
#where each row represents a particular order made on the online store. 
#There are 24 attributes related to each such transaction among which 8 have been shortlisted.
#The "Market" attribute has 7-factor levels 
#representing the geographical market sector that the customer belongs to. 

#The "Segment" attribute tells which of the 3 segments that customer belongs to. 
#You will find the complete data dictionary for the dataset from the link below.


#Data preparation:

#You would need to first segment the whole dataset into the 21 subsets 
#based on the market and the customer segment level. 


#Next, comes the most important data preparation step. 
#That is to convert the transaction-level data into a time series. 
#As, weneed to aggregate the 3 attributes  - Sales, Quantity & Profit
#over the Order Date to arrive at monthly values for these attributes for each market segment
#we derive a new column Month_seq which starts with 1 for the earliest date 
#and increments by 1 for every month from the earliest date. 

global_store_final$Order_year = year(global_store_final$Order.Date)
global_store_final$Order_month = month(global_store_final$Order.Date)

Month_year <- global_store_final$Order_year*12 + global_store_final$Order_month

Min_month_year <- min(Month_year)
Month_year_seq <- Month_year - Min_month_year +1
global_store_final$Month_seq = Month_year_seq
global_store_final <- global_store_final[order(global_store_final$Month_seq),c(1:11)]



#Once, you arrive at these 3 time series for each of the 21 segments,
#we need to find the 2 most profitable and consistently profitable segments. 
#For this, the metric that you can use is the coefficient of variation of the Profit for all 21 market segments. 


#Summing up Sales,Quantity & Profit vals on month of the Order.Date for all combinations of Market and Segments.

global_store_agg<-aggregate(global_store_final[,c("Sales","Quantity","Profit")],
                            by=list(global_store_final$Market,global_store_final$Segment,
                                    global_store_final$Month_seq)
                            ,FUN = sum)

colnames(global_store_agg)[1]<-c("Market")
colnames(global_store_agg)[2]<-c("Segment")
colnames(global_store_agg)[3]<-c("Month_Seq")

# To identify the market segments that are consistently profitable and profits with least variance
# we need to calculate sum of profit and variance for each market segment.

#aggregate (sum) profit for each month for each market segment
global_store_profit_agg <-aggregate(global_store_agg[,c("Profit")],
                                    by=list(global_store_agg$Market,global_store_agg$Segment)
                                    ,FUN = sum) 

head(global_store_profit_agg)
colnames(global_store_profit_agg)[1]<-c("Market")
colnames(global_store_profit_agg)[2]<-c("Segment")
colnames(global_store_profit_agg)[3]<-c("Profit_sum")

#aggregate cv for each month for each market segment
global_store_cv_agg <-aggregate(global_store_agg[,c("Profit")],
                                by=list(global_store_agg$Market,global_store_agg$Segment)
                                ,FUN = cv)

colnames(global_store_cv_agg)[1]<-c("Market")
colnames(global_store_cv_agg)[2]<-c("Segment")
colnames(global_store_cv_agg)[3]<-c("Profit_cv")

global_store_cv_profit_agg <- merge(global_store_profit_agg,global_store_cv_agg)


#Get the first two rows which has the highest profit
head(global_store_cv_profit_agg[order(-global_store_cv_profit_agg$Profit_sum),c(1:2)],2)

#Get the first two rows which has the least cv
head(global_store_cv_profit_agg[order(global_store_cv_profit_agg$Profit_cv),c(1:2)],2)

#Both have the same common Market Segment listed 
# that is EU Consumer and APAC Consumer

#View(global_store_cv_profit_agg)


consumer_apac_agg<-subset(global_store_agg,global_store_agg$Market=="APAC" & global_store_agg$Segment=="Consumer")
consumer_eu_agg<-subset(global_store_agg,global_store_agg$Market=="EU" & global_store_agg$Segment=="Consumer")

#APAC Consumer and EU consumer seems to have the lower cv on profit and the highest profit across all the segments.


#### From the analysis above the top 2 profitable Market-Segment are
### Consumer - APAC & Consumer-EU
###Both the Market-Segments has the least 2 CVs and top 2 sales.
### Next we will model TS on these 2 DS

###Building the TS for consumer_apac 

ylab <- c("Sales")
xlab<-c("Month")
title<-c("Sales from Jan 2011 to Dec 2014")
xcol <- c(1)
ycol <- c(2)

#Out of the 48 unique months, use the first 42 rows representing the earliest 42 months to train the model.
#And the rest 6 months to test the model.

total_tseries_apac_sales<-ts(consumer_apac_agg$Sales)
cons_apac_sales_train<-consumer_apac_agg[1:42,]
timeseries_apac_sales<-ts(cons_apac_sales_train$Sales)
plot(timeseries_apac_sales)

#Since the TS plot has moslty trend , smoothening is done using simple moving average method
w <-3
smoothedseries_apac_sales_1 <- stats::filter(timeseries_apac_sales, 
filter=rep(1/(2*w + 1),(2*w + 1)), 
method='convolution', sides=2)	

n<-nrow(cons_apac_sales_train)

diff <- smoothedseries_apac_sales_1[n-w] - smoothedseries_apac_sales_1[n-w-1]
for (i in seq(n-w+1, n)) {
smoothedseries_apac_sales_1[i] <- smoothedseries_apac_sales_1[i-1] + diff
}

diff <- smoothedseries_apac_sales_1[w+2] - smoothedseries_apac_sales_1[w+1]
for (i in seq(w,1,-1)) {
smoothedseries_apac_sales_1[i] <- smoothedseries_apac_sales_1[i+1] - diff
}

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe


timevals_apac_sales_in<-cons_apac_sales_train$Month_Seq
lines(smoothedseries_apac_sales_1, col="blue", lwd=2)
smootheddf_apac_sales <- as.data.frame(cbind(timevals_apac_sales_in, as.vector(smoothedseries_apac_sales_1)))
colnames(smootheddf_apac_sales) <- c('Month', 'Sales')
#View(smootheddf_apac_sales)
lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
+             + Month, data=smootheddf_apac_sales)
global_pred_apac <- predict(lmfit, Month=timevals_apac_sales_in)
lines(timevals_apac_sales_in, global_pred_apac, col='red', lwd=2)



local_pred_apac_sales<-timeseries_apac_sales-global_pred_apac
plot(local_pred_apac_sales,col="yellow",lwd=2)
plot(local_pred_apac_sales,col="yellow",type='l')
plot(local_pred_apac_sales,col="red",type='l')
acf(local_pred_apac_sales,type = "partial")
acf(local_pred_apac_sales)
armafit <- auto.arima(local_pred_apac_sales)

armafit
##Series: local_pred_apac_sales 
##ARIMA(0,0,0) with zero mean 

##sigma^2 estimated as 101931784:  log likelihood=-446.83
##AIC=895.66   AICc=895.76   BIC=897.4

resi_apac_sales<-local_pred_apac_sales-fitted(armafit)
adf.test(resi_apac_sales,alternative = "stationary")

##Augmented Dickey-Fuller Test

##data:  resi_apac_sales
##Dickey-Fuller = -5.1102, Lag order = 3, p-value = 0.01 
##p-value is less than 0.05 hence it rejects null hypothesis, hence the timeseries is stationary
##alternative hypothesis: stationary


kpss.test(resi_apac_sales)

##KPSS Test for Level Stationarity

##data:  resi_apac_sales
##KPSS Level = 0.025409, Truncation lag parameter = 1, p-value = 0.1
##p-values is more than 0.05 then it failes to reject the null hypothesis, hence the timeseries is stationary


outdata_apac_sales<-consumer_apac_agg[43:48,]
timevals_apac_sales_out<-outdata_apac_sales$Month_Seq
global_pred_apac_out<-predict(lmfit,data.frame(Month=timevals_apac_sales_out))

fcast_apac_sales<-global_pred_apac_out

MAPE_sales<-accuracy(fcast_apac_sales,outdata_apac_sales[,4])[5]
##37.3835
class_dec_pred_sales <- c(ts(global_pred_apac),ts(global_pred_apac_out))
 plot(total_tseries_apac_sales, col = "black")
 lines(class_dec_pred_sales, col = "red")

##changing the degree polynomial to 2 and then predicting

lmfit_1 <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
  +             +             + Month, data=smootheddf_apac_sales)
global_pred_apac_1<- predict(lmfit_1, Month=timevals_apac_sales_in)
lines(timevals_apac_sales_in, global_pred_apac_1, col='green', lwd=2) 
local_pred_apac_sales_1<-timeseries_apac_sales-global_pred_apac_1
plot(local_pred_apac_sales_1,col="yellow",lwd=2)
plot(local_pred_apac_sales_1,col="yellow",type='l')
plot(local_pred_apac_sales_1,col="red",type='l')
acf(local_pred_apac_sales_1,type = "partial")
acf(local_pred_apac_sales_1) ### acf of the series shows only spike and all other falling within the acceptable range.
armafit_1 <- auto.arima(local_pred_apac_sales_1)
##checking the armafit
armafit_1
##Series: local_pred_apac_sales 
##ARIMA(0,0,0) with zero mean 

##sigma^2 estimated as 104589005:  log likelihood=-447.37
##AIC=896.74   AICc=896.84   BIC=898.48 ###AIC,BIC and log likelihood will be comapred with the Auto Arima Model

##peforming the dickey fuller and kpss test to check for stationarity
resi_apac_sales_1<-local_pred_apac_sales_1-fitted(armafit_1)
adf.test(resi_apac_sales_1,alternative = "stationary")
	##Augmented Dickey-Fuller Test

##data:  resi_apac_sales_1
##Dickey-Fuller = -4.7612, Lag order = 3, p-value = 0.01
##alternative hypothesis: stationary
kpss.test(resi_apac_sales_1)
##KPSS Test for Level Stationarity

##data:  resi_apac_sales_1
##KPSS Level = 0.029775, Truncation lag parameter = 1, p-value = 0.1
###evaluating the model on the last 6 months of data
global_pred_apac_out_1<-predict(lmfit_1,data.frame(Month=timevals_apac_sales_out))
fcast_apac_sales_1<-global_pred_apac_out_1

##evaluating the model by the help of MAPE
MAPE_sales_1<-accuracy(fcast_apac_sales_1,outdata_apac_sales[,4])[5]

#here MAPE of the model has considerably improved.
MAPE_sales_1
#27.71026
 class_dec_pred_sales_1 <- c(ts(global_pred_apac_1),ts(global_pred_apac_out_1))
plot(total_tseries_apac_sales, col = "black")
lines(class_dec_pred_sales_1, col = "blue")

###Now predicting sales using ARIMA model

arima_apac_sales<-auto.arima((timeseries_apac_sales))
arima_apac_sales
##Series: (timeseries_apac_sales) 
##ARIMA(0,1,1) 

##Coefficients:
##ma1
##-0.7559
##s.e.   0.1381

##sigma^2 estimated as 174361555:  log likelihood=-447.11
##AIC=898.23   AICc=898.55   BIC=901.66


plot(arima_apac_sales$x,col="black")
#Again, let's check if the residual series is white noise
resi_arima_apac_sales<-timeseries_apac_sales-fitted(arima_apac_sales)
##Dickey -Fuller Test 
adf.test(resi_arima_apac_sales,alternative = "stationary")

##Augmented Dickey-Fuller Test

##data:  resi_arima_apac_sales
##Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01 #p <0.05; hence stationary
##alternative hypothesis: stationary

kpss.test(resi_arima_apac_sales)

##KPSS Test for Level Stationarity

##data:  resi_arima_apac_sales
##KPSS Level = 0.042734, Truncation lag parameter = 1, p-value = 0.1

fca_arima_apac_sales<-predict(arima_apac_sales,n.ahead=6)
Mape_sales_arima<-accuracy(fca_arima_apac_sales$pred,outdata_apac_sales[,4])[5]
Mape_sales_arima
##[1] 27.68952

auto_arima_pred <- c(fitted(arima_apac_sales),ts(fca_arima_apac_sales$pred))
plot(total_tseries_apac_sales, col = "black")
lines(auto_arima_pred, col = "red")

###Modelling the Consumer-APAC Quantity
###First using Classical Decomposition method
cons_apac_qty_train<-cons_apac_sales_train
##creating the timeseries
timeseries_apac_qty<-ts(cons_apac_qty_train$Quantity)
plot(timeseries_apac_qty)

###Since the TS plot has both trend , smoothening is done using simple Moving avdrage
w <-3
smoothedseries_apac_qty_1 <- stats::filter(timeseries_apac_qty, 
filter=rep(1/(2*w + 1),(2*w + 1)), 
method='convolution', sides=2)

diff <- smoothedseries_apac_qty_1[n-w] - smoothedseries_apac_qty_1[n-w-1]
for (i in seq(n-w+1, n)) {
smoothedseries_apac_qty_1[i] <- smoothedseries_apac_qty_1[i-1] + diff
}

diff <- smoothedseries_apac_qty_1[w+2] - smoothedseries_apac_qty_1[w+1]
for (i in seq(w,1,-1)) {
smoothedseries_apac_qty_1[i] <- smoothedseries_apac_qty_1[i+1] - diff
}

timevals_apac_qty_in<-cons_apac_qty_train$Month_Seq
lines(smoothedseries_apac_qty_1, col="blue", lwd=2)
##converting the tseries into a dataframe
smootheddf_apac_qty<- as.data.frame(cbind(timevals_apac_qty_in, as.vector(smoothedseries_apac_qty_1)))
##assigning column names
colnames(smootheddf_apac_qty) <- c('Month', 'Quantity')
#View(smootheddf_apac_qty)
##creating linear regression fit to the sinusodial graph of polinomial 3
lmfit_2 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
+             + Month, data=smootheddf_apac_qty)
global_pred_apac_2 <- predict(lmfit_2, Month=timevals_apac_qty_in)
lines(timevals_apac_qty_in, global_pred_apac_2, col='red', lwd=2)
local_pred_apac_qty<-timeseries_apac_qty-global_pred_apac_2
acf(local_pred_apac_qty)
acf(local_pred_apac_qty,type = "partial")## acf lies withtin the acceptable range
plot(local_pred_apac_qty,col="orange",type='l')

armafit_2<- auto.arima(local_pred_apac_qty)

armafit_2
##Series: local_pred_apac_qty 
##ARIMA(0,0,0) with zero mean 

##sigma^2 estimated as 12908:  log likelihood=-258.37
##AIC=518.75   AICc=518.85   BIC=520.48

resi_apac_qty<-local_pred_apac_qty-fitted(armafit_2)
##dickey fuller & kpss test to show the statioanrity of the data
adf.test(resi_apac_qty,alternative = "stationary")

##Augmented Dickey-Fuller Test

##data:  resi_apac_qty
##Dickey-Fuller = -5.574, Lag order = 3, p-value = 0.01
##alternative hypothesis: stationary

kpss.test(resi_apac_qty)

##KPSS Test for Level Stationarity

##data:  resi_apac_qty
##KPSS Level = 0.022934, Truncation lag parameter = 1, p-value = 0.1

##evaluate the model on the last 6 months
timevals_apac_qty_out<-timevals_apac_sales_out
global_pred_apac_out_2<-predict(lmfit_2,data.frame(Month=timevals_apac_qty_out))

fcast_apac_qty<-global_pred_apac_out_2
View(outdata_apac_sales)
outdata_apac_qty<-outdata_apac_sales
fcast_apac_qty<-global_pred_apac_out_2
##MAPE to evaluate the model
MAPE_qty<-accuracy(fcast_apac_qty,outdata_apac_qty[,5])[5]
MAPE_qty
#30.49665
####total vs predicted
class_dec_pred_qty <- c(ts(global_pred_apac_2),ts(global_pred_apac_out_2))
 plot(total_tseries_apac_qty, col = "black")
 lines(class_dec_pred_qty, col = "green")
 
###Model by Arima
arima_apac_qty<-auto.arima((timeseries_apac_qty))
arima_apac_qty

##Series: (timeseries_apac_qty) 
##ARIMA(0,1,0) 

##sigma^2 estimated as 25366:  log likelihood=-266.07
##AIC=534.14   AICc=534.24   BIC=535.85### all the parameters are more than the classical decomposition
plot(arima_apac_qty$x,col="black")
resi_arima_apac_qty<-timeseries_apac_qty-fitted(arima_apac_qty)
##Dickey FUller test
adf.test(resi_arima_apac_qty,alternative = "stationary")
####Augmented Dickey-Fuller Test

##data:  resi_arima_apac_qty
##Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
##alternative hypothesis: stationary
##KPSS test
kpss.test(resi_arima_apac_qty)

##KPSS Test for Level Stationarity

##data:  resi_arima_apac_qty
##KPSS Level = 0.031535, Truncation lag parameter = 1, p-value = 0.1

###from the above formal test we conclude that the model is statioanry

fca_arima_apac_qty<-predict(arima_apac_qty,n.ahead=6)
 Mape_qty_arima<-accuracy(fca_arima_apac_qty$pred,outdata_apac_qty[,4])[5]
 Mape_qty_arima
##98.71101 ### Mape higher than the classical decomposition
auto_arima_pred_1<- c(fitted(arima_apac_qty),ts(fca_arima_apac_qty$pred))
##creating ts on quantity
total_tseries_apac_qty<-ts(consumer_apac_agg$Quantity)
#finding the predicted value
auto_arima_pred_1<- c(fitted(arima_apac_qty),ts(fca_arima_apac_qty$pred))
plot(total_tseries_apac_qty, col = "black")
#checking the predicted curve agains the original ts
lines(auto_arima_pred_1, col = "red")


####Now modelling Consumer-EU segment 
##First classical decomposition method
###Creating time series on the Sales



ylab <- c("Sales")
xlab<-c("Month")
title<-c("Sales from Jan 2011 to Dec 2014")
xcol <- c(1)
ycol <- c(2)

total_tseries_eu_sales<-ts(consumer_eu_agg$Sales)
cons_eu_sales_train<-consumer_eu_agg[1:42,]
timeseries_eu_sales<-ts(cons_eu_sales_train$Sales)
plot(timeseries_eu_sales)

###Since the TS plot has mainly trend, smoothening is done using simple average method
w <-2
smoothedseries_eu_sales_1 <- stats::filter(timeseries_eu_sales, 
filter=rep(1/(2*w + 1),(2*w + 1)), 
method='convolution', sides=2)	

diff <- smoothedseries_eu_sales_1[n-w] - smoothedseries_eu_sales_1[n-w-1]
for (i in seq(n-w+1, n)) {
smoothedseries_eu_sales_1[i] <- smoothedseries_eu_sales_1[i-1] + diff
}

diff <- smoothedseries_eu_sales_1[w+2] - smoothedseries_eu_sales_1[w+1]
for (i in seq(w,1,-1)) {
smoothedseries_eu_sales_1[i] <- smoothedseries_eu_sales_1[i+1] - diff
}

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe


timevals_eu_sales_in<-cons_eu_sales_train$Month_Seq
lines(smoothedseries_eu_sales_1, col="blue", lwd=2)
smootheddf_eu_sales <- as.data.frame(cbind(timevals_eu_sales_in, as.vector(smoothedseries_eu_sales_1)))
colnames(smootheddf_eu_sales) <- c('Month', 'Sales')
View(smootheddf_eu_sales)
lmfit_3<- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
+             + Month, data=smootheddf_eu_sales)
global_pred_eu <- predict(lmfit_3, Month=timevals_eu_sales_in)
lines(timevals_eu_sales_in, global_pred_eu, col='red', lwd=2)

local_pred_eu_sales<-timeseries_eu_sales-global_pred_eu
plot(local_pred_eu_sales,col="yellow",lwd=2)
plot(local_pred_eu_sales,col="yellow",type='l')
plot(local_pred_eu_sales,col="red",type='l')
acf(local_pred_eu_sales,type = "partial")
acf(local_pred_eu_sales)
armafit_eu <- auto.arima(local_pred_eu_sales)

armafit_eu
##Series: local_pred_eu_sales 
##ARIMA(0,0,0) with zero mean 

##sigma^2 estimated as 97892454:  log likelihood=-445.98
##AIC=893.96   AICc=894.06   BIC=895.7

resi_eu_sales<-local_pred_eu_sales-fitted(armafit_eu)
adf.test(resi_eu_sales,alternative = "stationary")

	##Augmented Dickey-Fuller Test

##data:  resi_eu_sales
##Dickey-Fuller = -6.7179, Lag order = 3, p-value = 0.01
##alternative hypothesis: stationary 
##p-value is less than 0.05 hence it rejects null hypothesis, hence the timeseries is stationary
kpss.test(resi_eu_sales)

##KPSS Test for Level Stationarity

##data:  resi_eu_sales
##KPSS Level = 0.019284, Truncation lag parameter = 1, p-value = 0.1
##p-values is more than 0.05 hence the timeseries is stationary


outdata_eu_sales<-consumer_eu_agg[43:48,]
timevals_eu_sales_out<-outdata_eu_sales$Month_Seq
global_pred_eu_out<-predict(lmfit_3,data.frame(Month=timevals_eu_sales_out))

fcast_eu_sales<-global_pred_eu_out

MAPE_sales_eu<-accuracy(fcast_eu_sales,outdata_eu_sales[,4])[5]
##28.27462

total_tseries_eu_sales<-ts(consumer_eu_agg$Sales)
class_dec_pred_sales_eu <- c(ts(global_pred_eu),ts(global_pred_eu_out))
 plot(total_tseries_apac_qty, col = "black")
 lines(class_dec_pred_qty, col = "pink")


###Now predicting sales using ARIMA model

arima_eu_sales<-auto.arima((timeseries_eu_sales))
arima_eu_sales

##Series: (timeseries_eu_sales) 
##ARIMA(2,1,0) 

##Coefficients:
##          ar1      ar2
##      -0.5796  -0.4906
##s.e.   0.1346   0.1310

##sigma^2 estimated as 168564623:  log likelihood=-445.84
##AIC=897.67   AICc=898.32   BIC=902.81### Arima has high AIC,AICc,BIC values as compared to Classical decomposition

plot(arima_eu_sales$x,col="black")
#Again, let's check if the residual series is white noise
resi_arima_eu_sales<-timeseries_eu_sales-fitted(arima_eu_sales)
##Dickey -Fuller Test 
adf.test(resi_arima_eu_sales,alternative = "stationary")

##Augmented Dickey-Fuller Test

##data:  resi_arima_eu_sales
##Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01 #P<0.05, hence the residual series is stationary
##alternative hypothesis: stationary

kpss.test(resi_arima_eu_sales)

##KPSS Test for Level Stationarity

##data:  resi_arima_eu_sales
##KPSS Level = 0.05314, Truncation lag parameter = 1, p-value = 0.1

fca_arima_eu_sales<-predict(arima_eu_sales,n.ahead=6)
Mape_sales_arima_eu<-accuracy(fca_arima_eu_sales$pred,outdata_eu_sales[,4])[5]
Mape_sales_arima_eu
# 28.9226




##creating ts on quantity
total_tseries_eu_sales<-ts(consumer_eu_agg$Sales)
#finding the predicted value
auto_arima_pred_3<- c(fitted(arima_eu_sales),ts(fca_arima_eu_sales$pred))
plot(total_tseries_eu_sales, col = "black")
#checking the predicted curve agains the original ts
lines(auto_arima_pred_3, col = "red")


###Modelling the Consumer-EUQuantity
###First using Classical Decomposition method
cons_eu_qty_train<-cons_eu_sales_train
##creating the timeseries
timeseries_eu_qty<-ts(cons_eu_qty_train$Quantity)
plot(timeseries_eu_qty)

###Since the TS plot has both trend , smoothening is done using simple Moving avdrage
w <-2
smoothedseries_eu_qty_1 <- stats::filter(timeseries_eu_qty, 
filter=rep(1/(2*w + 1),(2*w + 1)), 
method='convolution', sides=2)

diff <- smoothedseries_eu_qty_1[n-w] - smoothedseries_eu_qty_1[n-w-1]
for (i in seq(n-w+1, n)) {
smoothedseries_eu_qty_1[i] <- smoothedseries_eu_qty_1[i-1] + diff
}

diff <- smoothedseries_eu_qty_1[w+2] - smoothedseries_eu_qty_1[w+1]
for (i in seq(w,1,-1)) {
smoothedseries_eu_qty_1[i] <- smoothedseries_eu_qty_1[i+1] - diff
}

timevals_eu_qty_in<-cons_eu_qty_train$Month_Seq
lines(smoothedseries_eu_qty_1, col="blue", lwd=2)
##converting the tseries into a dataframe
smootheddf_eu_qty<- as.data.frame(cbind(timevals_eu_qty_in, as.vector(smoothedseries_eu_qty_1)))
##assigning column names
colnames(smootheddf_eu_qty) <- c('Month', 'Quantity')
View(smootheddf_eu_qty)
##creating linear regression fit to the sinusodial graph of polinomial 3
lmfit_5 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
+             + Month, data=smootheddf_eu_qty)
global_pred_eu_2 <- predict(lmfit_5, Month=timevals_eu_qty_in)
lines(timevals_eu_qty_in, global_pred_eu_2, col='orange', lwd=2)##the prediction line hugs the regression line
local_pred_eu_qty<-timeseries_eu_qty-global_pred_eu_2###finding out the local wiggles
acf(local_pred_eu_qty)##acf within acceptable range
acf(local_pred_eu_qty,type = "partial")## acf lies withtin the acceptable range
plot(local_pred_eu_qty,col="orange",type='l')

armafit_5<- auto.arima(local_pred_eu_qty)

armafit_5
##Series: local_pred_eu_qty 
##ARIMA(2,0,0) with zero mean 

##Coefficients:
 ##         ar1      ar2
 ##     -0.5833  -0.5739
##s.e.   0.1246   0.1204

##sigma^2 estimated as 8505:  log likelihood=-249.06
##AIC=504.12   AICc=504.75   BIC=509.33

resi_eu_qty<-local_pred_eu_qty-fitted(armafit_5) ## residual sereis
##dickey fuller & kpss test to show the statioanrity of the data
adf.test(resi_eu_qty,alternative = "stationary")

##Augmented Dickey-Fuller Test

##data:  resi_eu_qty
##Dickey-Fuller = -4.9629, Lag order = 3, p-value = 0.01##P<0.05 hence stationary
##alternative hypothesis: stationary

kpss.test(resi_eu_qty)

##	KPSS Test for Level Stationarity

##data:  resi_eu_qty
##KPSS Level = 0.025165, Truncation lag parameter = 1, p-value = 0.1

##evaluate the model on the last 6 months
timevals_eu_qty_out<-timevals_apac_qty_out
global_pred_eu_out_3<-predict(lmfit_5,data.frame(Month=timevals_eu_qty_out))

fcast_eu_qty<-global_pred_eu_out_3
View(outdata_apac_sales)
outdata_eu_qty<-outdata_eu_sales
fcast_eu_qty<-global_pred_eu_out_3
##MAPE to evaluate the model
MAPE_qty_eu<-accuracy(fcast_eu_qty,outdata_eu_qty[,5])[5]
MAPE_qty
#30.49665
#NOT31.39889
###total vs predicted
total_tseries_eu_qty<-ts(consumer_eu_agg$Quantity)
class_dec_pred_qty_eu <- c(ts(global_pred_eu_2),ts(global_pred_eu_out_3))
 plot(total_tseries_apac_qty, col = "black")
 lines(class_dec_pred_qty, col = "yellow")
###Model by Arima
arima_eu_qty<-auto.arima((timeseries_eu_qty))
arima_eu_qty

##Series: (timeseries_eu_qty) 
ARIMA(2,1,0) 

##Coefficients:
#          ar1      ar2
##      -0.7359  -0.5879
##s.e.   0.1224   0.1185

##sigma^2 estimated as 21185:  log likelihood=-261.9
##AIC=529.8   AICc=530.44   BIC=534.94### all the parameters are more than the classical decomposition
plot(arima_eu_qty$x,col="black")
resi_arima_eu_qty<-timeseries_eu_qty-fitted(arima_eu_qty)
##Dickey FUller test
adf.test(resi_arima_eu_qty,alternative = "stationary")
##Augmented Dickey-Fuller Test

##data:  resi_arima_eu_qty
##Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
##alternative hypothesis: stationary
##KPSS test
kpss.test(resi_arima_eu_qty)

	##KPSS Test for Level Stationarity

##data:  resi_arima_eu_qty
##KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1

###from the above formal test we conclude that the model is statioanry

fca_arima_eu_qty<-predict(arima_eu_qty,n.ahead=6)
 Mape_qty_eu<-accuracy(fca_arima_eu_qty$pred,outdata_apac_qty[,5])[5]
 Mape_qty_eu
##37.54059 ### Mape higher than the classical decomposition

##creating ts on quantity
total_tseries_eu_qty<-ts(consumer_eu_agg$Quantity)
#finding the predicted value
auto_arima_pred_eu_1<- c(fitted(arima_eu_qty),ts(fca_arima_eu_qty$pred))
plot(total_tseries_eu_qty, col = "black")
#checking the predicted curve agains the original ts
lines(auto_arima_pred_eu_1, col = "red")

#The classical decomposition method provided a better MAPE paramenter value for all the forecast as compared to the auto ARIMA.





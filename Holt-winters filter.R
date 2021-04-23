#install.packages(c("forecast", "TTR"), dependencies=TRUE,repos="https://cloud.r-project.org")
library("forecast")								# install "forecast" package and load the library	
library("tseries") 

data<-read.csv("sample_data.csv", sep=",",dec=".",header=T) 	# weekly data

names(data)
head(data,5)

sales = data[,3]/1000000		# millions in xyz currency


##### Representing Data as Time Series Objects #####

yy = ts(sales, frequency = 52,start = c(2015,1)) # coverts sales data as time series object with start date and weekly frequency
plot.ts(yy)									# ALWAYS plot time series to see patterns: trend, cycle, variance over time



##### Time Series Decomposition  #####

## Sales: What's the growth rate? How do we obtain seasonally adjusted sales?
sales = decompose(yy) 
sales.trend = sales$trend
sales.seasonal = sales$seasonal
sales.resid = sales$random
sales.season.adj = yy - sales.seasonal	# seasonally adjusted sales
plot.ts(cbind(yy,sales.trend, sales.seasonal, sales.resid, sales.season.adj))


##### Holt-Winters Filter  #####
# combination 1
out1 = HoltWinters(yy, beta = FALSE, gamma = FALSE) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
out1														# output -- see alpha estimate
out1$fitted 												# fitted values in training data
plot(out1)													# graph of actual (black) vs fitted (red)

# combination 2
out2 = HoltWinters(yy, beta = TRUE, gamma = FALSE) 				
out2														# output -- see alpha estimate
out2$fitted 												# fitted values in training data
plot(out2)													# graph of actual (black) vs fitted (red)

# combination 3
out3 = HoltWinters(yy, beta = TRUE, gamma = TRUE) 			
out3													# output -- see alpha estimate
out3$fitted 												# fitted values in training data
plot(out3)													# graph of actual (black) vs fitted (red)

# combination 4
out4 = HoltWinters(yy, beta = FALSE, gamma = TRUE) 			
out4														# output -- see alpha estimate
out4$fitted 												# fitted values in training data
plot(out4)													# graph of actual (black) vs fitted (red)
##### Check Residuals

checkresiduals(out1)
checkresiduals(out2)
checkresiduals(out3)
checkresiduals(out4)

##### Out of Sample Forecasts
out11 = forecast:::forecast.HoltWinters(out1, h = 26, level = c(68, 95))	 
out22 = forecast:::forecast.HoltWinters(out2, h = 26, level = c(68, 95))	
out33 = forecast:::forecast.HoltWinters(out3, h = 26, level = c(68, 95))	
out44 = forecast:::forecast.HoltWinters(out4, h = 26, level = c(68, 95))	



plot(out11)
plot(out22)
plot(out33)
plot(out44)



forecast::accuracy(out11)
forecast::accuracy(out22)
forecast::accuracy(out33)
forecast::accuracy(out44)

# Combination 4, with beta off and gamma on, yields the lowest RMSE and MAE. From the plots, we can
# also see that combination 4 has the narrowest confidence interval, which means it has a more accurate fit.
# Therefore, we choose combination 4, out4 = HoltWinters(yy, beta = FALSE, gamma = TRUE), as our final model.

# ARIMA
sales_auto = auto.arima(yy)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
sales_auto.predict = forecast:::forecast.Arima(sales_auto, h = 26, level = c(68, 95))
plot(sales_auto.predict)
forecast::accuracy(sales_auto.predict)

## Step 1. Is the time series stationary? 

# Use Augmented Dickey-Fuller Test to test stationarity == > large p-value means nonstationary

adf.test(yy)				# if p-value is large (> 0.10), then nonstationary

yd = diff(yy,differences = 1)			
plot.ts(yd)								# looks stationary visually
adf.test(yd)							# estimated p = 0.01 => small p-value (< 0.10) => so yd is stationary ==> fix d = 1 in ARIMA models to be fitted


## Step 2. Decide AR(p) or MA(q) or both ARMA(p,q). Use the stationary series from Step 1. 

# To decide AR(p), plot Pacf. For AR(p) => Pacf becomes zero at some lag p

Pacf(yd, lag.max = 10)					# Pacf suggests p = 1


# To decide MA, plot Acf. For MA(q) => Acf becomes zero at some lag q

Acf(yd, lag.max = 10)				# Acf suggests q = 1

## Step 3. Fit several ARIMA models. 	

m1 = Arima(yy, order=c(1,1,0))			# note: differencing (d = 1) is specified in the "order"; so fit the original yy series (yy, not yd)
m1				# see the output of m1. The estimated phi value and its std err to assess significnace
summary(m1)		# see Accuracy using MAPE = Mean Absolute Percentage Error 

m2 = Arima(yy, order=c(2,1,0))
m2
summary(m2)	

m3 = Arima(yy, order=c(1,1,1))	
m3
summary(m3)	

m4 = Arima(yy, order=c(2,1,1))	
m4
summary(m4)	

# Consider Seasonal ARIMA(p,d,q) x (P, D, Q) components when seasonality is expected/suspected
m5 = Arima(yy, order=c(1,1,0), seasonal = list(order = c(0,0,1), period = 52))
summary(m5)

m6 = Arima(yy, order=c(2,1,0), seasonal = list(order = c(0,1,0), period = 52))
summary(m6)
m6.predict = forecast:::forecast.Arima(m6, h = 26, level = c(68, 95))
plot(m6.predict)


#m6 has the least MAPE with stationarity.


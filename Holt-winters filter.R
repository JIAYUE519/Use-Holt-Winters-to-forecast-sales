#install.packages(c("forecast", "TTR"), dependencies=TRUE,repos="https://cloud.r-project.org")
library("forecast")								# install "forecast" package and load the library	
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




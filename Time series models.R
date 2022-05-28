library("xts")
library('Metrics')
library('sarima')
library('dplyr')
library('forecast')
library('ggplot2')
library('fUnitRoots')
library('data.table')
library(fread)
library(vars)
df <- read.csv('Downloads/yellow_tripdata_2021-01.csv')

#drop improvement_surcharge and mta_tax columns
df <- subset(df, select = -c(improvement_surcharge, mta_tax))

summary(df)

df <- na.omit(df)

#Money related columns at the end made positive
a<-names(df)[11:16]
for (i in a){
  df[i] <- abs(df[i]) 
}

timetounix <- function(t) {
  as.numeric(strptime(t, "%Y-%m-%d %H:%M:%S")) 
}

pickup_time<-timetounix(df$tpep_pickup_datetime)
dropoff_time<-timetounix(df$tpep_dropoff_datetime)
#trip duration in minutes
df$trip_duration<-(dropoff_time - pickup_time) / 60
#speed in miles per hour
df$speed<-(df$trip_distance / df$trip_duration) * 60

boxplot(df$trip_duration)
boxplot(df$trip_distance)

#trip duration negative is error
df<-df[!df$trip_duration<0, ]

#trip duration can't be zero
df<-df[!df$trip_duration==0, ]

#trip distance can't be zero
df<-df[!df$trip_distance==0, ]

summary(df)

#anomalously high trip distance in 22 minutes 
df<-df[!df$trip_distance==114328.2, ]

#speed less than 100
df<-df[!df$speed>100, ]

df1 <- data.frame(time = substr(df$tpep_pickup_datetime,1,10)) #slicing the columns to extract dates
df2=df1 %>%              #grouping by date to get number of pickups
  count(time)

df2=df2[3:33,]           #processing
colnames(df2)[2] <- "pickups"     #renaming columns

df2$time <- as.Date(df2$time)    #converting it to datetime
data_ts <- xts(df2$pickups, df2$time)        # Convert data frame to time series format
colnames(data_ts)[1] <- "pickups"         #renaming the columns
adfTest(data_ts) #carryng out adf test
data_ts_s=diff(data_ts$pickups,lag=1)
adfTest(data_ts_s) #carryng out adf test
tsdisplay(data_ts_s)
train_data=data_ts[1:25,]    #splitting into train and test
fit <- arima(train_data, order=c(5,1,7)) #creating arima model
autoplot(forecast(fit,6))  #forecast
dataframe_val=fortify(forecast(fit,6)$mean,ts.connect=TRUE) #converting forecasted data into dataframe
test_data=data_ts[26:31,]  #extracting test data
test_data$predicted_pickups=dataframe_val$y  #adding new columns


#plotting actual vs predicted
plot(test_data$pickups,type = "l", col ='blue',ylim=c(20000,70000))
lines(test_data$predicted_pickups, type = "l", col ='red')
legend('bottomleft',legend=c('Actual','Predicted'),col=c('blue','red'))
#r squared and rmse for arima model
rmse(test_data$pickups,test_data$predicted_pickups)


#sarima
first_diff=diff(train_data, differences = 1)
tsdisplay(first_diff)
auto.arima(data_ts,trace=TRUE,stepwise=FALSE,approximation=FALSE)
best_fit <- arima(train_data, order=c(2,0,1))
autoplot(forecast(fit,6))  #forecast
dataframe_val1=fortify(forecast(fit,6)$mean,ts.connect=TRUE) 
df <- fread('ml_models.csv')

#new data
df3 <- data.frame(time = substr(df$tpep_pickup_datetime,1,13)) #slicing the columns to extract dates
df4=df3 %>%              #grouping by date to get number of pickups
  count(time)
df5=df4
df5$temp=':00:00'
df4$time=paste(df4$time,df5$temp,sep='')
df4=df4[6:749,]           #processing
colnames(df4)[2] <- "pickups"     #renaming columns
df4$time=strptime(df4$time,"%Y-%m-%d %H:%M:%S")


data_ts1 <- xts(df4$pickups, df4$time)        # Convert data frame to time series format
colnames(data_ts1)[1] <- "pickups"  
adfTest(data_ts1)
tsdisplay(data_ts1)
train_data_h=data_ts1[1:593,]    #splitting into train and test
fit_h <- arima(train_data_h, order=c(25,0,25)) #creating arima model
autoplot(forecast(fit_h,156))  #forecast
dataframe_val_h=fortify(forecast(fit_h,156)$mean,ts.connect=TRUE) #converting forecasted data into dataframe
test_data_h=data_ts1[594:749,]  #extracting test data
test_data_h$predicted_pickups=dataframe_val_h$y  #adding new columns
rmse(test_data_h$pickups,test_data_h$predicted_pickups)

plot(test_data_h$pickups,type = "l", col ='blue')
lines(test_data_h$predicted_pickups, type = "l", col ='red')

#Rolling forecast giving similar results
data_new=copy(data_ts1)

for (j in 593:740){
  #train_data = data_1[:train_end-timedelta(days=1)]
  rolling <- arima(data_new[1:j], order=c(6,0,1)) #creating arima model
  data_new[j+1]=fortify(forecast(rolling,1)$mean,ts.connect=TRUE)$y
}
predicted_rolling_origin=data_new[594:741,]

plot(test_data_h$pickups,type = "l", col ='blue')
lines(predicted_rolling_origin$pickups, type = "l", col ='red')

#resampling part

arr=c('tpep_pickup_datetime','passenger_count','trip_distance','payment_type','fare_amount','extra','tip_amount','tolls_amount','total_amount','congestion_surcharge','trip_duration','speed')
data_var=copy(df[arr])
data_var$tpep_pickup_datetime <- as.POSIXct(data_var$tpep_pickup_datetime,tz=Sys.timezone())
temp=copy(data_var[-c(1)])

data_new=xts(temp,data_var$tpep_pickup_datetime)
resampled_data <- aggregate(data_new, format(time(data_new),"%y-%m-%d %H"), mean)

tt=copy(df[c('PULocationID')])
tt$tpep_pickup_datetime <- as.POSIXct(df$tpep_pickup_datetime,tz=Sys.timezone())
dfdf <- data.frame(time = substr(df$tpep_pickup_datetime,1,13))
dfdf$PULocationID=df$PULocationID
data_location=xts(copy(tt[-c(2)]),tt$tpep_pickup_datetime)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
zz=aggregate(PULocationID ~ time,dfdf, Mode)
zz=zz[6:749,]

tt1=copy(df[c('DOLocationID')])
tt1$tpep_pickup_datetime <- as.POSIXct(df$tpep_pickup_datetime,tz=Sys.timezone())
dfdf1 <- data.frame(time = substr(df$tpep_pickup_datetime,1,13))
dfdf1$DOLocationID=df$DOLocationID
data_location1=xts(copy(tt1[-c(2)]),tt1$tpep_pickup_datetime)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
zz1=aggregate(DOLocationID ~ time,dfdf1, Mode)
zz1=zz1[6:749,]
resampled_data=resampled_data[6:749,]
arr1=c('passenger_count','trip_distance','payment_type','fare_amount','extra','tip_amount','tolls_amount','total_amount','congestion_surcharge','trip_duration','speed')
data_ts1$passenger_count=resampled_data$passenger_count
data_ts1$trip_distance=resampled_data$trip_distance
data_ts1$payment_type=resampled_data$payment_type
data_ts1$fare_amount=resampled_data$fare_amount
data_ts1$extra=resampled_data$extra
data_ts1$tip_amount=resampled_data$tip_amount
data_ts1$tolls_amount=resampled_data$tolls_amount
data_ts1$total_amount=resampled_data$total_amount
data_ts1$congestion_surcharge=resampled_data$congestion_surcharge
data_ts1$trip_duration=resampled_data$trip_duration
data_ts1$speed=resampled_data$speed
data_ts1$PULocationID=zz$PULocationID
data_ts1$DOLocationID=zz1$DOLocationID

#data_ts1 is our final resampled data

#plotting the time series graph for all columns in data_ts1
plot(data_ts1$passenger_count)
plot(data_ts1$trip_distance)
plot(data_ts1$payment_type)
plot(data_ts1$fare_amount)
plot(data_ts1$extra)
plot(data_ts1$tip_amount)
plot(data_ts1$tolls_amount)
plot(data_ts1$total_amount)
plot(data_ts1$congestion_surcharge)
plot(data_ts1$trip_duration)
plot(data_ts1$speed)
plot(data_ts1$PULocationID)
plot(data_ts1$DOLocationID)

#checking if all the columns are stationary
pp.test(data_ts1$passenger_count)
pp.test(data_ts1$trip_distance)
pp.test(data_ts1$payment_type)
pp.test(data_ts1$fare_amount)
pp.test(data_ts1$extra)
pp.test(data_ts1$tip_amount)
pp.test(data_ts1$tolls_amount)
pp.test(data_ts1$total_amount)
pp.test(data_ts1$congestion_surcharge)
pp.test(data_ts1$trip_duration)
pp.test(data_ts1$speed)
pp.test(data_ts1$PULocationID)
pp.test(data_ts1$DOLocationID)

#checking which lag gives the best result
lagselect <- VARselect(data_ts1, lag.max = 24, type = "const")
lagselect$selection

#splitting data into train and test
train_data_var=data_ts1[1:600,]
test_data_h=data_ts1[601:744,]

#training the VAR model with the training data
Model1 <- VAR(train_data_var, p = 8, type = "const", season = NULL, exog = NULL)
summary(Model1)

Serial1 <- serial.test(Model1, lags.pt = 8, type = "PT.asymptotic")
Serial1

#checking if any column in the data is correlated with other columns in the data using Granger causality test
Granger_passenger_count<- causality(Model1, cause = "passenger_count")
Granger_passenger_count

Granger_trip_distance<- causality(Model1, cause = "trip_distance")
Granger_trip_distance

Granger_payment_type<- causality(Model1, cause = "payment_type")
Granger_payment_type

Granger_payment_type<- causality(Model1, cause = "payment_type")
Granger_payment_type

Granger_fare_amount<- causality(Model1, cause = "fare_amount")
Granger_fare_amount

Granger_extra<- causality(Model1, cause = "extra")
Granger_extra

Granger_tip_amount<- causality(Model1, cause = "tip_amount")
Granger_tip_amount

Granger_tolls_amount<- causality(Model1, cause = "tolls_amount")
Granger_tolls_amount

Granger_total_amount<- causality(Model1, cause = "total_amount")
Granger_total_amount

Granger_congestion_surcharge<- causality(Model1, cause = "congestion_surcharge")
Granger_congestion_surcharge

Granger_trip_duration<- causality(Model1, cause = "trip_duration")
Granger_trip_duration

Granger_speed<- causality(Model1, cause = "speed")
Granger_speed

Granger_PULocationID<- causality(Model1, cause = "PULocationID")
Granger_PULocationID

Granger_DOLocationID<- causality(Model1, cause = "DOLocationID")
Granger_DOLocationID

#forecasting the time series for 156 data points in the future
forecast <- predict(Model1, n.ahead = 156, ci = 0.95)
fanchart(forecast, names = "passenger_count", main = "Fanchart for passenger count", xlab = "Time", ylab = "passenger count")

train = taxi_samp[1:600,]
test = taxi_samp[601:744,]

train_x = subset(train, select=-c(number_pickups))
train_y = train[, "number_pickups", with = FALSE]

test_x = subset(test, select=-c(number_pickups))
test_y = test[, "number_pickups", with=FALSE]


Model1 <- VAR(train, p = 8, type = "const", season = NULL, exog = NULL)
forecast <- predict(Model1, n.ahead = 1, ci = 0.95)
plot(test_y, type = "l", col ='blue', xlab = 'Day', ylab = 'Number of pickups')
lines(forecast, type = "l", col ='red')
legend('topright',legend=c('Actual','Predicted'),col=c('blue','red'))

#arima
fit_h <- arima(train_data_var, order=c(25,0,25)) #creating arima model
dataframe_val_h=fortify(forecast(fit_h,144)$mean,ts.connect=TRUE) #converting forecasted data into dataframe
test_data_h=data_ts1[600:744,]  #extracting test data
test_data_h$predicted_pickups=dataframe_val_h$y  #adding new columns
plot(test_data_h$pickups,type = "l", col ='blue', xlab = 'Day', ylab = 'Number of pickups')
lines(test_data_h$predicted_pickups, type = "l", col ='red')

#arimax
ddlj=copy(train_data_var[,c(2,3,4,5,6,7,8,9,10,11,12,13,14)])
pickup_col=copy(train_data_var[,c(1)])
model_with_var= auto.arima(pickup_col, xreg=ddlj)
ddlj_future=copy(test_data_var[,c(2,3,4,5,6,7,8,9,10,11,12,13,14)])
fcast <- model_with_var%>%
  forecast(xreg = ddlj_future) 
autoplot(fcast) + 
  xlab("Year") +
  ylab("Pickups")
dataframe_val_htt=fortify(fcast$mean,ts.connect=TRUE)
dataframe_val_htt=dataframe_val_htt[-c(1)]
test_data_var$arimaxpickups=dataframe_val_htt$Data
plot(test_data_var$arimaxpickups,type='l',col='red')
lines(test_data_var$pickups, type = "l", col ='green')

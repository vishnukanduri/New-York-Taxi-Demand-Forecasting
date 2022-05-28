library(plotly)
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)
library(cluster)
library(GGally)
library(randomForest)
library(xgboost)
library(keras)
library(monmlp)
library(Metrics)

df <- fread('yellow_tripdata_2021-01.csv')

#drop improvement_surcharge and mta_tax columns
df <- subset(df, select = -c(improvement_surcharge, mta_tax))

summary(df)

df <- na.omit(df)

#Money related columns at the end made positive
df[,11:16] = abs(df[,11:16])

timetounix <- function(t) {
  as.numeric(strptime(t, "%Y-%m-%d %H:%M:%S")) 
}

pickup_time<-timetounix(df$tpep_pickup_datetime)
dropoff_time<-timetounix(df$tpep_dropoff_datetime)
#trip duration in hour
df$trip_duration<-(dropoff_time - pickup_time) / 3600
#speed in miles per hour
df$speed<-(df$trip_distance / df$trip_duration)

boxplot(df$trip_duration)
boxplot(df$trip_distance)

#trip duration negative is error
df<-df[!df$trip_duration<0, ]

#trip duration can't be zero
df<-df[!df$trip_duration==0, ]

#trip distance can't be zero
df<-df[!df$trip_distance==0, ]

#anomalously high trip distance in 22 minutes 
df<-df[!df$trip_distance==114328.2, ]

#speed less than 100 miles/hr
df<-df[!df$speed>100, ]

#distance less than 130 miles
df<-df[!df$trip_distance>130, ]

#time less than 24hrs
df<-df[!df$trip_duration>24, ]

#total amount less than 1000
df<-df[!df$total_amount>420, ]

# Remove category that is an outlier
df = filter(df, RatecodeID != 99)

# convert to categorical
df[,c("store_and_fwd_flag", "RatecodeID", "payment_type")] <- lapply(df[,c("store_and_fwd_flag", "RatecodeID", "payment_type")], factor)

summary(df)

setwd('C:/MY STUFF/UB/Spring 22/EAS 509 - SDM/Project/')
taxi = fread('processed.csv')

summary(taxi)

# Visualizing distribution using box plot
plot_ly(taxi, y = ~trip_duration, type = "box", name = "") %>%
  layout(title = "Distribution of Trip duration")
plot_ly(taxi, y = ~speed, type = "box", name = "") %>%
  layout(title = "Distribution of Speed")
plot_ly(taxi, y = ~trip_distance, type = "box", name = "") %>%
  layout(title = "Distribution of Trip Distance")
plot_ly(taxi, y = ~total_amount, type = "box", name = "") %>%
  layout(title = "Distribution of Total amount")

plot_ly(taxi, y = ~trip_duration, color=~RatecodeID, type = "box") %>%
  layout(title = "Distribution of Trip duration based on RatecodeID", legend=list(title=list(text='RatecodeID')))
plot_ly(taxi, y = ~speed, color=~RatecodeID, type = "box") %>%
  layout(title = "Distribution of Speed based on RatecodeID", legend=list(title=list(text='RatecodeID')))
plot_ly(taxi, y = ~trip_distance, color=~RatecodeID, type = "box") %>%
  layout(title = "Distribution of Trip Distance based on RatecodeID", legend=list(title=list(text='RatecodeID')))
plot_ly(taxi, y = ~total_amount, color=~RatecodeID, type = "box") %>%
  layout(title = "Distribution of Total amount based on RatecodeID", legend=list(title=list(text='RatecodeID')))

plot_ly(taxi, y = ~trip_duration, color=~payment_type, type = "box") %>%
  layout(title = "Distribution of Trip duration based on PaymentType", legend=list(title=list(text='PaymentType')))
plot_ly(taxi, y = ~speed, color=~payment_type, type = "box") %>%
  layout(title = "Distribution of Speed based on PaymentType", legend=list(title=list(text='PaymentType')))
plot_ly(taxi, y = ~trip_distance, color=~payment_type, type = "box") %>%
  layout(title = "Distribution of Trip Distance based on PaymentType", legend=list(title=list(text='PaymentType')))
plot_ly(taxi, y = ~total_amount, color=~payment_type, type = "box") %>%
  layout(title = "Distribution of Total amount based on PaymentType", legend=list(title=list(text='PaymentType')))

plot_ly(taxi, y = ~trip_duration, color=~store_and_fwd_flag, type = "box") %>%
  layout(title = "Distribution of Trip duration based on store_and_fwd_flag", legend=list(title=list(text='store_and_fwd_flag')))
plot_ly(taxi, y = ~speed, color=~store_and_fwd_flag, type = "box")  %>%
  layout(title = "Distribution of Speed based on store_and_fwd_flag", legend=list(title=list(text='store_and_fwd_flag')))
plot_ly(taxi, y = ~trip_distance, color=~store_and_fwd_flag, type = "box")  %>%
  layout(title = "Distribution of Trip Distance based on store_and_fwd_flag", legend=list(title=list(text='store_and_fwd_flag')))
plot_ly(taxi, y = ~total_amount, color=~store_and_fwd_flag, type = "box")  %>%
  layout(title = "Distribution of Total amount based on store_and_fwd_flag", legend=list(title=list(text='store_and_fwd_flag')))

# Scatter plot to find the relation among features
ggplot(taxi,aes(x=trip_distance,y=total_amount)) + geom_point() + ggtitle("Scatter Plot of trip distance vs total amount") +
  xlab("Trip Distance (miles)") + ylab("Total Amount (Dollars)")
ggplot(taxi,aes(x=trip_distance,y=total_amount, color=RatecodeID)) + geom_point()  + ggtitle("Scatter Plot of trip distance vs total amount") +
  xlab("Trip Distance (miles)") + ylab("Total Amount (Dollars)") + labs(fill = "RatecodeID")
ggplot(taxi,aes(x=trip_distance,y=total_amount, color=payment_type)) + geom_point()  + ggtitle("Scatter Plot of trip distance vs total amount") +
  xlab("Trip Distance (miles)") + ylab("Total Amount (Dollars)") + labs(fill = "payment_type")
ggplot(taxi,aes(x=trip_distance,y=total_amount, color=store_and_fwd_flag)) + geom_point()  + ggtitle("Scatter Plot of trip distance vs total amount") +
  xlab("Trip Distance (miles)") + ylab("Total Amount (Dollars)") + labs(fill = "store_and_fwd_flag")



# heatmap plot for plotting correlations
taxi$store_and_fwd_flag = ifelse(taxi$store_and_fwd_flag=="Y",1,0)
numeric_cols = names(taxi[,-c("tpep_pickup_datetime", "tpep_dropoff_datetime", "RatecodeID",
                              "payment_type")])
plot_ly(
  x = numeric_cols, y = numeric_cols, 
  z = cor(taxi[,-c("tpep_pickup_datetime", "tpep_dropoff_datetime", "RatecodeID",
                   "payment_type")]), type = "heatmap", colors = colorRamp(c("white", "blue")))  %>%
  layout(title = "Heatmap plot of the correlations among the features")


# Cluster data based on pickup demand

coord = taxi[,c("trip_distance")]

set.seed(10)

WCSS = vector()
for (i in 1:10){
  WCSS[i] = sum(kmeans(x=coord, centers=i)$withinss)
}

plot(x=1:10, y=WCSS, type="b", 
     main="Elbow Graph for Optimal Number of Clusters", 
     xlab="Number of Clusters", ylab="WCSS")

pickup_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
taxi$cluster_pickup = as.factor(pickup_clust$cluster)

ggplot(taxi,aes(x=trip_distance,y=fare_amount, color=cluster_pickup)) + geom_point() + ggtitle("Scatter Plot of trip distance vs total amount") +
  xlab("Trip Distance (miles)") + ylab("Total Amount (Dollars)") + labs(fill = "Cluster Group")

taxi = taxi[order(tpep_pickup_datetime)]


# Remove the pickups and dropoffs not in year 2021
taxi = taxi[(format(taxi$tpep_pickup_datetime,'%Y') == "2021" ) & 
              (format(taxi$tpep_dropoff_datetime,'%Y') == "2021"), ]

taxi = taxi[(format(taxi$tpep_pickup_datetime,'%m') == "01" ) | 
              (format(taxi$tpep_dropoff_datetime,'%m') == "01"), ]
write.csv(taxi, 'peak-off-peak.csv')

taxi = subset(taxi, select = -c(VendorID))

# Aggregate on hourly basis
taxi_samp = taxi %>%
  mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
         pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
  group_by(pickup_date) %>%
  summarize(total_amount = mean(total_amount), fare_amount = mean(fare_amount),
            extra = mean(extra), tip_amount = mean(tip_amount),
            passenger_count = sum(passenger_count), tolls_amount = mean(tolls_amount),
            congestion_surcharge = mean(congestion_surcharge), 
            trip_distance = mean(trip_distance), 
            trip_duration = mean(trip_duration), 
            speed = sum(speed),
            RatecodeID = 
              unique(RatecodeID)[which.max(tabulate(match(RatecodeID, unique(RatecodeID))))],
            PULocationID = 
              unique(PULocationID)[which.max(tabulate(match(PULocationID, unique(PULocationID))))],
            DOLocationID = 
              unique(DOLocationID)[which.max(tabulate(match(DOLocationID, unique(DOLocationID))))],
            payment_type = 
              unique(payment_type)[which.max(tabulate(match(payment_type, unique(payment_type))))],
            store_and_fwd_flag = 
              unique(store_and_fwd_flag)[which.max(tabulate(match(store_and_fwd_flag, unique(store_and_fwd_flag))))],
            number_pickups = n()
  )


summary(taxi_samp)

# Removing redundant columns

taxi_samp = subset(taxi_samp, select = -c(store_and_fwd_flag, RatecodeID, 
                                          pickup_date))
write.csv(taxi_samp, 'ml_models.csv')
# Split dataset into training and test data

taxi_samp = fread('ml_models.csv')

train = taxi_samp[1:600,]
test = taxi_samp[601:744,]

train_x = subset(train, select=-c(number_pickups))
train_y = train[, "number_pickups", with = FALSE]

test_x = subset(test, select=-c(number_pickups))
test_y = test[, "number_pickups", with=FALSE]

set.seed(123)
# Random Forest Regression
ntrees = seq(10, 500, by=10)
mtry = seq(11, 60, by=3)

rt_df = data.frame(matrix(ncol = 4, nrow = 0))
colnames(rt_df) = c("n_tree", "m_try", "RMSE", "R-Squared")

for (ntree in ntrees){
  for (m in mtry){
    rt.fit = randomForest(train_y$number_pickups~., data=train_x,
                          ntree=ntree, importance = T, mtry=m)
    
    rt.predict = predict(rt.fit, test_x)
    
    rt.rmse = rmse(test_y$number_pickups, rt.predict)
    rss <- sum((test_y$number_pickups - rt.predict) ^ 2)  ## residual sum of squares
    tss <- sum((test_y$number_pickups - mean(test_y$number_pickups)) ^ 2)  ## total sum of squares
    rt.rsq <- 1 - rss/tss
    
    rt_df[nrow(rt_df) + 1,] = c(ntree, m, rt.rmse, rt.rsq)
    print(c(ntree, m, rt.rmse, rt.rsq))
  }
}

# minimum rmse hyper parameter
min_rt = rt_df[which.min(rt_df$RMSE),]

#rshiny rf
rt.fit = randomForest(train_y$number_pickups~., data=train_x,
                      ntree=20, importance = T, mtry=9)
predict(rt.fit, train_x[2,])
plot(test_y$number_pickups,type = "l", col ='blue', xlab = 'Day', ylab = 'Number of pickups')
lines(rt.predict, type = "l", col ='red')
legend('topright',legend=c('Actual','Predicted'),col=c('blue','red'))

set.seed(123)

# XGBoost
depths = seq(5, 25, by=1)
learning_rates = seq(0.05, 1, by=0.05)
xg_df = data.frame(matrix(ncol = 4, nrow = 0))
colnames(xg_df) = c("depth", "la", "RMSE", "R-Squared")

for (depth in depths){
  for (la in learning_rates){
    xg.fit = xgboost(
      data = data.matrix(train_x),
      label = data.matrix(train_y),
      nrounds = 1000,
      objective = "reg:squarederror",
      max_depth = depth,
      eta = la
    )
    
    xg.predict = predict(xg.fit, data.matrix(test_x))
    
    xg.rmse = rmse(test_y$number_pickups, xg.predict)
    rss <- sum((test_y$number_pickups - xg.predict) ^ 2)  ## residual sum of squares
    tss <- sum((test_y$number_pickups - mean(test_y$number_pickups)) ^ 2)  ## total sum of squares
    xg.rsq <- 1 - rss/tss
    
    xg_df[nrow(xg_df) + 1,] = c(depth, la, xg.rmse, xg.rsq)
    
    
  }
}

min_xg = xg_df[which.min(xg_df$RMSE),]

#rshiny xg
xg.fit = xgboost(
  data = data.matrix(train_x),
  label = data.matrix(train_y),
  nrounds = 1000,
  objective = "reg:squarederror",
  max_depth = 9,
  eta = 0.15
)

xg.predict = predict(xg.fit, data.matrix(test_x))
plot(test_y$number_pickups,type = "l", col ='blue', xlab = 'Day', ylab = 'Number of pickups')
lines(xg.predict, type = "l", col ='red')
legend('topright',legend=c('Actual','Predicted'),col=c('blue','red'))

set.seed(123)

# MLP
mlp_df = data.frame(matrix(ncol = 4, nrow = 0))
colnames(mlp_df) = c("hidden1", "hidden2", "RMSE", "R-Squared")

hidden1 = seq(1, 5, by=1)
hidden2 = seq(1, 5, by=1)

for (h1 in hidden1){
  for (h2 in hidden2){
    mlp.fit <- monmlp.fit(x = data.matrix(train_x), y = data.matrix(train_y), hidden1 = 2, bag = TRUE, 
                          iter.max = 500, iter.stopped = 10)
    
    mlp.predict = monmlp.predict(weights=mlp.fit, x=data.matrix(test_x))
    
    mlp.rmse = rmse(test_y$number_pickups, mlp.predict)
    
    rss <- sum((test_y$number_pickups - mlp.predict) ^ 2)  ## residual sum of squares
    tss <- sum((test_y$number_pickups - mean(test_y$number_pickups)) ^ 2)  ## total sum of squares
    mlp.rsq <- 1 - rss/tss
    
    mlp_df[nrow(mlp_df) + 1,] = c(h1, h2, mlp.rmse, mlp.rsq)
    
  }
}

#rshiny mlp
mlp.fit <- monmlp.fit(x = data.matrix(train_x), y = data.matrix(train_y), hidden1 = 3, bag = TRUE, 
                      iter.max = 1, iter.stopped = 10, hidden2 = 5)

monmlp.predict(weights=mlp.fit, x=data.matrix(test_x))
plot(test_y$number_pickups,type = "l", col ='blue', xlab = 'Day', ylab = 'Number of pickups')
lines(mlp.predict[1], type = "l", col ='red')
legend('topright',legend=c('Actual','Predicted'),col=c('blue','red'))



train_x


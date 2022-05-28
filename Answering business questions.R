#Activating necessary packages
library(plotly)
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)
library(cluster)
library(GGally)
library(TSA)
library(tseries)

#importing the dataset
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

#boxplot(df$trip_duration)
#boxplot(df$trip_distance)

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

taxi = fread('peak_hours.csv')
summary(taxi)

#Filtering month and year to include only Jan 2021
taxi = taxi[(format(taxi$tpep_pickup_datetime,'%Y') == "2021" ) & 
              (format(taxi$tpep_dropoff_datetime,'%Y') == "2021"), ]

taxi = taxi[(format(taxi$tpep_pickup_datetime,'%m') == "01" ) | 
              (format(taxi$tpep_dropoff_datetime,'%m') == "01"), ]

#Finding Peak hour and Off-peak hour for Pick-up location 48
pu_loc = 48
taxi_samp = taxi %>%
  mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
         pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
  group_by(pickup_date) %>%
  summarise(total_trips=sum(PULocationID == pu_loc))

max_trips = max(taxi_samp$total_trips)
min_trips = min(taxi_samp$total_trips)

#Peak hour
taxi_samp[taxi_samp$total_trips == max_trips,]

#Off-peak hour
taxi_samp[taxi_samp$total_trips == min_trips,]


# Cluster data based on trip distance to understand the difference between short and long distance taxi trips
coord = taxi[,c("trip_distance")]

set.seed(1)

WCSS = vector()
for (i in 1:10){
  WCSS[i] = sum(kmeans(x=coord, centers=i)$withinss)
}

plot(x=1:10, y=WCSS, type="b", 
     main="Elbow Graph for Optimal Number of Clusters", 
     xlab="Number of Clusters", ylab="WCSS")

distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
taxi$cluster_distance = as.factor(distance_clust$cluster)

ggplot(taxi,aes(x=trip_distance,y=total_amount, color=cluster_distance)) + geom_point() + ggtitle("Scatter Plot of trip distance vs total amount") +
  xlab("Trip Distance (miles)") + ylab("Total Amount (dollars)") + labs(fill = "Cluster Group")

#top 10 pickup locations with highest number of short distance trips
clus_1 = taxi[taxi$cluster_distance == 1,]
df1 = data.frame(table(clus_1$PULocationID))
colnames(df1) <- c('PULocationID','Count')
df1 = df1[order(-df1$Count),]
head(df1, 10)

#top 10 pickup locations with highest number of long distance trips
clus_2 = taxi[taxi$cluster_distance == 2,]
df2 = data.frame(table(clus_2$PULocationID))
colnames(df2) <- c('PULocationID','Count')
df2 = df2[order(-df2$Count),]
head(df2, 10)

#maximum trip distance
max(clus_1$trip_distance)
max(clus_2$trip_distance)

max(clus_1$trip_distance)
min(clus_1$trip_distance)
max(clus_2$trip_distance)
min(clus_2$trip_distance)

#average tip amount for short distance trips
mean(clus_1$tip_amount)

#average tip amount for long distance trips
mean(clus_2$tip_amount)

#popular payment type among short distance trip takers
payment_type_clus_1 = data.frame(table(clus_1$payment_type))
colnames(payment_type_clus_1) <- c('Payment Type','Count')
payment_type_clus_1 = payment_type_clus_1[order(-payment_type_clus_1$Count),]
head(payment_type_clus_1, 1)

#popular payment type among long distance trip takers
payment_type_clus_2 = data.frame(table(clus_2$payment_type))
colnames(payment_type_clus_2) <- c('Payment Type','Count')
payment_type_clus_2 = payment_type_clus_2[order(-payment_type_clus_2$Count),]
head(payment_type_clus_2, 1)

#comparing passenger count for short and long distance trips
short_dist_passenger_count = sum(clus_1$passenger_count)
short_dist_passenger_count
long_dist_passenger_count = sum(clus_2$passenger_count)
long_dist_passenger_count
diff_in_passenger_count = short_dist_passenger_count - long_dist_passenger_count
diff_in_passenger_count

#GARCH
taxi = taxi[order(tpep_pickup_datetime)]


# Remove the pickups and dropoffs not in year 2021
taxi = taxi[(format(taxi$tpep_pickup_datetime,'%Y') == "2021" ) & 
              (format(taxi$tpep_dropoff_datetime,'%Y') == "2021"), ]

taxi = taxi[(format(taxi$tpep_pickup_datetime,'%m') == "01" ) | 
              (format(taxi$tpep_dropoff_datetime,'%m') == "01"), ]


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

x = taxi_samp$pickup_date

# Removing redundant columns
taxi_samp = subset(taxi_samp, select = -c(store_and_fwd_flag, RatecodeID, 
                                          pickup_date, payment_type))

summary(taxi_samp)

#covert to time series
# taxi_samp = ts(taxi_samp,frequency = 24)

# Split dataset into training and test data
train = taxi_samp[1:600,]
test = taxi_samp[601:744,]

train_x = subset(train, select=-c(number_pickups))
train_y = train["number_pickups"]

test_x = subset(test, select=-c(number_pickups))
test_y = test["number_pickups"]

taxi_samp = as.numeric(taxi_samp$number_pickups)
n_pickups = diff(log(taxi_samp))
garch(n_pickups, order = c(1, 1), series = NULL)

plot(n_pickups,type = "l", col ='blue', xlab = 'Day', ylab = 'Number of pickups')
lines(predict(n_pickups), type = "l", col ='red')
legend('bottomleft',legend=c('Actual','Predicted'),col=c('red','blue'),fill=c('red','blue'))


plot(predict(n_pickups),                               
     n_pickups,
     xlab = "Predicted Values",
     ylab = "Observed Values")

#Computing fourier transform
p = periodogram(taxi$trip_duration)
p

dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)

#display the 2 highest "power" frequencies
top2

#convert frequency to time periods
time = 1/top2$f
time

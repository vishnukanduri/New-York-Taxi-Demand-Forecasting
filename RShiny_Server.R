#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)
library(cluster)
library(GGally)
library(randomForest)
library(xgboost)
#library(keras)
library(monmlp)
library(Metrics)
library(caret)
library(vars)
library(base)
library(forecast)
library(xts)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    taxi_samp <- fread('processed.csv')
    train = taxi_samp[1:600,]
    test = taxi_samp[601:744,]
    
    train_x = subset(train, select=-c(number_pickups))
    train_y = train[, "number_pickups", with=FALSE]
    
    test_x = subset(test, select=-c(number_pickups))
    test_y = test[, "number_pickups", with=FALSE]
    
    data = fread('resampled.csv')
    data_xts<- xts(data[,c(-1)], data$V1)
    train_data_xts = data_xts[1:600,]
    test_data_xts = data_xts[601:744,]
    # Model_1 RF
    rt.fit = randomForest(train_y$number_pickups~., data=train_x,
                          ntree=20, importance = T, mtry=9)
    rt.predict = predict(rt.fit, test_x)
    
    # Model_2 XGB
    
    xg.fit = xgboost(
        data = data.matrix(train_x),
        label = data.matrix(train_y),
        nrounds = 1000,
        objective = "reg:squarederror",
        max_depth = 9,
        eta = 0.15
    )
    
    xg.predict = predict(xg.fit, data.matrix(test_x))
    #plot(test_y$number_pickups,type = "l", col ='blue', xlab = 'Day', ylab = 'Number of pickups')
    #lines(xg.predict, type = "l", col ='red')
    #legend('topright',legend=c('Actual','Predicted'),col=c('blue','red'))
    
    # Model_3 ml
    mlp.fit = monmlp.fit(x = data.matrix(train_x), y = data.matrix(train_y), hidden1 = 3, bag = TRUE,
                         iter.max = 1, iter.stopped = 10, hidden2 = 5)
    
    mlp.predict = monmlp.predict(weights=mlp.fit, x=data.matrix(test_x))
    
    # Model_4 VAR
    
    Model4 <- VAR(train_data_xts, p = 8, type = "const", season = NULL, exog = NULL)
    fc = predict(Model4, n.ahead = 144, ci = 0.95)
    tt=as.data.frame(fc$fcst[1])
    
    dataframe_arima=as.data.frame(train_data_xts[,c(1)])
    
    #Model 5 arima
    Model5 <- arima(dataframe_arima, order=c(25,0,25),method='ML') #creating arima model
    dataframe_arima_mod=as.data.frame(predict(Model5,n.ahead=144))
    j=c()
    for (i in 1:144){
        j[i]=dataframe_arima_mod$pred[i]
    }
    j=as.data.frame(j)
    
    #Model 6 arimax
    ddlj=copy(train_data_xts[,c(2:14)])
    pickup_col=copy(train_data_xts[,c(1)])
    Model6= auto.arima(pickup_col, xreg=ddlj)
    ddlj_future=copy(test_data_xts[,c(2:14)])
    fcast <- Model6%>%
        forecast(xreg = ddlj_future)
    dataframe_val_htt=fortify(fcast$mean,ts.connect=TRUE)
    dataframe_val_htt=dataframe_val_htt[-c(1)]
    
    model1_pred=reactive({
        dateinp= input$date
        hourinp = input$hour
        index=24*(dateinp-1) + hourinp
        if(index <= 600) {
            predict(rt.fit, train_x[index,])
        }
        else {
            index = index - 600
            predict(rt.fit, test_x[index,])
        }
    })
    
    
    model2_pred=reactive({
        dateinp= input$date
        hourinp = input$hour
        index=24*(dateinp-1) + hourinp
        if(index <= 600) {
            predict(xg.fit, data.matrix(train_x[index,]))
        }
        else {
            index = index - 600
            predict(xg.fit, data.matrix(test_x[index,]))
        }
    })
    
    model3_pred=reactive({
        dateinp= input$date
        hourinp = input$hour
        index=24*(dateinp-1) + hourinp
        if(index <= 600) {
            monmlp.predict(weights=mlp.fit, x=data.matrix(train_x[index,]))[1]
        }
        else {
            index = index - 600
            monmlp.predict(weights=mlp.fit, x=data.matrix(test_x[index,]))[1]
        }
    })
    
    model4_pred=reactive({
        dateinp= input$date
        hourinp = input$hour
        index=24*(dateinp-1) + hourinp
        if(index <= 144) {
            return(data_xts[index,c(1)])
        }
        else {
            Model4 <- VAR(data_xts[1:index-1,], p = 8, type = "const", season = NULL, exog = NULL)
            fc = predict(Model4, n.ahead = 1, ci = 0.95)
            chi=as.data.frame(fc$fcst[1])
            chi1=chi[1,c(1)]
            return(chi1)
        }
    })
    
    model5_pred=reactive({
        dateinp= input$date
        hourinp = input$hour
        index=24*(dateinp-1) + hourinp
        if(index <= 144) {
            return(data_xts[index,c(1)])
        }
        else {
            z=as.data.frame(data_xts[1:index-1,c(1)])
            Model5 <- arima(z, order=c(25,0,25),method='ML') #creating arima model
            dfa=as.data.frame(predict(Model5,n.ahead=1))
            return(dfa[1,c(1)])
        }
    })
    
    model6_pred=reactive({
        dateinp= input$date
        hourinp = input$hour
        index=24*(dateinp-1) + hourinp
        if(index <= 144) {
            return(data_xts[index,c(1)])
        }
        else {
            ddlj=copy(data_xts[1:index-1,c(2:14)])
            pickup_col=copy(data_xts[1:index-1,c(1)])
            Model6= auto.arima(pickup_col, xreg=ddlj)
            ddlj_future=copy(data_xts[index+1,c(2:14)])
            fcast <- Model6%>%
                forecast(xreg = ddlj_future)
            dataframe_val_htt=fortify(fcast$mean,ts.connect=TRUE)
            dataframe_val_htt=dataframe_val_htt[-c(1)]
            return (dataframe_val_htt[1,c(1)])
        }
    })
    
    # location ID:
    # if(input$Loc_1){
    #   pu_loc = 40
    # }
    # if(input$Loc_2){
    #   pu_loc = 50
    # }
    # if(input$Loc_3){
    #   pu_loc = 60
    # }
    # if(input$Loc_4){
    #   pu_loc = 70
    # }
    
    max_short = reactive({
        set.seed(123)
        taxi <- fread('peak_hours.csv')
        coord = taxi[,c("trip_distance")]
        distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
        taxi$cluster_distance = as.factor(distance_clust$cluster)
        clus_2 = taxi[taxi$cluster_distance == 2,]
        paste0(max(clus_2$trip_distance))
    })
    
    avg_short = reactive({
        set.seed(123)
        taxi <- fread('peak_hours.csv')
        coord = taxi[,c("trip_distance")]
        distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
        taxi$cluster_distance = as.factor(distance_clust$cluster)
        clus_2 = taxi[taxi$cluster_distance == 2,]
        paste0(round(mean(clus_2$tip_amount), 2))
    })
    
    pay_short = reactive({
        set.seed(123)
        taxi <- fread('peak_hours.csv')
        coord = taxi[,c("trip_distance")]
        distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
        taxi$cluster_distance = as.factor(distance_clust$cluster)
        clus_2 = taxi[taxi$cluster_distance == 2,]
        payment_type_clus_2 = data.frame(table(clus_2$payment_type))
        colnames(payment_type_clus_2) <- c('Payment Type','Count')
        payment_type_clus_2 = payment_type_clus_2[order(-payment_type_clus_2$Count),]
        payment_type = head(payment_type_clus_2$`Payment Type`, 1)
        payment = ""
        if (payment_type == 1){
            payment = "Credit Card"
        }
        else if(payment_type == 2){
            payment = "Cash"
        }
        else if(payment_type == 3){
            payment = "No Charge"
        }
        else if(payment_type == 4){
            payment = "Dispute"
        }
        else if(payment_type == 5){
            payment = "Unknown"
        }
        else if(payment_type == 6){
            payment = "Voided Trip"
        }
        
        paste0(payment)
    })
    
    loc_short = reactive({
        set.seed(123)
        taxi <- fread('peak_hours.csv')
        coord = taxi[,c("trip_distance")]
        distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
        taxi$cluster_distance = as.factor(distance_clust$cluster)
        clus_2 = taxi[taxi$cluster_distance == 2,]
        df2 = data.frame(table(clus_2$PULocationID))
        colnames(df2) <- c('PULocationID','Count')
        df2 = df2[order(-df2$Count),]
        paste0(head(df2$PULocationID, 10))
    })
    
    cnt_short = reactive({
        set.seed(123)
        taxi <- fread('peak_hours.csv')
        coord = taxi[,c("trip_distance")]
        distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
        taxi$cluster_distance = as.factor(distance_clust$cluster)
        clus_1 = taxi[taxi$cluster_distance == 1,]
        clus_2 = taxi[taxi$cluster_distance == 2,]
        short_dist_passenger_count = sum(clus_1$passenger_count)
        long_dist_passenger_count = sum(clus_2$passenger_count)
        diff_in_passenger_count = short_dist_passenger_count - long_dist_passenger_count
        paste0(long_dist_passenger_count)
        
        
    })
    
    max_long = reactive({
        set.seed(123)
        taxi <- fread('peak_hours.csv')
        coord = taxi[,c("trip_distance")]
        distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
        taxi$cluster_distance = as.factor(distance_clust$cluster)
        clus_1 = taxi[taxi$cluster_distance == 1,]
        paste0(max(clus_1$trip_distance))
    })
    
    avg_long = reactive({
        set.seed(123)
        taxi <- fread('peak_hours.csv')
        coord = taxi[,c("trip_distance")]
        distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
        taxi$cluster_distance = as.factor(distance_clust$cluster)
        clus_1 = taxi[taxi$cluster_distance == 1,]
        paste0(round(mean(clus_1$tip_amount), 2))
    })
    
    pay_long = reactive({
        set.seed(123)
        taxi <- fread('peak_hours.csv')
        coord = taxi[,c("trip_distance")]
        distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
        taxi$cluster_distance = as.factor(distance_clust$cluster)
        clus_1 = taxi[taxi$cluster_distance == 1,]
        payment_type_clus_1 = data.frame(table(clus_1$payment_type))
        colnames(payment_type_clus_1) <- c('Payment Type','Count')
        payment_type_clus_1 = payment_type_clus_1[order(-payment_type_clus_1$Count),]
        payment_type = head(payment_type_clus_1$`Payment Type`, 1)
        payment = ""
        if (payment_type == 1){
            payment = "Credit Card"
        }
        else if(payment_type == 2){
            payment = "Cash"
        }
        else if(payment_type == 3){
            payment = "No Charge"
        }
        else if(payment_type == 4){
            payment = "Dispute"
        }
        else if(payment_type == 5){
            payment = "Unknown"
        }
        else if(payment_type == 6){
            payment = "Voided Trip"
        }
        
        paste0(payment)
    })
    
    loc_long = reactive({
        set.seed(123)
        taxi <- fread('peak_hours.csv')
        coord = taxi[,c("trip_distance")]
        distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
        taxi$cluster_distance = as.factor(distance_clust$cluster)
        clus_1 = taxi[taxi$cluster_distance == 1,]
        df2 = data.frame(table(clus_1$PULocationID))
        colnames(df2) <- c('PULocationID','Count')
        df2 = df2[order(-df2$Count),]
        paste0(head(df2$PULocationID, 10))
    })
    cnt_long = reactive({
        set.seed(123)
        taxi <- fread('peak_hours.csv')
        coord = taxi[,c("trip_distance")]
        distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
        taxi$cluster_distance = as.factor(distance_clust$cluster)
        clus_1 = taxi[taxi$cluster_distance == 1,]
        clus_2 = taxi[taxi$cluster_distance == 2,]
        short_dist_passenger_count = sum(clus_1$passenger_count)
        long_dist_passenger_count = sum(clus_2$passenger_count)
        diff_in_passenger_count = short_dist_passenger_count - long_dist_passenger_count
        paste0(short_dist_passenger_count)
        
        
    })
    # diff_dist = reactive({
    #   set.seed(123)
    #   taxi <- fread('peak_hours.csv')
    #   coord = taxi[,c("trip_distance")]
    #   distance_clust = kmeans(x=coord, centers=3, iter.max=500, nstart=20)
    #   taxi$cluster_distance = as.factor(distance_clust$cluster)
    #   clus_1 = taxi[taxi$cluster_distance == 1,]
    #   clus_2 = taxi[taxi$cluster_distance == 2,]
    #   short_dist_passenger_count = sum(clus_1$passenger_count)
    #   long_dist_passenger_count = sum(clus_2$passenger_count)
    #   diff_in_passenger_count = short_dist_passenger_count - long_dist_passenger_count
    #   paste0("Total passenger count for short distance trips is ",short_dist_passenger_count, "\n",
    #          "Total passenger count for long distance trips is ",long_dist_passenger_count, "\n",
    #          "Difference in passenger counts for long and short distance trips is ", diff_in_passenger_count
    #   )
    # })
    
    # pu_loc = 40
    # taxi_s <- fread('peak_hours.csv')
    # taxi_grp = taxi_s %>%
    #   mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
    #          pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
    #   group_by(pickup_date) %>%
    #   summarise(total_trips=sum(PULocationID == pu_loc))
    # 
    # max_trips = max(taxi_grp$total_trips)
    # min_trips = min(taxi_grp$total_trips)
    # 
    # #Peak hour
    # taxi_samp[taxi_samp$total_trips == max_trips,]
    # 
    # #Off-peak hour
    # taxi_samp[taxi_samp$total_trips == min_trips,]
    
    
    output$Plot1 = renderPlot({dateinp=input$date
    plot(test_y$number_pickups,type = "l", col ='blue', xlab = 'Day', ylab = 'Number of pickups')
    if(input$Model_1){
        lines(rt.predict, type = "l", col ='red') 
    }
    if(input$Model_2){
        lines(xg.predict, type = "l", col ='red') 
    }
    if(input$Model_3){
        lines(mlp.predict, type = "l", col ='red')
    }
    if(input$Model_4){
        lines(tt$pickups.fcst,type = "l", col ='red', xlab = 'Day', ylab = 'Number of pickups')
    }
    if(input$Model_5){
        lines(j$j,type = "l", col ='red', xlab = 'Day', ylab = 'Number of pickups')
    }
    if(input$Model_6){
        lines(dataframe_val_htt[,c(1)],type = "l", col ='red', xlab = 'Day', ylab = 'Number of pickups')
    }
    
    output$predloc1 = renderText({
        if(input$Loc_1){
            pu_loc = 40
            taxi_s <- fread('peak_hours.csv')
            taxi_grp = taxi_s %>%
                mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
                       pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
                group_by(pickup_date) %>%
                summarise(total_trips=sum(PULocationID == pu_loc))
            
            max_trips = max(taxi_grp$total_trips)
            paste0(max(taxi_grp[taxi_grp$total_trips == max_trips,]$pickup_date))
        }
    })
    output$predloc2 = renderText({
        if(input$Loc_2){
            pu_loc = 50
            taxi_s <- fread('peak_hours.csv')
            taxi_grp = taxi_s %>%
                mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
                       pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
                group_by(pickup_date) %>%
                summarise(total_trips=sum(PULocationID == pu_loc))
            
            max_trips = max(taxi_grp$total_trips)
            paste0(max(taxi_grp[taxi_grp$total_trips == max_trips,]$pickup_date))
        }
    })
    output$predloc3 = renderText({
        if(input$Loc_3){
            pu_loc = 60
            taxi_s <- fread('peak_hours.csv')
            taxi_grp = taxi_s %>%
                mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
                       pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
                group_by(pickup_date) %>%
                summarise(total_trips=sum(PULocationID == pu_loc))
            
            max_trips = max(taxi_grp$total_trips)
            paste0(max(taxi_grp[taxi_grp$total_trips == max_trips,]$pickup_date))
        }
    })
    output$predloc4 = renderText({
        if(input$Loc_4){
            pu_loc = 70
            taxi_s <- fread('peak_hours.csv')
            taxi_grp = taxi_s %>%
                mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
                       pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
                group_by(pickup_date) %>%
                summarise(total_trips=sum(PULocationID == pu_loc))
            
            max_trips = max(taxi_grp$total_trips)
            paste0(max(taxi_grp[taxi_grp$total_trips == max_trips,]$pickup_date))
        }
    })
    
    output$predloc5 = renderText({
        if(input$Loc_1){
            pu_loc = 40
            taxi_s <- fread('peak_hours.csv')
            taxi_s = taxi_s[(format(taxi_s$tpep_pickup_datetime,'%Y') == "2021" ) & 
                                (format(taxi_s$tpep_dropoff_datetime,'%Y') == "2021"), ]
            
            taxi_s = taxi_s[(format(taxi_s$tpep_pickup_datetime,'%m') == "01" ) | 
                                (format(taxi_s$tpep_dropoff_datetime,'%m') == "01"), ]
            
            taxi_grp = taxi_s %>%
                mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
                       pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
                group_by(pickup_date) %>%
                summarise(total_trips=sum(PULocationID == pu_loc))
            
            min_trips = min(taxi_grp$total_trips)
            paste0(min(taxi_grp[taxi_grp$total_trips == min_trips,]$pickup_date))
        }
    })
    output$predloc6 = renderText({
        if(input$Loc_2){
            pu_loc = 50
            taxi_s <- fread('peak_hours.csv')
            taxi_s = taxi_s[(format(taxi_s$tpep_pickup_datetime,'%Y') == "2021" ) & 
                                (format(taxi_s$tpep_dropoff_datetime,'%Y') == "2021"), ]
            
            taxi_s = taxi_s[(format(taxi_s$tpep_pickup_datetime,'%m') == "01" ) | 
                                (format(taxi_s$tpep_dropoff_datetime,'%m') == "01"), ]
            taxi_grp = taxi_s %>%
                mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
                       pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
                group_by(pickup_date) %>%
                summarise(total_trips=sum(PULocationID == pu_loc))
            
            min_trips = min(taxi_grp$total_trips)
            paste0(min(taxi_grp[taxi_grp$total_trips == min_trips,]$pickup_date))
        }
    })
    output$predloc7 = renderText({
        if(input$Loc_3){
            pu_loc = 60
            taxi_s <- fread('peak_hours.csv')
            taxi_s = taxi_s[(format(taxi_s$tpep_pickup_datetime,'%Y') == "2021" ) & 
                                (format(taxi_s$tpep_dropoff_datetime,'%Y') == "2021"), ]
            
            taxi_s = taxi_s[(format(taxi_s$tpep_pickup_datetime,'%m') == "01" ) | 
                                (format(taxi_s$tpep_dropoff_datetime,'%m') == "01"), ]
            taxi_grp = taxi_s %>%
                mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
                       pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
                group_by(pickup_date) %>%
                summarise(total_trips=sum(PULocationID == pu_loc))
            
            min_trips = min(taxi_grp$total_trips)
            paste0(min(taxi_grp[taxi_grp$total_trips == min_trips,]$pickup_date))
        }
    })
    
    output$predloc8 = renderText({
        if(input$Loc_4){
            pu_loc = 70
            taxi_s <- fread('peak_hours.csv')
            taxi_s = taxi_s[(format(taxi_s$tpep_pickup_datetime,'%Y') == "2021" ) & 
                                (format(taxi_s$tpep_dropoff_datetime,'%Y') == "2021"), ]
            
            taxi_s = taxi_s[(format(taxi_s$tpep_pickup_datetime,'%m') == "01" ) | 
                                (format(taxi_s$tpep_dropoff_datetime,'%m') == "01"), ]
            taxi_grp = taxi_s %>%
                mutate(date = lubridate::ymd_hms(tpep_pickup_datetime),
                       pickup_date = format(tpep_pickup_datetime, "%Y-%m-%d %H:00:00")) %>%
                group_by(pickup_date) %>%
                summarise(total_trips=sum(PULocationID == pu_loc))
            
            min_trips = min(taxi_grp$total_trips)
            paste0(min(taxi_grp[taxi_grp$total_trips == min_trips,]$pickup_date))
        }
    })
    
    # Maximum trip distance for long distance taxi travels
    output$maxlong1 = renderText({
        if(input$Long_Loc){
            max_long()
        }
    })
    
    # Average trip distance for long distance taxi travels
    output$avglong1 = renderText({
        if(input$Long_Loc){
            avg_long()
        }
    })
    
    # Payment method preferred for long distance taxi travels
    output$paylong1 = renderText({
        if(input$Long_Loc){
            pay_long()
        }
    })
    
    # Top 10 Locations for long distance taxi travels
    output$loclong1 = renderText({
        if(input$Long_Loc){
            loc_long()
        }
    })
    
    # Total passenger count for long distance taxi travels
    output$cntlong1 = renderText({
        if(input$Long_Loc){
            cnt_long()
        }
    })
    
    # Maximum trip distance for short distance taxi travels
    output$maxshort1 = renderText({
        if(input$Short_Loc){
            max_short()
        }
    })
    
    # Average trip distance for short distance taxi travels
    output$avgshort1 = renderText({
        if(input$Short_Loc){
            avg_short()
        }
    })
    
    # Payment method preferred for short distance taxi travels
    output$payshort1 = renderText({
        if(input$Short_Loc){
            pay_short()
        }
    })
    
    # Top 10 Locations for short distance taxi travels
    output$locshort1 = renderText({
        if(input$Short_Loc){
            loc_short()
        }
    })
    
    # Total passenger count for short distance taxi travels
    output$cntshort1 = renderText({
        if(input$Short_Loc){
            cnt_short()
        }
    })
    
    
    legend('topright',legend=c('Actual','Predicted'),col=c('blue','red'))  
    
    output$pred1=renderText({
        if(input$Model_1){
            model1_pred()
        }  })
    output$pred2=renderText({
        if(input$Model_2){
            model2_pred()
        }  })
    output$pred3=renderText({
        if(input$Model_3){
            model3_pred()
        }
    })
    output$pred4=renderText({
        if(input$Model_4){
            model4_pred()
        }
    })
    output$pred5=renderText({
        if(input$Model_5){
            model5_pred()
        }
    })
    output$pred6=renderText({
        if(input$Model_6){
            model6_pred()
        }
    })
    })
    
})

### This is a replica of phs_covid_death_ARIMA_model.R
### This is created for Presentation purpose.


automated_arima_dth <- function (lag_value) {
  #Data Preparation---------------------------------------------------------------
  
  #For forecasting, we chose the latest data
  trend_death_hb <- trend_hb_daily %>% 
    filter (hb_name == "Scotland") %>% 
    filter(date >="2021-06-01") %>% 
    filter(!(is.na(daily_deaths))) %>% 
    select(date, daily_deaths)
  
  # Convert it into a time series
  daily_death_hb_zoo <- zoo(trend_death_hb$daily_deaths, 
                            order.by=as.Date(trend_death_hb$date, format='%m/%d/%Y'))
  
  # Convert it into a time series
  daily_death_hb_timeseries <-  timeSeries::as.timeSeries(daily_death_hb_zoo)
  
  #Step 1 : Visualise the time series---------------------------------------------
  
  original_series_death<-autoplot(daily_death_hb_timeseries, ts.colour = '#5ab4ac')+
    xlab("Month") + 
    ylab("Patient died")+
    #scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    ggtitle("Trend on Deaths") +
    color_theme()
  
  #Step 2 : Identification of model (Finding d)-----------------------------------
  
  # Identify whether the time series is stationary / non stationary
  # using ADF Augmented Dickey-Fuller test 
  
  adf_test_death <- adf.test(daily_death_hb_timeseries)
  
  first_diff_death<- diff(daily_death_hb_timeseries)
  adf_test1_death <- adf.test(na.omit(first_diff_death))
  
  #Create a data frame to store the adf values
  adf_data_death <- data.frame(Data = c("Original", "First-Ordered"),
                               Dickey_Fuller = c(adf_test_death$statistic, adf_test1_death$statistic),
                               p_value = c(adf_test_death$p.value,adf_test1_death$p.value))
  adf_data_death
  
  # First Order Difference
  
  first_diff_death<- diff(daily_death_hb_timeseries)
  p<- autoplot(first_diff_death, ts.colour = '#5ab4ac') +
    xlab("Month") + 
    ylab("DEATH")+
    # scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    ggtitle("First-Order Difference Series") +
    color_theme()
  
  #Step 3: Estimate the parameters (Finding p and q)-----------------------------
  
  par(mfrow=c(2,2))
  acf_death  <- acf1(first_diff_death, col=2:7, lwd=4)
  pacf_death <- acf1(first_diff_death,  pacf = TRUE, col=2:7, lwd=4)
  
  #Step 4 : Build the ARIMA model------------------------------------------------ 
  #Automated method
  
  if (lag_value == 0){
    auto_arima_fit_death <- auto.arima(daily_death_hb_timeseries,lag_value,
                                       seasonal=FALSE,
                                       stepwise=FALSE,
                                       approximation=FALSE,
                                       trace = TRUE  )
  }
  else{
    auto_arima_fit_death <- auto.arima(lag(daily_death_hb_timeseries,lag_value),
                                       seasonal=FALSE,
                                       stepwise=FALSE,
                                       approximation=FALSE,
                                       trace = TRUE
    )
  }
  #Finding the co-efficient
  coef_dth<-lmtest::coeftest(auto_arima_fit_death)
  
  #Step 5: Check the diagnostics
  res_dth <-checkresiduals(auto_arima_fit_death, theme = color_theme())
  
  #Step 6: Plot the actual data and fitted data-----------------------------------
  
  # Convert model and time series to dataframe for plotting
  
  daily_death_hb_timeseries_data <- fortify(daily_death_hb_timeseries) %>% 
    clean_names() %>% 
    remove_rownames %>% 
    rename (date = index,
            death = data)%>% 
    mutate(index = seq(1:nrow(daily_death_hb_timeseries)))
  
  arima_fit_dth_resid <- ts(daily_death_hb_timeseries[1:nrow(daily_death_hb_timeseries)]) - resid(auto_arima_fit_death)
  
  arima_fit_dth_data <- fortify(arima_fit_dth_resid) %>% 
    clean_names() %>% 
    mutate(data = round(data,2))
  
  fit_existing_dth_data <- daily_death_hb_timeseries_data %>% 
    inner_join(arima_fit_dth_data, by = c("index"))
  
  #plotting the series along with the fitted values
  fit_existing_dth_plot <- fit_existing_dth_data %>% 
    mutate(date = as.Date(date)) %>% 
    ggplot()+
    aes(x=date, y = death)+
    geom_line(color ="#5ab4ac")+
    geom_line(aes(x= date, y = data), colour = "red" )+
    xlab("Month") + 
    ylab("Deaths reported")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
    ggtitle("Fitting the ARIMA model with existing data") +
    color_theme()
  
  #Step 7: Forecast the Model ----------------------------------------------------
  
  forecast_dth_model <- forecast(auto_arima_fit_death,level = c(80, 95), h = 15) 
  
  #Convert the model to dataframe for plotting
  
  # Negative values of the CI interval are considered as 0
  
  forecast_dth_model_data <- fortify(forecast_dth_model) %>% 
    clean_names() %>% 
    mutate(data = round(data,2),
           fitted= round(fitted,2))  %>% 
    mutate (lo_80 = ifelse(lo_80 < 0,0,lo_80),
            lo_95 = ifelse(lo_95 < 0,0,lo_95)
    )
  forecast_start_date <- as.Date(max(daily_death_hb_timeseries_data$date)+1)
  forecast_end_date <- as.Date(forecast_start_date+14)
  
  forecast_dth_data <- forecast_dth_model_data %>% 
    filter(!(is.na(point_forecast))) %>% 
    mutate(date = seq(forecast_start_date,forecast_end_date, by =1)) %>% 
    select(-data,-fitted, -index)  
  
  fitted_dth_data <- forecast_dth_model_data %>% 
    filter(!(is.na(data))) %>% 
    inner_join(daily_death_hb_timeseries_data, by = c("index")) %>% 
    mutate(date = as.Date(date)) %>% 
    select(date, data, fitted) 
  
  #Plotting the Vaccination series plus the forecast and 95% prediction intervals
  
  annotation <- data.frame(
    x = c(as.Date("13-06-2021","%d-%m-%Y"),as.Date("11-10-2021","%d-%m-%Y")),
    y = c(30,60),
    label = c("PAST", "FUTURE")
  )
  
  #Time series plots for the next 15 days according to best ARIMA models with 80%–95% CI.
  forecast_data_dth_plot <- fitted_dth_data %>% 
    ggplot()+
    geom_line(aes(x= date, y = data), color = "#5ab4ac")+
    # geom_line(aes(x= date, y = fitted), colour = "red" )+
    geom_line(aes(x= date, y =point_forecast), color ="blue", data = forecast_dth_data )+
    geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_80, ymax = hi_80), 
                data = forecast_dth_data, alpha = 0.3, fill = "green")+
    geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_95, ymax = hi_95), 
                data = forecast_dth_data, alpha = 0.1)+
    geom_vline(aes(xintercept=as.numeric(max(date))),color="#f1a340", linetype="dashed",data = fitted_dth_data)+
    ggtitle("Projection of new Deaths") +
    xlab("Month") + 
    ylab("Death reported")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    color_theme()+
    scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  return(list(forecast_dth_data, fit_existing_dth_data))
}

list_0 <- automated_arima_dth(0)
forecast_dth_data_0 <- list_0[[1]]
fitted_dth_data_0 <- list_0[[2]]

list_1 <- automated_arima_dth(1)
forecast_dth_data_1  <- list_1[[1]]
fitted_dth_data_1 <- list_1[[2]]

list_2 <- automated_arima_dth(2)
forecast_dth_data_2 <- list_2[[1]]
fitted_dth_data_2 <- list_2[[2]]

list_3 <- automated_arima_dth(3)
forecast_dth_data_3 <- list_3[[1]]
fitted_dth_data_3 <- list_3[[2]]

#Time series plots for the next 60 days according to best ARIMA models with 80%–95% CI.
fitted_dth_data_0$lab1 = "ARIMA(2,1,3)"
forecast_data_dth_plot_0 <- fitted_dth_data_0 %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot()+
  geom_line(aes(x= date, y = death), color = "#5ab4ac")+
  geom_line(aes(x= date, y = data), colour = "red" )+
  geom_line(aes(x= date, y =point_forecast), color ="blue", data = forecast_dth_data_0 )+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_80, ymax = hi_80), 
              data = forecast_dth_data_0, alpha = 0.3, fill = "green")+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_95, ymax = hi_95), 
              data = forecast_dth_data_0, alpha = 0.1)+
  geom_vline(aes(xintercept=as.numeric(min(date))),color="#f1a340", linetype="dashed",data = forecast_dth_data_0)+
  xlab("Month") + 
  ylab("Death reported")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  color_theme()+
  scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  facet_wrap(~lab1)

#Time series plots for the next 60 days according to best ARIMA models with 80%–95% CI.
fitted_dth_data_1$lab1 = "ARIMA(2,1,3) lag = 1"
forecast_data_dth_plot_1 <- fitted_dth_data_1 %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot()+
  geom_line(aes(x= date, y = death), color = "#5ab4ac")+
  geom_line(aes(x= date, y = data), colour = "red" )+
  geom_line(aes(x= date, y =point_forecast), color ="blue", data = forecast_dth_data_1 )+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_80, ymax = hi_80), 
              data = forecast_dth_data_1, alpha = 0.3, fill = "green")+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_95, ymax = hi_95), 
              data = forecast_dth_data_1, alpha = 0.1)+
  geom_vline(aes(xintercept=as.numeric(min(date))),color="#f1a340", linetype="dashed",data = forecast_dth_data_1)+
  xlab("Month") + 
  ylab("Death reported")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  color_theme()+
  scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  facet_wrap(~lab1)

#Time series plots for the next 60 days according to best ARIMA models with 80%–95% CI.
fitted_dth_data_2$lab1 = "ARIMA(5,1,0)"
forecast_data_dth_plot_2 <- fitted_dth_data_2 %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot()+
  geom_line(aes(x= date, y = death), color = "#5ab4ac")+
  geom_line(aes(x= date, y = data), colour = "red" )+
  geom_line(aes(x= date, y =point_forecast), color ="blue", data = forecast_dth_data_2 )+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_80, ymax = hi_80), 
              data = forecast_dth_data_1, alpha = 0.3, fill = "green")+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_95, ymax = hi_95), 
              data = forecast_dth_data_1, alpha = 0.1)+
  geom_vline(aes(xintercept=as.numeric(min(date))),color="#f1a340", linetype="dashed",data = forecast_dth_data_2)+
  xlab("Month") + 
  ylab("Death reported")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  color_theme()+
  scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  facet_wrap(~lab1)

#Time series plots for the next 60 days according to best ARIMA models with 80%–95% CI.
fitted_dth_data_3$lab1 = "ARIMA(5,1,0) with drift"
forecast_data_dth_plot_3 <- fitted_dth_data_3 %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot()+
  geom_line(aes(x= date, y = death), color = "#5ab4ac")+
  geom_line(aes(x= date, y = data), colour = "red" )+
  geom_line(aes(x= date, y =point_forecast), color ="blue", data = forecast_dth_data_3 )+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_80, ymax = hi_80), 
              data = forecast_dth_data_3, alpha = 0.3, fill = "green")+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_95, ymax = hi_95), 
              data = forecast_dth_data_3, alpha = 0.1)+
  geom_vline(aes(xintercept=as.numeric(min(date))),color="#f1a340", linetype="dashed",data = forecast_dth_data_3)+
  xlab("Month") + 
  ylab("Death reported")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  color_theme()+
  scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  facet_wrap(~lab1)


annotation <- data.frame(
  x = c(as.Date("13-06-2021","%d-%m-%Y"),as.Date("11-10-2021","%d-%m-%Y")),
  y = c(20,30),
  label = c("PAST", "FUTURE")
)
colors <- c("ARIMA (5,1,0) with drift" = "blue", 
            "ARIMA (5,1,0)" = "red", 
            "ARIMA (2,1,3) lag = 1" = "orange",
            "ARIMA (2,1,3)" = "black")

forecast_data_dth_plot_all <- fitted_dth_data %>% 
  ggplot()+
  geom_line(aes(x= date, y = data), color = "#5ab4ac")+
  # geom_line(aes(x= date, y = fitted), colour = "red" )+
  geom_line(aes(x= date, y =point_forecast, color ="ARIMA (2,1,3)"),data = forecast_dth_data_0)+
  geom_line(aes(x= date, y =point_forecast, color ="ARIMA (2,1,3) lag = 1"),data = forecast_dth_data_1)+
  geom_line(aes(x= date, y =point_forecast, color ="ARIMA (5,1,0)"), data = forecast_dth_data_2)+
  geom_line(aes(x= date, y =point_forecast, color ="ARIMA (5,1,0) with drift"), data = forecast_dth_data_3 )+
  labs(color = "Model")+
  scale_color_manual(values = colors)+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_80, ymax = hi_80),
              data = forecast_dth_data, alpha = 0.3, fill = "green")+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_95, ymax = hi_95),
              data = forecast_dth_data, alpha = 0.1)+
  geom_vline(aes(xintercept=as.numeric(max(date))),color="#f1a340", linetype="dashed",data = fitted_dth_data)+
 # ggtitle("Projection of new Deaths based on various models") +
  xlab("Month") + 
  ylab("Death reported")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  color_theme()+
  scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  geom_text(data=annotation, 
            aes( x=x, y=y, label=label),                  
            color="blue", 
            size=4 )



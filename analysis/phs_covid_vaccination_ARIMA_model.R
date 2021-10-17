
#ARIMA MODEL


#Data Preparation

trend_vacc_hb <- trend_vacc_hb %>% 
  filter (dose == "Dose 2") %>% 
  select(date,cumulative_number_vaccinated)

# Convert it to zoo type
daily_vacc_hb_zoo <- zoo(trend_vacc_hb$cumulative_number_vaccinated, 
                         order.by=as.Date(trend_vacc_hb$date, format='%m/%d/%Y'))
# Convert it into a time series
daily_vacc_hb_timeseries <-timeSeries::as.timeSeries(daily_vacc_hb_zoo)

#Step 1 : Visualise the time series

original_series<-autoplot(daily_vacc_hb_timeseries, colour = '#5ab4ac')+
  xlab("Month") + 
  ylab("Number of People vaccinated")+
  #scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle("Trend on Vaccination") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))+
  color_theme()

#Step 2 : Identification of model :
# Identify whether the time series is stationary / non stationary
# using ADF Augmented Dickey-Fuller test 

adf_test <- adf.test(daily_vacc_hb_timeseries)

first_diff_ts<- diff(daily_vacc_hb_timeseries)
adf_test1 <- adf.test(na.omit(first_diff_ts))

second_diff_ts<- diff(first_diff_ts)
adf_test2 <- adf.test(na.omit(second_diff_ts))

#Create a data frame to store the adf values
adf_data <- data.frame(Data = c("Original", "First-Ordered", "Second Ordered"),
                       Dickey_Fuller = c(adf_test$statistic, adf_test1$statistic, adf_test2$statistic),
                       p_value = c(adf_test$p.value,adf_test1$p.value,adf_test2$p.value))


# First Order Difference
first_order<- autoplot(first_diff_ts, ts.colour = '#5ab4ac') +
  xlab("Month") + 
  ylab("VACCINATED")+
  # scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle("First-Order Difference Series") +
  color_theme()

# Second Order Difference
second_order<- autoplot(second_diff_ts, ts.colour = '#5ab4ac') +
  xlab("Month") + 
  ylab("VACCINATED")+
  # scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle("Second-Order Difference Series") +
  color_theme()

# Build an automatic ARIMA Model
auto_arima_fit <- auto.arima(daily_vacc_hb_timeseries,
                             seasonal=FALSE
)

#Build the ARIMA model (Manual)

arima_fit1 = Arima(daily_vacc_hb_timeseries, order = c(1,2,0))
arima_fit2 = Arima(daily_vacc_hb_timeseries, order = c(0,2,1))
arima_fit3 = Arima(daily_vacc_hb_timeseries, order = c(1,2,1))
arima_fit4 = Arima(daily_vacc_hb_timeseries, order = c(3,2,2))

#plotting the series along with the fitted values

arima_fit <- ts(daily_vacc_hb_timeseries) - resid(auto_arima_fit)

# arima_fit_existing <- autoplot(ts(daily_vacc_hb_timeseries), colour = '#5ab4ac') +
#   autolayer(auto_arima_fit) +
#   xlab("Month") + 
#   ylab("Number of People vaccinated")+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
#   ggtitle("Fitting the ARIMA model with existing data") +
#   scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))+
#   color_theme()

#Plotting the Vaccination series plus the forecast and 95% prediction intervals

#forecast automated model
forecast_model <- forecast(auto_arima_fit,level = c(80, 95), h = 30) 

#forecast manual models
future  = forecast(arima_fit1, level = c(80, 95), h = 30)
future2 = forecast(arima_fit2, level = c(80, 95), h = 30)
future3 = forecast(arima_fit3, level = c(80, 95), h = 30)
future4 = forecast(arima_fit4, level = c(80, 95), h = 30)


# Text to print on the plot
annotation <- data.frame(
  x = c(50,305),
  y = c(1000000,3000000),
  label = c("PAST", "FUTURE")
)

# Forecasted Vaccination Plot
vacc_prediction <- forecast_model %>% 
  autoplot(colour ='#f1a340') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle("Forecast for 1 month") +
  xlab("Year") +
  ylab("No of people vaccinated") +
  scale_y_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))+
  color_theme()  +
  geom_text(data=annotation, 
            aes( x=x, y=y, label=label),                  
            color="red", 
            size=4 )+
  geom_vline(xintercept =285, linetype = 2)

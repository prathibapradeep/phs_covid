#-------------------------------------------------------------------------- 
# Name :phs_covid_hospitalisation_ARIMA_model.R
# This script forecasts the projection of hospitalization using ARIMA Model
# This is also a consolidated version for the presentation (slidy)
# Created by Prathiba
# Date : 15-Oct-2021
#--------------------------------------------------------------------------

#Data Preparation-------------------------------------------------------------

#For forecasting, we chose the latest data
trend_hosp_hb <- trend_hb_daily %>% 
  filter (hb_name == "Scotland") %>% 
  filter(date >="2021-06-01") %>% 
  filter(!(is.na(hospital_admissions))) %>% 
  select(date, hospital_admissions)

# Convert it to zoo type
daily_hosp_hb_zoo <- zoo(trend_hosp_hb$hospital_admissions, 
                         order.by=as.Date(trend_hosp_hb$date, format='%m/%d/%Y'))

# Convert it into a time series
daily_hosp_hb_timeseries <-  timeSeries::as.timeSeries(daily_hosp_hb_zoo)

#Step 1 : Visualise the time series---------------------------------------------

original_series<-autoplot(daily_hosp_hb_timeseries, colour = '#5ab4ac')+
  xlab("Month") + 
  ylab("Number of People hospitalised")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle("Original Series") +
 # scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))+
  color_theme()

#Step 2 : Identification of model (Finding d)-----------------------------------

# Identify whether the time series is stationary / non stationary
# using ADF Augmented Dickey-Fuller test 

adf_test_hosp <- adf.test(daily_hosp_hb_timeseries)

first_diff_hosp<- diff(daily_hosp_hb_timeseries)
adf_test1_hosp <- adf.test(na.omit(first_diff_hosp))

#Create a data frame to store the adf values
adf_data_hosp <- data.frame(Data = c("Original", "First-Ordered"),
                       Dickey_Fuller = c(adf_test_hosp$statistic, adf_test1_hosp$statistic),
                       p_value = c(adf_test_hosp$p.value,adf_test1_hosp$p.value))

# First Order Difference
first_diff_hosp_plot<- autoplot(first_diff_hosp, ts.colour = '#5ab4ac') +
  xlab("Month") + 
  ylab("HOSPITALIZATION")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle("First-Order Difference") +
  color_theme()

#Step 3: Estimate the parameters (Finding p and q)-----------------------------

acf_hosp  <- acf1(first_diff_hosp, col=2:7, lwd=4, theme = color_theme())
pacf_hosp <- acf1(first_diff_hosp,  pacf = TRUE, col=2:7, lwd=4, theme = color_theme())

#Step 4 : Build the ARIMA model------------------------------------------------ 

#Manual method
#But its not used as the automated model picks the best based on AIC
arima_fit_hosp_1 = Arima(daily_hosp_hb_timeseries, order = c(3,1,2))
arima_fit_hosp_2 = Arima(daily_hosp_hb_timeseries, order = c(3,1,0))
arima_fit_hosp_3 = Arima(daily_hosp_hb_timeseries, order = c(0,1,2))

#Automated method
#Lag is used to best fit the model
auto_arima_fit_hosp <- auto.arima(lag(daily_hosp_hb_timeseries),
                                  seasonal=FALSE,
                                  stepwise=FALSE, 
                                  approximation=FALSE,
                                  trace = TRUE
)

#Finding the co-efficient
coef<-lmtest::coeftest(auto_arima_fit_hosp)

#Step 5: Check the diagnostics
res <- checkresiduals(auto_arima_fit_hosp, theme = color_theme())

#Step 6: Plot the actual data and fitted data-----------------------------------

# Convert model and time series to dataframe for plotting

daily_hosp_hb_timeseries_data <- fortify(daily_hosp_hb_timeseries) %>% 
  clean_names() %>% 
  remove_rownames %>% 
  rename (date = index,
          hosp = data)%>% 
  mutate(index = seq(1:nrow(daily_hosp_hb_timeseries)))

arima_fit_resid <- ts(daily_hosp_hb_timeseries[1:nrow(daily_hosp_hb_timeseries)]) - resid(auto_arima_fit_hosp)

arima_fit_data <- fortify(arima_fit_resid) %>% 
  clean_names() %>% 
  mutate(data = round(data,2))

fit_existing_data <- daily_hosp_hb_timeseries_data %>% 
  inner_join(arima_fit_data, by = c("index"))

#plotting the series along with the fitted values

fit_existing_hosp_plot <- fit_existing_data %>% 
  mutate (date = as.Date(date)) %>% 
  ggplot()+
  aes(x=date, y = hosp)+
  geom_line(color ="#5ab4ac")+
  geom_line(aes(x= date, y = data), colour = "red" )+
  xlab("Month") + 
  ylab("Patient Hospitalised")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  #ggtitle("Fitting the ARIMA model with existing data") +
  scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  #scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))+
  color_theme()

#Step 7: Forecast the Model for 2 weeks-----------------------------------------

forecast_model <- forecast(auto_arima_fit_hosp,level = c(80, 95), h = 15) 

# Negative values of the CI interval are considered as 0
#Convert the model to dataframe for plotting
forecast_model_data <- fortify(forecast_model) %>% 
  clean_names() %>% 
  mutate(data = round(data,2),
         fitted= round(fitted,2))  %>% 
  mutate (lo_80 = ifelse(lo_80 < 0,0,lo_80),
          lo_95 = ifelse(lo_95 < 0,0,lo_95)
  )

forecast_start_date <- as.Date(max(daily_hosp_hb_timeseries_data$date)+1)
forecast_end_date <- as.Date(forecast_start_date+14)

forecast_data <- forecast_model_data %>% 
  filter(!(is.na(point_forecast))) %>% 
  mutate(date = seq(forecast_start_date,forecast_end_date, by =1)) %>% 
  select(-data,-fitted, -index)  

fitted_data <- forecast_model_data %>% 
  filter(!(is.na(data))) %>% 
  inner_join(daily_hosp_hb_timeseries_data, by = c("index")) %>% 
  mutate(date = as.Date(date)) %>% 
  select(date, data, fitted) 

#Plotting the hospitalization plus the forecast with prediction intervals

annotation <- data.frame(
  x = c(as.Date("13-08-2021","%d-%m-%Y"),as.Date("31-10-2021","%d-%m-%Y")),
  y = c(180,200),
  label = c("PAST", "FUTURE")
)

#Time series plots for the next 15 days according to best ARIMA models with 80% and 95% CI.
forecast_data_hosp_plot <-fitted_data %>% 
  ggplot()+
  geom_line(aes(x= date, y = data), color = "#5ab4ac")+
  geom_line(aes(x= date, y =point_forecast), color ="blue", size = 0.5,data = forecast_data )+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_80, ymax = hi_80),data = forecast_data, alpha = 0.3, fill = "green")+
  geom_ribbon(aes(x = date, y = point_forecast, ymin = lo_95, ymax = hi_95), data = forecast_data, alpha = 0.1)+
  geom_vline(aes(xintercept=as.numeric(max(date))),color="#f1a340", linetype="dashed",data = fitted_data)+
  #ggtitle("Projection of Hospitalisation") +
  xlab("Month") + 
  ylab("Patient Hospitalised")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  color_theme()+
  scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  geom_text(data=annotation, 
            aes( x=x, y=y, label=label),                  
            color="blue", 
            size=4 )


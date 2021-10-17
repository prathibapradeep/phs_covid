#-------------------------------------------------------------------------- 
# This script contains necessary functions for covid analysis
# Created by Prathiba
# Date : 14-Oct-2021
#--------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Function to create a theme for the plot
#-------------------------------------------------------------------------------
color_theme <- function() {
  theme(
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(size = 14, face = "bold",),
    plot.title.position = "plot",
    panel.border = element_rect(colour = "blue", fill = NA, linetype = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "grey85", linetype = 1, size = 0.5),
    axis.text = element_text(colour = "blue", face = "italic", size = 12),
    axis.title.y = element_text(colour = "#1B732B", size = 10, angle = 90),
    axis.title.x = element_text(colour = "#1B732B", size = 10),
    legend.box.background = element_rect(),
    legend.box.margin = margin(6, 6, 6, 6)
  )
}

#-------------------------------------------------------------------------------
#List attributes of x-axis for Plot_ly
#-------------------------------------------------------------------------------
xaxis_list =list(
  title = "Date", color = '#f1a340',
  showline = TRUE, showgrid = TRUE,linecolor = 'blue' ,
  showticklabels = TRUE,  mirror = TRUE, 
  # Range Selector list
  rangeselector = list(
    buttons = list(
      list(
        count = 3,
        label = "3 mo",
        step = "month",
        stepmode = "backward"),
      list(
        count = 6,
        label = "6 mo",
        step = "month",
        stepmode = "backward"),
      list(
        count = 1,
        label = "1 yr",
        step = "year",
        stepmode = "backward"),
      list(
        count = 1,
        label = "YTD",
        step = "year",
        stepmode = "todate"),
      list(step = "all"))),
  # Range Slider list
  rangeslider = list(type = "date"))
#-------------------------------------------------------------------------------
#List attributes of y-axis for Plot_ly
#-------------------------------------------------------------------------------
yaxis_list <- list(showline = TRUE,
                   showgrid = TRUE,
                   mirror = TRUE, 
                   linecolor = 'blue' ,
                   color = '#f1a340')

#-------------------------------------------------------------------------------
# Function to calculate rolling average based on the attribute
#
# Input : Attribute 
#             positive - Daily Positive cases
#             hospital - Daily Hospitalisations
#             death    - Daily Deaths
#             la_neighbour - Local Authority Neighborhood
#Output: Data set with date, attribute and seven day rolling average
#-------------------------------------------------------------------------------
calculate_roll_avg <- function (attrib){
  
if(attrib == "positive")  {
  trend_hb_daily_roll_avg <- trend_hb_daily %>% 
    filter (hb_name == "Scotland") %>% 
    mutate(seven_day_roll_avg = round(rollmean(daily_positive, 7, fill = list(NA, NULL, NA), align = "right"),2)) %>% 
    select(date, daily_positive,seven_day_roll_avg)
  return (trend_hb_daily_roll_avg)
}  
if(attrib =="la_neighbour") {
  
  trend_la_daily_nb <- trend_la_daily %>% 
    group_by(ca_name) %>% 
    mutate(seven_day_roll_avg = round(rollmean(daily_positive, 7, 
                                               fill = list(NA, NULL, NA),
                                               align = "right"), digit =2),
           crude_rate7day_positive = round(crude_rate7day_positive, 2)) %>% 
    select(ca, date, ca_name, daily_positive, seven_day_roll_avg, crude_rate_positive, crude_rate7day_positive) %>% 
    arrange(desc(date)) %>% 
    slice_head(n=1) %>% 
    ungroup()
  return (trend_la_daily_nb)
  
} 
  if(attrib == "hospital") {
    trend_hb_hosp_roll_avg <- trend_hb_daily %>% 
      filter (hb_name == "Scotland") %>% 
      mutate(seven_day_roll_avg = round(rollmean(hospital_admissions, 7, fill = list(NA, NULL, NA), align = "right"),2)) %>% 
      select(date, hospital_admissions,seven_day_roll_avg)
     return (trend_hb_hosp_roll_avg)
  }

  if (attrib == "death") {
    trend_hb_death_roll_avg <- trend_hb_daily %>% 
      filter (hb_name == "Scotland") %>% 
      mutate(seven_day_roll_avg = round(rollmean(daily_deaths, 7, fill = list(NA, NULL, NA), align = "right"),2)) %>% 
      select(date, daily_deaths,seven_day_roll_avg)
    return (trend_hb_death_roll_avg)
  }
}

#-------------------------------------------------------------------------------
# Function to convert the data to time series to display various timeframe in plotly
# The plot provides an option to control the display of data based on the input chosen.
# So the data is converted to Time Series to handle the same.
#-------------------------------------------------------------------------------
convert_to_timeseries <- function (data){
  #convert the data frame to xts (extended time series) to use the range slider and selector
  data_xts <- xts::xts(data[,-1], order.by = data$date) 
  
  #create a data frame to display
  data_ts <- data.frame(Date = index(data_xts),
                        data_xts[,1],
                        data_xts[,2])
  return(data_ts)
}

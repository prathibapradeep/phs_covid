
#Load the library---------------------------------------------------------------
library(here)
library(ggplot2)
library(plotly)
library(fable)
library(fabletools)
library(tsibble)
library(RColorBrewer)
library(leaflet)
library(tseries)
library(forecast)
library(zoo)
library(dlm)
library(urca)
library(ggfortify)
library(gridExtra)
library(astsa)

#Load the data------------------------------------------------------------------
source(here("cleaning_scripts/phs_covid_cleaning.R"))
source(here("analysis/phs_covid_functions.R"))

# ***Plot1(a): Trend on people who tested positive*** 

#Calculate rolling average for Health board equals Scotland   
trend_hb_daily_roll_avg <- calculate_roll_avg("positive")

#convert the data frame to time series to use the range slider and selector
trend_hb_daily_roll_avg_ts <- convert_to_timeseries(trend_hb_daily_roll_avg)

# Create the plot using plot_ly
trend_positive_plot <- plot_ly(trend_hb_daily_roll_avg_ts, x = ~Date,
                          hoverinfo ="text",
                          text = ~paste("Date: ", Date,"<br>",
                                        "Daily Positive: ", daily_positive,"<br>",
                                        "Seven Day Average: ",seven_day_roll_avg))  
#add bar chart for daily positive
trend_positive_plot <- trend_positive_plot %>% add_bars(y = ~daily_positive, 
                                              name = "Daily Positive",
                                              color  =  I("#5ab4ac")  
)
#add line for rolling average
trend_positive_plot <- trend_positive_plot %>% add_lines(y = ~seven_day_roll_avg,
                                               name = "Seven Day Rolling Average",
                                               color = I("red")) 
#add layout to the existing plot
trend_positive_plot <- trend_positive_plot %>% layout(
  title = list(text ="Trend on Positive Cases"),
  xaxis = xaxis_list,
  yaxis = yaxis_list,
  legend = list(title=list(text='<b> Trend </b>'),
                x = 0.1, y = 0.9))

# ***Plot1(b): Total Positive Case by neighborhood (Based on Local Authority).***

trend_la_daily_nb <- calculate_roll_avg("la_neighbour")

# Get the population data  
la_population <-trend_seven_day %>% 
  filter(date == max(date)) %>% 
  group_by (ca) %>% 
  summarise(total_population = sum(population))

# Prepare the map data by combining spatial data and (local authority) la data  
map_data <- zones_la %>% 
  inner_join(trend_la_daily_nb, by = c("code" = "ca"))%>% 
  inner_join(la_population, by = c("code" = "ca")) %>% 
  arrange(seven_day_roll_avg)

# Create a map palette  
map_palette <- colorNumeric("plasma", domain = range(map_data$seven_day_roll_avg))
map_data <- map_data %>% 
  mutate(colour = map_palette(seven_day_roll_avg))

# Display the spatial data
map_data <- map_data %>%
  leaflet() %>%
  addPolygons(
    popup = ~ str_c("<b><h2>",name,  "</h2>", 
                    "<h3>(01-Oct-2011 to 07-Oct-2021)</h3></b>", 
                    "<b>Number of Positive Cases over 7 days: </b>", seven_day_roll_avg,
                    " <br><b>7 day rate per 100,000 people: </b>", crude_rate7day_positive,
                    " <br><br><b>Total Population: </b>", total_population,
                    
                    sep = ""),
    color = ~colour
  ) %>%
  addLegend(
    position = "topright",
    colors = ~colour,
    labels = ~name
  )


# **2 Analyse the trend on Hospitalizations:**

#Calculate rolling average for Health board equals Scotland   
trend_hb_hosp_roll_avg <- calculate_roll_avg("hospital")

#convert the data frame to time series to use the range slider and selector
trend_hb_hosp_roll_avg_ts <- convert_to_timeseries(trend_hb_hosp_roll_avg)

# Create the plot using plot_ly
trend_hospital_plot <- plot_ly(trend_hb_hosp_roll_avg_ts, x = ~Date,
                          hoverinfo ="text",
                          text = ~paste("Date: ", Date,"<br>",
                                        "Patients admitted: ", hospital_admissions,"<br>",
                                        "Seven Day Average: ",seven_day_roll_avg))  
#add bar chart for daily positive
trend_hospital_plot <- trend_hospital_plot %>% add_bars(y = ~hospital_admissions, 
                                              name = "Patients admitted",
                                              color  =  I("#5ab4ac")  
)
#add line for rolling average
trend_hospital_plot <- trend_hospital_plot %>% add_lines(y = ~seven_day_roll_avg,
                                               name = "Seven Day Rolling Average",
                                               color = I("red")) 
#add layout to the existing plot
trend_hospital_plot <- trend_hospital_plot %>% layout(
  title = list(text ="Hospitalisation"),
  xaxis = xaxis_list,
  yaxis = yaxis_list,
  legend = list(title=list(text='<b> Trend </b>'),
                x = 0.1, y = 0.9))

# **2 Analyse the trend on Deaths:**


#Calculate rolling average for Health board equals Scotland   
trend_hb_death_roll_avg <- calculate_roll_avg("death")

#convert the data frame to time series to use the range slider and selector
trend_hb_death_roll_avg_ts <- convert_to_timeseries(trend_hb_death_roll_avg)

# Create the plot using plot_ly
trend_death_plot <- plot_ly(trend_hb_death_roll_avg_ts, x = ~Date,
                       hoverinfo ="text",
                       text = ~paste("Date: ", Date,"<br>",
                                     "Daily Deaths: ", daily_deaths,"<br>",
                                     "Seven Day Average: ",seven_day_roll_avg))  
#add bar chart for daily positive
trend_death_plot <- trend_death_plot %>% add_bars(y = ~daily_deaths, 
                                        name = "Daily Deaths",
                                        color  =  I("#5ab4ac")  
)
#add line for rolling average
trend_death_plot <- trend_death_plot %>% add_lines(y = ~seven_day_roll_avg,
                                         name = "Seven Day Rolling Average",
                                         color = I("red")) 
#add layout to the existing plot
trend_death_plot <- trend_death_plot %>% layout(
  title = list(text ="Deaths reported"),
  xaxis = xaxis_list,
  yaxis = yaxis_list,
  legend = list(title=list(text='<b> Trend </b>'),
                x = 0.1, y = 0.9))

# **3 Analyse the trend on Vaccination:**

trend_vacc_hb <- daily_vacc_hb %>% 
  filter(hb_name == "Scotland") %>% 
  filter(sex =="Total") %>% 
  filter(age_group == "All vaccinations") %>% 
  filter(cumulative_number_vaccinated!=0) 


### ***Plot3(a): Trend on Vaccination(For all the data).***

#Plot to visualize vaccination.
plot_vaccine <- trend_vacc_hb %>% 
  ggplot()+
  aes(x = date, y = number_vaccinated)+
  geom_line(aes(color = dose))+
  scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle("Trend on Vaccination") +
  xlab("Year") +
  ylab("No of Positive Cases") +
  color_theme()+
  scale_colour_manual(values = c("#f1a340", "#5ab4ac"))

trend_vaccine_plot <- ggplotly(plot_vaccine)

#Plot to visualise cumulative vaccinated trend.
plot_vaccine_cumm <- trend_vacc_hb %>% 
  ggplot()+
  aes(x = date, y = cumulative_number_vaccinated)+
  geom_line(aes(color = dose))+
  scale_x_date(breaks = "1 month", date_labels = "%b - %y" )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle("Cummulative Trend on Vaccination") +
  xlab("Year") +
  ylab("No of People Vaccinated") +
  color_theme()+
  scale_colour_manual(values = c("#f1a340", "#5ab4ac"))+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))

trend_vaccine_cumm <- ggplotly(plot_vaccine_cumm)


#------------------------------------------------------------------------------- 
#This script is to load, clean and simplify the health board spatial data.
#-------------------------------------------------------------------------------
# Load the dataset --------------------------------------------------------
library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(sf)

#Load all the datafiles into dataset using loop-----------------------------

## List the files.

setwd("../raw_data")
files <- list.files(pattern=".csv")
## read data using loop
for (f in files) {
  file_name <- str_to_lower(str_replace(f,".csv",""))
  assign(paste(file_name),read_csv(f))
}

#Clean the columns and perform necessary data wrangling----------------------

#Prepare the data for daily positive, death and hospitalisations
trend_hb_daily <- trend_hb_20211008 %>% 
  clean_names()

#Convert the date to date format
trend_hb_daily_all <- trend_hb_daily %>% 
  mutate(date = as_date(ymd(date)))

#Convert the date to date format 
#(For analysis we filter the data to 2021)
trend_hb_daily <- trend_hb_daily %>% 
  mutate(date = as_date(ymd(date))) %>% 
  filter (year(date) == 2021)

#Prepare the data for daily positive, death and hospitalisations
trend_la_daily <- trend_ca_20211008 %>% 
  clean_names()

#Convert the date to date format
trend_la_daily_all <- trend_la_daily %>% 
  mutate(date = as_date(ymd(date)))

#Convert the date to date format 
#(For analysis we filter the data to 2021)
trend_la_daily <- trend_la_daily %>% 
  mutate(date = as_date(ymd(date))) %>% 
  filter (year(date) == 2021)

#Prepare the data for vaccinations
daily_vacc_hb <- daily_vacc_hb_20211009 %>% 
  clean_names()

#Convert the date to date format
daily_vacc_hb <- daily_vacc_hb %>% 
  mutate(date = as_date(ymd(date)))

daily_vacc_hb_plot <- daily_vacc_hb %>% 
  filter(hb_name == "Scotland") %>% 
  filter(sex =="Total") %>% 
  filter(age_group == "All vaccinations") %>% 
  filter(number_vaccinated!=0) 
#select(date,sex, age_group, number_vaccinated)

# Function to create a theme for the plot-----------------------------------------
color_theme <- function() {
  theme(
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(size = rel(2)),
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


#read the shapefile and clean column names

zones_nhs_hb <- read_sf(here("raw_data/zones/SG_NHS_HealthBoards_2019.shp")) %>% 
  clean_names()

#Transform the projection to match leaflet() requirements
#Then simplify geometries for quicker mapping
#NB do not use these simplified geometries for analysis

zones_nhs_hb <- zones_nhs_hb %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(geometry = st_simplify(geometry, dTolerance = 500)) %>% 
  rename(name = hb_name, code = hb_code)

#Use this code to save the output in a new file.  Remember to specify filepath

zones_nhs_hb %>% st_write(here("clean_data/zones/zones_nhs_hb.shp"), append = FALSE)


#read the shapefile and clean column names

zones_la <- read_sf(here("raw_data/localauthority/pub_las.shp")) %>% 
  clean_names()

#Transform the projection to match leaflet() requirements
#Then simplify geometries for quicker mapping
#NB do not use these simplified geometries for analysis

zones_la <- zones_la %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(geometry = st_simplify(st_make_valid(geometry), dTolerance = 500)) %>% 
  rename(name = local_auth)

#Use this code to save the output in a new file.  Remember to specify filepath

zones_la %>% st_write(here("clean_data/localauthority/zones_la_simple.shp"), append = FALSE)
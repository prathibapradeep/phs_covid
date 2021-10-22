#-------------------------------------------------------------------------- 
# This script is to load the data and spatial files and to clean the data
# Created by Prathiba
# Date : 11-Oct-2021
#--------------------------------------------------------------------------

# Load the dataset --------------------------------------------------------
library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(sf)

#Load all the datafiles into dataset using loop-----------------------------

## List the files.
setwd(here::here("raw_data"))
files <- list.files(pattern=".csv")
## read data using loop
for (f in files) {
  file_name <- str_to_lower(str_replace(f,".csv",""))
  assign(paste(file_name),read_csv(f))
}

#Clean the columns and perform necessary data wrangling----------------------

#Prepare the data for daily positive, death and hospitalizations
trend_hb_daily <- trend_hb_20211008 %>% 
  clean_names() %>% 
  mutate(date = as_date(ymd(date))) %>% 
  filter(date <"2021-10-01")

#Prepare the data for daily positive, death and hospitalizations
trend_la_daily <- trend_ca_20211008 %>% 
  clean_names() %>% 
  mutate(date = as_date(ymd(date))) %>% 
  filter(date <"2021-10-01")

#Prepare the data for population using seven day trend data
trend_seven_day <- trend_iz_20211008 %>% 
  clean_names() %>% 
  mutate(date = as_date(ymd(date))) %>% 
  filter(date <"2021-10-01")

#Prepare the data for vaccinations
daily_vacc_hb <- daily_vacc_hb_20211009 %>% 
  clean_names() %>% 
  mutate(date = as_date(ymd(date))) %>% 
  filter(date <"2021-10-01")

#read the shape file and clean column names

zones_la <- read_sf(here("raw_data/localauthority/pub_las.shp")) %>% 
  clean_names()

#Transform the projection to match leaflet() requirements
#Then simplify geometries for quicker mapping
#NB do not use these simplified geometries for analysis

zones_la <- zones_la %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(geometry = st_simplify(st_make_valid(geometry), dTolerance = 500)) %>% 
  rename(name = local_auth)

#Write this into csv for future reference------------------------------------
zones_la %>% st_write(here("clean_data/localauthority/zones_la_simple.shp"), append = FALSE)
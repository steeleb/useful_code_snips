## rnoaa codesnip for station list and selection of variables
library(rnoaa)
library(tidyverse)
# rnoaa station list

#store stations
stationlist=ghcnd_stations()

#look at parameters
unique(stationlist$element)

stationlist_NH = stationlist %>%
  filter(state == 'NH') %>%
  filter(element == 'TMAX'|element == 'TMIN')

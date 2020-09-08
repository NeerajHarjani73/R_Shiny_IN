library(tidyverse)
library(stringr)
library(magrittr)
library(dplyr)
library(readxl)
library(hablar)
library(htmltools)
library(sf)
library(leaflet)
library(dplyr)
library(rgdal)
library(shinydashboard)
library(ggplot2)
options(scipen = 999)
##############################################################################
#Reading India's State csv
df <-
  read.csv("C:\\Users\\neera\\Documents\\Codes\\India_state_list.csv")
location <-
  read_excel("C:\\Users\\neera\\Documents\\Codes\\location.xlsx")
df <- df %>%
  convert(chr(State.or.union.territory))

population_df <- df %>%
  dplyr::select(Rank,
                State.or.union.territory,
                Population,
                Rural.population,
                Urban.population)

#Getting Shapefile
map <-
  readOGR("C:\\Users\\neera\\Downloads\\maps-master\\maps-master\\States\\Admin2.shp",
          stringsAsFactors = FALSE)
map2 <- st_read("C:\\Users\\neera\\Downloads\\maps-master\\maps-master\\States\\Admin2.shp",
                stringsAsFactors = FALSE)

map_df <- fortify(map2)

#Realized that naming convention in shapefile and population_df is different,make it similar

sort(population_df$State.or.union.territory) == sort(map$ST_NM)

population_df$State.or.union.territory[34] = 'Andaman & Nicobar'
population_df$State.or.union.territory[20] = 'Jammu & Kashmir'

population_df <-
  population_df[order(match(population_df$State.or.union.territory, map$ST_NM)), ]

#Merging geojson with data for ggplot
combined_df <- merge(
  x = map_df,
  y = population_df,
  by.x = "ST_NM",
  by.y = "State.or.union.territory",
  all = TRUE
)

combined_df %<>% mutate_if(is.integer,as.numeric)
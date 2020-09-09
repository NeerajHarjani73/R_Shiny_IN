library(tidyverse)
library(rvest)
library(stringr)
library(magrittr)
library(dplyr)
library(readxl)
library(hablar)
library(htmltools)

#target url
url <-
  xml2::read_html(
    "https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population"
  )

#scrapping the table by 'XPath'
table <- url %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all('//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  rvest::html_table()

#Storing the table in global environment
df <- table[[1]]

#Data Cleaning
glimpse(df)

#Removing the total row
df <- df[-c(37),]

df[25, 2] <- gsub("\\[.*?\\]", "", df[25, 2])
df[10, 3] <- gsub("\\[.*?\\]", "", df[10, 3])
df[, 4] <- str_remove(df %>% pull(`Population (%)`), "[%]")
df[, 5] <-
  str_remove(df %>% pull(`Decadal growth(2001-2011)`), "[%]")
df[, 7] <- str_remove(df %>% pull(`Percent rural`), "[%]")
df[, 9] <- str_remove(df %>% pull(`Percent urban`), "[%]")
df[19, 11] <- gsub("\\[.*?\\]", "", df[19, 11])
df[20, 11] <- gsub("\\[.*?\\]", "", df[20, 11])
df[35, 11] <- gsub("\\[.*?\\]", "", df[35, 11])


#Seperating Area in km2 and sq mi
df <- df %>%
  separate('Area[16]', c("Area(km2)", "Area(sq mi)"), "km2")
df[, 11] <- str_remove(df %>% pull(`Area(sq mi)`), "[sqmi()]")

#Seperating Density in km2 and sq mi
df <- df %>%
  separate('Density[a]', c("Density(/km2)", "Denisty(/sq mi)"), "/km2")
df[, 13] <- str_remove(df %>% pull(`Denisty(/sq mi)`), "[(/sqmi)]")

#Downloading to CSV
write.csv(df,
          "C:\\Users\\neera\\Documents\\Codes\\India_state_list.csv",
          row.names = FALSE)
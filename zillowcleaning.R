#load packages. install with install.packages('x') if you don't have
library(readxl)
library(RCurl)
library(XML)
library(dplyr)
library(stringr)
library(tidyverse)
library(xlsx)
library(ggmap)

#download the scraped excel file from Google Drive
download.file("https://drive.google.com/uc?authuser=0&id=1DPQNV32UQQBLnZwsDPZIxddP1mRr9La5&export=download", 
              destfile = "zillowdata_dff.xlsx", mode = "wb")
zillowdata_dff <- read_excel("zillowdata_dff.xlsx")

View(zillowdata_dff)

#Transfer to another dataframe to be worked with
zillowdata_reg <- zillowdata_dff

#Here I clean and transform a few variables I might want to use for regression or visualization
#Cleaning lot variable with an ifelse statement that converts lot column to numeric sqft data
zillowdata_reg$lot <- ifelse(grepl("acre", zillowdata_reg$lot), 
                             as.numeric(gsub("[ acres]|[,]", "", zillowdata_reg$lot))*43560, 
                             as.numeric(gsub("[ sqft]|[,]", "", zillowdata_reg$lot))
)

#Tidy up some bathroom numbers where half-bathrooms were not accounted for
zillowdata_reg$tfhalf <- zillowdata_reg$halfba
zillowdata_reg$baths <- ifelse((zillowdata_reg$tfhalf = 0 | is.na(zillowdata_reg$tfhalf)), 
                               zillowdata_reg$baths,
                               zillowdata_reg$baths - (as.numeric(zillowdata_reg$halfba)*0.5)
)

#create a new variable that taxes the highest of 3 nearby school ratings 
zillowdata_reg$maxschool <- pmax(zillowdata_reg$school1rating, 
                                 zillowdata_reg$school2rating, 
                                 zillowdata_reg$school3rating)

#register your own google key in quotes. Google API keys are wallet connected so I can't make this public 
register_google(key = "x", second_limit = "50")

#geomlocate the zipcodes into latitude and longitutude coordinates for mapping
zillowgeo <- geocode(location = zillowdata_reg$address, output="latlon", source="google")
zillowdata_reg$lon <- zillowgeo$lon
zillowdata_reg$lat <- zillowgeo$lat

#Create a seperate dataframe for display, regression, and visualization purposes
zillowdata_reg2 <- zillowdata_reg %>% select(price, sqft, maxschool, beds, baths, lot, lon, lat)

#write both spread sheets one with isolated variables and one with complete variables
write.xlsx(zillowdata_reg2, "zillowdata_reg2.xlsx")
write.xlsx(zillowdata_reg, "zillowdata_reg.xlsx")

#for cSV output
zillowdata_reg3 <- zillowdata_reg
zillowdata_reg3$address <- gsub("[,]", " ", zillowdata_reg3$address)
zillowdata_reg3[is.na(zillowdata_reg3)] <- '\\N'
write.csv(zillowdata_reg3,"zillowdata_reg3.csv", row.names = FALSE)
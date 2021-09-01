##load packages. install with install.packages('x') if you don't have
library(readxl)
library(RCurl)
library(XML)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(ggiraphExtra)

#download the data file I previously made from Google Docs
download.file("https://drive.google.com/uc?authuser=0&id=18Ji5Lxqw6TWBNAcKykZP7G8mSSQOj5qC&export=download", 
              destfile = "zillowdata_reg.xlsx", mode = "wb")
zillowdata_reg <- read_excel("zillowdata_reg.xlsx")

#create a box splot comparing the spreads of Bedrooms, Bathrooms, and Highest Nearby School Score
zillowdata_reg %>% select(price, beds, baths, maxschool, sqft, lot, everything()) %>% gather("variable", "value",2:4) %>% 
  ggplot(., aes(x = variable, y = value)) + geom_boxplot() +
  ggtitle("Bedrooms, Bathrooms, and School Score Box Plot") +
  theme(plot.title = element_text(hjust = 0.5))

#A scatter plot of price vs sqft with School score colorization and regression line comparison
ggplot(data=zillowdata_reg, aes(x=sqft, y=price, colour=maxschool)) +
  geom_point(size = 4) +
  scale_colour_gradient(low = "black", high = "yellow") +
  stat_smooth(method = "lm") +
  ggtitle("Price vs Interior Sqft vs School Score") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Interior Size (sqft)") +
  ylab("Price (USD)")

zillowreg2 <- lm(price ~sqft + maxschool, data = zillowdata_reg)
ggPredict(zillowreg2, interactive=FALSE)  +
  ggtitle("Multiple Regression Model") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Interior Size (sqft)") +
  ylab("Price (USD)")

#download a San Pablo map from Google based on coordinates found on Google Maps
sanpablomap <- get_map(location=c(-122.336765, 37.973731), zoom=13, maptype = "roadmap",
                       source='google',color='color')

#Geomap home locations and color them based on price
ggmap(sanpablomap) +
  geom_point(data = zillowdata_reg , aes(x = lon, y = lat, colour = price), size = 4) +
  scale_colour_gradient(low = "black", high = "yellow") +
  ggtitle("San Pablo Single Family House Price Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

#Geomap home locations and color them based on Highest Nearby School Score
ggmap(sanpablomap) +
  geom_point(data = zillowdata_reg , aes(x = lon, y = lat, colour = maxschool), size = 4) +
  scale_colour_gradient(low = "black", high = "yellow") +
  ggtitle("San Pablo Highest Nearby School Score Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

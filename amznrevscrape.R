#please install packages if you don't have them before you library them
#an example format install.packages("polite")

library(rvest)
library(XML)
library(dplyr)
library(stringr)
library(tidyverse)
library(xlsx)
library(httr)
library(RCurl)
library(polite)

#enter url here wrapped in quotes 'x'
amznrev_url <- 'https://www.macrotrends.net/stocks/charts/AMZN/amazon/revenue'

#polite bowing to check scrape permissions
amznrev_session <- bow(
  url = "https://www.macrotrends.net/stocks/charts/AMZN/amazon/revenue",
  user_agent = "resumepractice",
  force = TRUE
)

#scrape the page
amznrev_page <- nod(amznrev_session, amznrev_url) %>%
  scrape()

#find and dataframe the table that contains Quarterly Revenue
amznquarterlyrev_table <- html_table(amznrev_page) %>%
  .[str_detect(.,"Quarterly Revenue")==TRUE] %>%
  data.frame()

#fix column names
colnames(amznquarterlyrev_table) <- c("Quarter", 
                                      "Revenue_Millions_of_USD")

#remove dollar sign and commas from revenue value
amznquarterlyrev_table$Revenue_Millions_of_USD <- gsub("\\$|\\,",
                                                       "", 
                                                       amznquarterlyrev_table$Revenue_Millions_of_USD)

#write data frame to an excel file
write.xlsx(amznquarterlyrev_table, 'amznquarterlyrev_table.xlsx')
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

#insert link here wrapped in quotes 'x'
sanpablohomesurl <- 'https://www.zillow.com/san-pablo-ca/houses/?searchQueryState={%22pagination%22:{},%22usersSearchTerm%22:%22San%20Pablo%20CA%22,%22mapBounds%22:{%22west%22:-122.39115475482413,%22east%22:-122.28558300799796,%22south%22:37.908852460772124,%22north%22:38.02172182365078},%22mapZoom%22:13,%22regionSelection%22:[{%22regionId%22:9255,%22regionType%22:6}],%22isMapVisible%22:true,%22filterState%22:{%22isManufactured%22:{%22value%22:false},%22isCondo%22:{%22value%22:false},%22isMultiFamily%22:{%22value%22:false},%22isApartment%22:{%22value%22:false},%22isLotLand%22:{%22value%22:false},%22isTownhouse%22:{%22value%22:false}},%22isListVisible%22:true}'

#polite bowing to check scrape permissions
zillowsession <- bow(
  url = "https://www.zillow.com/",
  user_agent = "resumepractice",
  force = TRUE
)

#scrape the link for the urls of all listings on the front page
sanpablohomesurl <- read_html(sanpablohomesurl)
sphlinknodes <- html_nodes(sanpablohomesurl, 'a.list-card-link.list-card-info')
sphlinks <- html_attr(sphlinknodes, "href")

#loop a scrape through each scraped link with humanized pauses and retry if flagged a robot, resulting in scraped fail
scrapedsphlinks <- lapply(sphlinks, function(webpage){
  zillowsession <- bow(
    url = webpage,
    user_agent = "resumepractice",
    force = TRUE
  )
  Sys.sleep(sample(5,1)+5)
  page <- nod(zillowsession, webpage) %>%
    scrape()
  t <- html_node(page, 'h1.ds-address-container')
  i <- 0
  while (length(t) < 1 & i < 5) {
    Sys.sleep(6)
    nod(zillowsession, webpage) %>%
      scrape()
    i <- i + 1
  }
  return(page)
})

#sort the scraped html code into a data frame
sanpablozillow_dflist <- lapply(scrapedsphlinks,function(webpage){

  price <- html_node(webpage, 'h3.ds-price')
  price <- html_text(price)
  price <- gsub("\\$","", price)
  price <- gsub(",","", price)
  price <- ifelse(length(price) == 0||price == "", NA, price)
  price <- as.numeric(price)
  
  bdbacontainer <- html_nodes(webpage, 'span.ds-bed-bath-living-area')
  bdbacontainer <- html_text(bdbacontainer)
  
  bd <- grep("bd", bdbacontainer, value = TRUE)
  bd <- bd[1]
  bd <- gsub(" bd","", bd)
  bd <- ifelse(length(bd) == 0||bd == "", NA, bd)
  bd <- as.numeric(bd)
  
  ba <- grep("ba", bdbacontainer, value = TRUE)
  ba <- ba[1]
  ba <- gsub(" ba","", ba)
  ba <- ifelse(length(ba) == 0||ba == "", NA, ba)
  ba <- as.numeric(ba)
  
  sqft <- html_nodes(webpage, 'span.ds-bed-bath-living-area')
  sqft <- html_text(sqft)
  sqft <- grep("sqft", sqft, value = TRUE)
  sqft <- sqft[1]
  sqft <- gsub(" sqft","", sqft)
  sqft <- gsub(",","", sqft)
  sqft <- ifelse(length(sqft) == 0||sqft == "", NA, sqft)
  sqft <- as.numeric(sqft)
  
  address <- html_node(webpage, 'h1.ds-address-container')
  address <- html_text(address)
  address <- ifelse(length(address) == 0||address == "", NA, address)
  
  pageoverviewcontainer <- html_nodes(webpage, 'li.ds-overview-stat')
  pageoverviewcontainer <- html_text(pageoverviewcontainer)
  
  days_on_zillow <- grep("Time on Zillow", pageoverviewcontainer, value = TRUE)
  days_on_zillow <- gsub("Time on Zillow","", days_on_zillow)
  days_on_zillow <- gsub(" days","", days_on_zillow)
  days_on_zillow <- ifelse(length(days_on_zillow) == 0||days_on_zillow == "", NA, days_on_zillow)
  days_on_zillow <- as.numeric(days_on_zillow)
  
  page_views <- grep("Views", pageoverviewcontainer, value = TRUE)
  page_views <- gsub("Views","", page_views)
  page_views <- ifelse(length(page_views) == 0||page_views == "", NA, page_views)
  page_views <- as.numeric(page_views)
  
  page_saves <- grep("Saves", pageoverviewcontainer, value = TRUE)
  page_saves <- gsub("Saves","", page_saves)
  page_saves <- ifelse(length(page_saves) == 0||page_saves == "", NA, page_saves)
  page_saves <- as.numeric(page_saves)
  
  homefactlistcontainer <- html_nodes(webpage, 'li.ds-home-fact-list-item')
  homefactlistcontainer <- html_text(homefactlistcontainer)
  
  home_type <- grep("Type:", homefactlistcontainer, value = TRUE)
  home_type <- gsub("Type:","", home_type)
  home_type <- ifelse(length(home_type) == 0||home_type == "", NA, home_type)
  
  year_built <- grep("Year built:", homefactlistcontainer, value = TRUE)
  year_built <- gsub("Year built:","", year_built)
  year_built <- ifelse(length(year_built) == 0||year_built == "", NA, year_built)
  
  heating <- grep("Heating:", homefactlistcontainer, value = TRUE)
  heating <- gsub("Heating:","", heating)
  heating <- ifelse(length(heating) == 0||heating == "", NA, heating)
  
  cooling <- grep("Cooling:", homefactlistcontainer, value = TRUE)
  cooling <- gsub("Cooling:","", cooling)
  cooling <- ifelse(length(cooling) == 0||cooling == "", NA, cooling)
  
  parking <- grep("Parking:", homefactlistcontainer, value = TRUE)
  parking <- gsub("Parking:","", parking)
  parking <- ifelse(length(parking) == 0||parking == "", NA, parking)
  
  lot <- grep("Lot:", homefactlistcontainer, value = TRUE)
  lot <- gsub("Lot:","", lot)
  lot <- ifelse(length(lot) == 0||lot == "", NA, lot)
  
  pricepersqft <- grep("Price/sqft:", homefactlistcontainer, value = TRUE)
  pricepersqft <- gsub("Price/sqft:","", pricepersqft)
  pricepersqft <- gsub("[\\$]","", pricepersqft)
  pricepersqft <- ifelse(length(pricepersqft) == 0||pricepersqft == "", NA, pricepersqft)
  
  detailcontainer <- html_nodes(webpage, 'li')
  detailcontainer <- html_text(detailcontainer)
  
  fullba <- grep("Full bathrooms:", detailcontainer, value = TRUE)
  fullba <- gsub("Full bathrooms:","", fullba)
  fullba <- fullba[2]
  fullba <- gsub(" ","", fullba)
  fullba <- ifelse(length(fullba) == 0||price == "", NA, fullba)
  
  halfba <- grep("1/2 bathrooms:", detailcontainer, value = TRUE)
  halfba <- gsub("1/2 bathrooms:","", halfba)
  halfba <- halfba[2]
  halfba <- gsub(" ","", halfba)
  halfba <- ifelse(length(halfba) == 0||halfba == "", NA, halfba)
  
  stories <- grep("Stories:", detailcontainer, value = TRUE)
  stories <- gsub("Stories:","", stories)
  stories <- stories[2]
  stories <- gsub(" ","", stories)
  stories <- ifelse(length(stories) == 0||stories == "", NA, stories)
  
  newconstruction <- grep("New construction:", detailcontainer, value = TRUE)
  newconstruction <- gsub("New construction:","", newconstruction)
  newconstruction <- newconstruction[2]
  newconstruction <- gsub(" ","", newconstruction)
  newconstruction <- ifelse(length(newconstruction) == 0||newconstruction == "", NA, newconstruction)
  
  majorremodelingyr <- grep("Major remodel year:", detailcontainer, value = TRUE)
  majorremodelingyr <- gsub("Major remodel year:","", majorremodelingyr)
  majorremodelingyr <- majorremodelingyr[2]
  majorremodelingyr <- gsub(" ","", majorremodelingyr)
  majorremodelingyr <- ifelse(length(majorremodelingyr) == 0||majorremodelingyr == "", NA, majorremodelingyr)
  
  roofing <- grep("Roof:", detailcontainer, value = TRUE)
  roofing <- gsub("Roof:","", roofing)
  roofing <- roofing[2]
  roofing <- gsub(" ","", roofing)
  roofing <- ifelse(length(roofing) == 0||roofing == "", NA, roofing)
  
  schoolratingcontainer <- html_nodes(webpage, 'span.ds-hero-headline.ds-schools-display-rating')
  schoolratingcontainer <- html_text(schoolratingcontainer)
  
  school1rating <- as.numeric(schoolratingcontainer[1])
  
  school2rating <- as.numeric(schoolratingcontainer[2])
  
  school3rating <- as.numeric(schoolratingcontainer[3])

  zillowdata_df = data.frame(price = price, beds = bd,
                             baths = ba, sqft = sqft,
                             address = address, days_on_zillow = days_on_zillow,
                             page_views = page_views, page_saves = page_saves,
                             home_type = home_type, year_built = year_built,
                             heating = heating,  cooling = cooling,
                             parking = parking, lot = lot, pricepersqft = pricepersqft,
                             fullba = fullba, halfba = halfba,
                             stories = stories, newconstruction = newconstruction,
                             majorremodelingyr = majorremodelingyr, roofing = roofing,
                             school1rating = school1rating, school2rating = school2rating, 
                             school3rating = school3rating)
})
zillowdata_dff <- bind_rows(sanpablozillow_dflist)
zillowdata_dff$url <- sphlinks

View(zillowdata_dff)

#Write the scraped dataframe into an excel spreadsheet
write.xlsx(zillowdata_dff, 'zillowdata_dff.xlsx')

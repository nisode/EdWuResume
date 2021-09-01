##load packages. install with install.packages('x') if you don't have
library(readxl)
library(RCurl)
library(XML)
library(dplyr)
library(stringr)
library(tidyverse)
library(GGally)
library(mctest)
library(car)
library(stargazer)
library(broom)

#download the data file I previously made from Google Docs
download.file("https://drive.google.com/uc?authuser=0&id=18Ji5Lxqw6TWBNAcKykZP7G8mSSQOj5qC&export=download", 
              destfile = "zillowdata_reg.xlsx", mode = "wb")
zillowdata_reg <- read_excel("zillowdata_reg.xlsx")

#make 3 regression models with progressively more variables
zillowreg1 <- lm(price ~sqft, data = zillowdata_reg)
zillowreg2 <- lm(price ~sqft + maxschool, data = zillowdata_reg)
zillowreg3 <- lm(price ~sqft + maxschool + beds + baths + lot, data = zillowdata_reg)
summary(zillowreg3)

#create a stargazer latex html code which is often seen in Academic Papers
#shows regression results and tables it neatly
stargazer(zillowreg1, zillowreg2, zillowreg3, type="html", title="Regression Results", align=TRUE, dep.var.labels = "Price (USD)", covariate.labels=c("Interior Size (sqft)","Highest Nearby School Rating (x/10)", "No. of Bedrooms","No. of Bathrooms","Lot Size (sqft)"))

#check the regression variables correlation against each other
zillowregx <- zillowdata_reg %>% select(sqft,maxshool,beds,baths,lot)
ggpairs(zillowregx)

#vif check for multicolinearity analysis
vifcheckex <- imcdiag(zillowregx, zillowdata_reg$price)

#create a regression table that includes vif stats for Model 3
vifcheck <- vif(zillowreg3) %>% tibble(.) %>% 
  mutate(term = names(vif(zillowreg3))) %>% 
  rename('vif'='.')
tidysum <- tidy(summary(zillowreg3))
summaryvif <- left_join(tidysum, vifcheck) 
summaryvif<- round(summaryvif[,2:6],digits=3)
summaryvif$variable <- c("constant","sqft","maxschool","beds","baths","lot")
summaryvif <- summaryvif %>% select(variable, everything())
summaryvif <- summaryvif %>% slice(match(c("sqft","maxschool","beds","baths","lot","constant"),variable))
stargazer(summaryvif, type='html', summary=FALSE, title="Multicolinearity Comparison")

#OLS assumption plots for Model 3
par(mfrow=c(2,2)) 
plot(zillowreg3)
par(mfrow=c(1,1))

#create a regression table that includes vif stats for Model 2
vifcheck2 <- vif(zillowreg2) %>% tibble(.) %>% 
  mutate(term = names(vif(zillowreg2))) %>% 
  rename('vif'='.')
tidysum2 <- tidy(summary(zillowreg2))
summaryvif2 <- left_join(tidysum2, vifcheck2) 
summaryvif2<- round(summaryvif2[,2:6],digits=3)
summaryvif2$variable <- c("constant","sqft","maxschool")
summaryvif2 <- summaryvif2 %>% select(variable, everything())
summaryvif2 <- summaryvif2 %>% slice(match(c("sqft","maxschool","constant"),variable))
stargazer(summaryvif2, type='html', summary=FALSE, title="Multicolinearity Comparison (Model 2)")

#OLS assumption plots for Model 2
par(mfrow=c(2,2)) 
plot(zillowreg2)
par(mfrow=c(1,1))
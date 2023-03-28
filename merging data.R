setwd("/Users/rasheed/Documents/GitHub/FinalProject")

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
###Merging datasets
library(wbstats)
library(countrycode)
library(tidyr)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#combining waves 5, 6, and 7-----------------------------------------------------------------------
all.waves <- rbind()
for (i in 5:7){
  all.waves <- rbind(all.waves, read.csv(paste0("wave",i,"_cleaned.csv")))
}

##creating country code
all.waves$iso3c <- countrycode(sourcevar=all.waves$COUNTRY, origin="country.name", destination="iso3c")

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#importing vdem data-----------------------------------------------------------------------
vdem <- read.csv("vdem.csv")

##choosing only the liberal democracy index

libdem<- subset(vdem, year>=2010, select= c(year, v2x_libdem, COWcode, country_name))

##renaming the year variable before reshaping (pivoting)
libdem$year <- paste0("libdem",libdem$year)

libdem<- pivot_wider(libdem, names_from = year, values_from = v2x_libdem)

## adding countrycode to allow merging with afrobarometer data
libdem$iso3c <- countrycode(sourcevar=libdem$country_name, origin="country.name", destination="iso3c")


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#importing corruption control from wbstates-------------------------------------------------------------

corruption.control <- wb(country = c("all"), indicator = "CC.EST",
                         startdate = 2000, enddate = 2021,
                         return_wide = T)

corruption.control$date <- paste0("cc",corruption.control$date)

#reshaping to have the years/time as individual variables
corruption.control <- pivot_wider(corruption.control, 
                                  names_from = date, values_from = CC.EST)

## merging corruption control variable with afrobarometer based on the countrycodes
merged.data<- merge(all.waves, corruption.control,
                 by="iso3c", all.x=T, all.y=F)


## merging the data  with liberal democracy data based on the countrycodes
merged.data<- merge(merged.data, libdem ,
                    by="iso3c", all.x=T, all.y=F)


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

## Different waves of afrobarometer were conducted in different years; 
##creating a general corruption control and liberal democracy variables by 
#picking the values of corruption control for year of survey

merged.data$control.cor <- ifelse(merged.data$wave== "wave5", merged.data$cc2012,
                                  ifelse(merged.data$wave== "wave6", merged.data$cc2015,
                                         ifelse( merged.data$wave== "wave7", merged.data$cc2017, NA)))

merged.data$democracy <-  ifelse(merged.data$wave== "wave5", merged.data$libdem2011,
                              ifelse(merged.data$wave== "wave6", merged.data$libdem2014,
                                 ifelse( merged.data$wave== "wave7", merged.data$libdem2016, NA)))


## Export data for analysis
write.csv(merged.data, "forAnalysis.csv")

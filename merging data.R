###Merging datasets
library(wbstats)
library(countrycode)
library(tidyr)

setwd("/Users/rasheed/Desktop/sem 4/CPI/final paper/tests")
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#importing vdem data-----------------------------------------------------------------------
vdem <- read.csv("vdem.csv")
vdem <- subset(vdem, year>=2010)  ##subsetting to more recent democracy

libdem<- subset(vdem, select= c(year, v2x_libdem, COWcode, country_name))


libdem$year <- paste0("libdem",libdem$year)

libdem<- pivot_wider(libdem, names_from = year, values_from = v2x_libdem)
libdem$iso3c <- countrycode(sourcevar=libdem$country_name, origin="country.name", destination="iso3c")

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#combining all 3 waves-----------------------------------------------------------------------
all.waves <- rbind()
for (i in 5:7){
  all.waves <- rbind(all.waves, read.csv(paste0("wave",i,"_cleaned.csv")))
}

all.waves$iso3c <- countrycode(sourcevar=all.waves$COUNTRY, origin="country.name", destination="iso3c")

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#importing corruption control from wbstates-------------------------------------------------------------

corruption.control <- wb(country = c("all"), indicator = "CC.EST",
                         startdate = 2000, enddate = 2021,
                         return_wide = T)

corruption.control$date <- paste0("cc",corruption.control$date)

corruption.control <- pivot_wider(corruption.control, 
                                  names_from = date, values_from = CC.EST)

merged.data<- merge(all.waves, corruption.control,
                 by.x="iso3c", by.y="iso3c", all.x=T, all.y=F)



merged.data<- merge(merged.data, libdem ,
                    by.x="iso3c", by.y="iso3c", all.x=T, all.y=F)


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## People must pay taxes Q48C-----------------------------------------------------------------------
merged.data$control.cor <- ifelse(merged.data$wave== "wave5", merged.data$cc2012,
                                  ifelse(merged.data$wave== "wave6", merged.data$cc2015,
                                         ifelse( merged.data$wave== "wave7", merged.data$cc2017, NA)))
merged.data$democracy <-  ifelse(merged.data$wave== "wave5", merged.data$libdem2011,
                              ifelse(merged.data$wave== "wave6", merged.data$libdem2014,
                                 ifelse( merged.data$wave== "wave7", merged.data$libdem2016, NA)))

mean(merged.data$democracy)


write.csv(merged.data, "forAnalysis.csv")

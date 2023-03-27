#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## downloading and cleaning afrobarometer survey wave 5

setwd("/Users/rasheed/Documents/GitHub/FinalProject")

## downloading data (in spss) directly from website
wave5 <- read.spss( "http://afrobarometer.org/sites/default/files/data/round-5/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav", 
                    to.data.frame = T)
#Cleaning ---------------------------------------------------------------------------------

#wave indicator
wave5$wave <- paste0("wave",5)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

# Q56 Refusal to pay tax
wave5$tax.compliance <- ifelse(wave5$Q26C == "Yes, often", 0,
                          ifelse(wave5$Q26C == "Yes, several times", 1,
                                ifelse(wave5$Q26C == "Yes, once or twice", 2,
                                     ifelse(wave5$Q26C == "No, but would do if had the chance", 3,
                                         ifelse(wave5$Q26C == "No, would never do this", 4, NA)))))
mean(wave5$tax.compliance.1, na.rm=T)
table(wave5$tax.compliance)
wave5$tax.compliance.1 <- ifelse(wave5$Q26C == "Yes, often", 0,
                                 ifelse(wave5$Q26C == "Yes, several times", 0.25,
                                        ifelse(wave5$Q26C == "Yes, once or twice", 0.5,
                                               ifelse(wave5$Q26C == "No, but would do if had the chance", 0.75,
                                                      ifelse(wave5$Q26C == "No, would never do this", 1, NA)))))

wave5$compliance.binary <- ifelse(wave5$Q26C == "Yes, often", 0,
                              ifelse(wave5$Q26C == "Yes, several times", 0,
                                ifelse(wave5$Q26C == "Yes, once or twice", 0,
                                    ifelse(wave5$Q26C == "No, but would do if had the chance", 0,
                                       ifelse(wave5$Q26C == "No, would never do this", 1, NA)))))


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# Q59A Trust the president ---------------------------------------------------------------------------------

wave5$trust.president <- ifelse(wave5$Q59A == "Not at all", 0,
                                ifelse(wave5$Q59A == "Just a little", 1,
                                       ifelse(wave5$Q59A == "Somewhat", 2,
                                              ifelse(wave5$Q59A == "A lot", 3, NA))))


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

# Q59B Trust the parl. ---------------------------------------------------------------------------------
wave5$trust.parl <- ifelse(wave5$Q59B == "Not at all", 0,
                        ifelse(wave5$Q59B == "Just a little", 1,
                               ifelse(wave5$Q59B == "Somewhat", 2,
                                      ifelse(wave5$Q59B == "A lot", 3, NA))))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# Education ---------------------------------------------------------------------------------
### Resume Here
wave5$education <- ifelse(wave5$Q97 == "No formal schooling", 0,
                    ifelse(wave5$Q97 == "Informal schooling only", 1,
                      ifelse(wave5$Q97 == "Some primary schooling", 2,
                        ifelse(wave5$Q97 == "Primary school completed", 3,
                          ifelse(wave5$Q97 == "Some secondary school/high school", 4,
                           ifelse(wave5$Q97 == "Secondary school completed/high school", 5,
                             ifelse(wave5$Q97 == "Post-secondary qualifications, not university", 6,
                              ifelse(wave5$Q97 == "Some university", 7,
                                ifelse(wave5$Q97 == "University completed", 8,
                                 ifelse(wave5$Q97 == "Post-graduate", 9, NA))))))))))

table(wave5$education)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# Q101 gender ---------------------------------------------------------------------------------
wave5$female <- ifelse(wave5$Q101== "Male", 0,
                       ifelse(wave5$Q101 == "Female", 1, NA))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#URBRUR urban ---------------------------------------------------------------------------------

wave5$urban <- ifelse(wave5$URBRUR == "Rural", 0, 1)
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~


# LPI ---------------------------------------------------------------------------------
#no food Q8A
wave5$no.food <- ifelse(wave5$Q8A == "Never", 0,
                   ifelse(wave5$Q8A == "Just once or twice", .25,
                    ifelse(wave5$Q8A == "Several times", .5,
                       ifelse(wave5$Q8A == "Many times", .75,
                            ifelse(wave5$Q8A == "Always", 1, NA)))))
# no water Q8 B
wave5$no.water <- ifelse(wave5$Q8B == "Never", 0,
                    ifelse(wave5$Q8B == "Just once or twice", .25,
                        ifelse(wave5$Q8B == "Several times", .5,
                            ifelse(wave5$Q8B == "Many times", .75,
                               ifelse(wave5$Q8B == "Always", 1, NA)))))
#no meds Q8c
wave5$no.meds <- ifelse(wave5$Q8C == "Never", 0,
                   ifelse(wave5$Q8C == "Just once or twice", .25,
                     ifelse(wave5$Q8C == "Several times", .5,
                       ifelse(wave5$Q8C == "Many times", .75,
                         ifelse(wave5$Q8C == "Always", 1, NA)))))

# no fuel Q8d
wave5$no.fuel <- ifelse(wave5$Q8D == "Never", 0,
                   ifelse(wave5$Q8D == "Just once or twice", .25,
                      ifelse(wave5$Q8D == "Several times", .5,
                        ifelse(wave5$Q8D == "Many times", .75,
                         ifelse(wave5$Q8D == "Always", 1, NA)))))
table(wave5$no.cash)
# no cash Q8e
wave5$no.cash <- ifelse(wave5$Q8E == "Never", 0,
                   ifelse(wave5$Q8E == "Just once or twice", .25,
                      ifelse(wave5$Q8E == "Several times", .5,
                        ifelse(wave5$Q8E == "Many times", .75,
                          ifelse(wave5$Q8E == "Always", 1, NA)))))


wave5$LPI <- wave5$no.cash + wave5$no.food + wave5$no.water + wave5$no.meds + wave5$no.fuel

table(wave5$LPI)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## age ---------------------------------------------------------------------------------
wave5$age<- wave5$Q1
wave5$age <- gsub("[^[:digit:]]", NA, wave5$age)
wave5$age<- as.numeric(wave5$age)
table(wave5$age)
?gsub

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## Q43. Satisfaction with democracy---------------------------------------------------------------------------------

wave5$demo.satisfaction <- ifelse(wave5$Q43 == "The country is not a democracy", 0,
                           ifelse(wave5$Q43 == "Not at all satisfied", .25,
                                 ifelse(wave5$Q43 == "Not very satisfied", .5,
                                     ifelse(wave5$Q43 == "Fairly satisfied", .75,
                                          ifelse(wave5$Q43 == "Very satisfied", 1, NA)))))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## People must pay taxes Q48C-----------------------------------------------------------------------

wave5$compulsory.tax <- ifelse(wave5$Q48C == "Strongly Disagree", 0,
                          ifelse(wave5$Q48C == "Disagree", .25,
                             ifelse(wave5$Q48C == "Neither Agree Nor Disagree", .5,
                                 ifelse(wave5$Q48C == "Agree", .75,
                                    ifelse(wave5$Q48C == "Strongly Agree", 1, NA)))))
table(wave5$compulsory.tax)
table(wave5$Q48C)
##Perceived non-compliance of others ----------------------------------------------------------------------

#Q56I How often people avoid paying taxes
wave5$tax.evasion <- ifelse(wave5$Q56I == "Never", 0,
                        ifelse(wave5$Q56I == "Rarely", .33,
                           ifelse(wave5$Q56I == "Often", .67,
                               ifelse(wave5$Q56I == "Always", 1 , NA))))

table(wave5$tax.evasion)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

#Q94. Employment status

table(wave5$Q96)
table(wave5$employed)
wave5$employed <- ifelse(wave5$Q96 == "No (not looking)"|
                           wave5$Q96 == "No (looking)", 0,
                         ifelse(wave5$Q96 == "Yes, part time"|
                                  wave5$Q96 == "Yes, full time", 1 , NA))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#Q53C Corruption: government officials--------------------------------------------------------------------------
table(wave5$Q60C)
table(wave5$corrupt.officials)

wave5$corrupt.officials <- ifelse(wave5$Q60C == "None", 0,
                                  ifelse(wave5$Q60C == "Some of them", .33,
                                         ifelse(wave5$Q60C == "Most of them", .67,
                                                ifelse(wave5$Q60C == "All of them", 1 , NA))))


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~#
#index for goods and services ---------------------------------------------------------------------------------
#services:
#EA-SVC-A. Electricity
wave5$electric <- ifelse(wave5$EA_SVC_A== "Yes", .1,
                         ifelse(wave5$EA_SVC_A=="No",0,NA))

table(wave5$electric)

#EA-SVC-B. Piped water
wave5$water <- ifelse(wave5$EA_SVC_B== "Yes", .1,
                      ifelse(wave5$EA_SVC_B=="No",0,NA))
#EA-SVC-C. Sewage system in the PSU/EA
wave5$sewage <- ifelse(wave5$EA_SVC_C== "Yes", .1,
                       ifelse(wave5$EA_SVC_C=="No",0,NA))
#EA-SVC-D. Cell phone service in the PSU/EA
wave5$cellphone <- ifelse(wave5$EA_SVC_D== "Yes", .1,
                          ifelse(wave5$EA_SVC_D=="No",0,NA))

#EA-FAC-A. Post office in the PSU/EA
wave5$postoffice <- ifelse(wave5$EA_FAC_A=="Yes",.1,
                           ifelse(wave5$EA_FAC_A=="No",0,NA))

#EA-FAC-B. School in the PSU/EA
wave5$school <- ifelse(wave5$EA_FAC_B=="Yes",.1,
                       ifelse(wave5$EA_FAC_B=="No",0,NA))
#EA-FAC-C. Police station in the PSU/EA
wave5$police <- ifelse(wave5$EA_FAC_C=="Yes",.1,
                       ifelse(wave5$EA_FAC_C=="No",0,NA))
#EA-FAC-D. Health Clinic in the PSU/EA
wave5$clinic <- ifelse(wave5$EA_FAC_D=="Yes",.1,
                       ifelse(wave5$EA_FAC_D=="No",0,NA))
#EA-FAC-E. Market stalls in the PSU/EA
wave5$market <- ifelse(wave5$EA_FAC_E=="Yes",.1,
                          ifelse(wave5$EA_FAC_G=="No",0,NA))


# EA-ROAD-C-A. Tarred road
table(wave5$EA_ROAD)
wave5$goodroad <- ifelse(wave5$EA_ROAD=="Yes",.1,0)
table(wave5$goodroad)

#services index
wave5$serv.index <- wave5$goodroad + wave5$market + wave5$clinic + wave5$police +wave5$school +
  wave5$postoffice + wave5$cellphone + wave5$sewage +wave5$water + wave5$electric
table(wave5$serv.index)
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~


wave5$COUNTRY <- as.character(wave5$COUNTRY) 

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
##subsetting and choosing relevant variables
wave5_cleaned <- subset(wave5, select = c(wave, tax.compliance, tax.compliance.1, compliance.binary, trust.president,
                   trust.parl, education, female,urban, LPI, age, demo.satisfaction, compulsory.tax, tax.evasion, 
                   employed, corrupt.officials, serv.index, COUNTRY))
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## saving the cleaned data

write.csv(wave5_cleaned, "wave5_cleaned.csv")



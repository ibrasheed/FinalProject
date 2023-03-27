#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## downloading and cleaning afrobarometer survey wave 6

setwd("/Users/rasheed/Documents/GitHub/FinalProject")

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## downloading data (in spss) directly from website

wave6<- read.spss("https://www.afrobarometer.org/wp-content/uploads/2022/02/merged_r6_data_2016_36countries2.sav", 
                  to.data.frame = T)

##WAVE6 Cleaning ---------------------------------------------------------------------------------
#wave indicator
wave6$wave <- paste0("wave",6)


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# Refusal to pay tax

# Use an if-else loop to recode the categorical variable to numeric
wave6$tax.compliance <- ifelse(wave6$Q27D == "Yes, often", 0,
                         ifelse(wave6$Q27D == "Yes, several times", 1,
                           ifelse(wave6$Q27D == "Yes, once or twice", 2,
                             ifelse(wave6$Q27D == "No, but would do if had the chance", 3,
                                 ifelse(wave6$Q27D == "No, would never do this", 4, NA)))))


wave6$tax.compliance.1 <- ifelse(wave6$Q27D == "Yes, often", 0,
                              ifelse(wave6$Q27D == "Yes, several times", 0.25,
                                ifelse(wave6$Q27D == "Yes, once or twice", 0.5,
                                       ifelse(wave6$Q27D == "No, but would do if had the chance", 0.75,
                                             ifelse(wave6$Q27D == "No, would never do this", 1, NA)))))

wave6$compliance.binary <- ifelse(wave6$Q27D == "Yes, often", 0,
                             ifelse(wave6$Q27D == "Yes, several times", 0,
                               ifelse(wave6$Q27D == "Yes, once or twice", 0,
                                   ifelse(wave6$Q27D == "No, but would do if had the chance", 0,
                                        ifelse(wave6$Q27D == "No, would never do this", 1, NA)))))


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# Q52a Trust the president ---------------------------------------------------------------------------------

table(wave6$Q52A)
table(wave6$trust.president)
wave6$trust.president <- ifelse(wave6$Q52A == "Not at all", 0,
                                ifelse(wave6$Q52A == "Just a little", 1,
                                       ifelse(wave6$Q52A == "Somewhat", 2,
                                              ifelse(wave6$Q52A == "A lot", 3, NA))))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

# Q52b Trust the parl. ---------------------------------------------------------------------------------
wave6$trust.parl <- ifelse(wave6$Q52B == "Not at all", 0,
                           ifelse(wave6$Q52B == "Just a little", 1,
                                  ifelse(wave6$Q52B == "Somewhat", 2,
                                         ifelse(wave6$Q52B == "A lot", 3, NA))))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# Education ---------------------------------------------------------------------------------

wave6$education <- ifelse(wave6$Q97 == "No formal schooling", 0,
                     ifelse(wave6$Q97 == "Informal schooling only", 1,
                        ifelse(wave6$Q97 == "Some primary schooling", 2,
                           ifelse(wave6$Q97 == "Primary school completed", 3,
                             ifelse(wave6$Q97 == "Some secondary school / high school", 4,
                               ifelse(wave6$Q97 == "Secondary school / high school completed", 5,
                                   ifelse(wave6$Q97 == "Post-secondary qualifications, other than university", 6,
                                       ifelse(wave6$Q97 == "Some university", 7,
                                         ifelse(wave6$Q97 == "University completed", 8,
                                             ifelse(wave6$Q97 == "Post-graduate", 9, NA))))))))))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# gender ---------------------------------------------------------------------------------
wave6$female <- ifelse(wave6$Q101== "Male", 0,
                       ifelse(wave6$Q101 == "Female", 1, NA))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#urban ---------------------------------------------------------------------------------

wave6$urban <- ifelse(wave6$URBRUR == "Rural", 0, 1)
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~


# LPI ---------------------------------------------------------------------------------
#no food Q8A
wave6$no.food <- ifelse(wave6$Q8A == "Never", 0,
                    ifelse(wave6$Q8A == "Just once or twice", .25,
                        ifelse(wave6$Q8A == "Several times", .5,
                           ifelse(wave6$Q8A == "Many times", .75,
                              ifelse(wave6$Q8A == "Always", 1, NA)))))
table(wave6$no.water)
# no water Q8 B
wave6$no.water <- ifelse(wave6$Q8B == "Never", 0,
                         ifelse(wave6$Q8B == "Just once or twice", .25,
                                ifelse(wave6$Q8B == "Several times", .5,
                                       ifelse(wave6$Q8B == "Many times", .75,
                                              ifelse(wave6$Q8B == "Always", 1, NA)))))
#no meds Q8c
wave6$no.meds <- ifelse(wave6$Q8C == "Never", 0,
                        ifelse(wave6$Q8C == "Just once or twice", .25,
                               ifelse(wave6$Q8C == "Several times", .5,
                                      ifelse(wave6$Q8C == "Many times", .75,
                                             ifelse(wave6$Q8C == "Always", 1, NA)))))

# no fuel Q8d
wave6$no.fuel <- ifelse(wave6$Q8D == "Never", 0,
                        ifelse(wave6$Q8D == "Just once or twice", .25,
                               ifelse(wave6$Q8D == "Several times", .5,
                                      ifelse(wave6$Q8D == "Many times", .75,
                                             ifelse(wave6$Q8D == "Always", 1, NA)))))

# no cash Q8e
wave6$no.cash <- ifelse(wave6$Q8E == "Never", 0,
                        ifelse(wave6$Q8E == "Just once or twice", .25,
                               ifelse(wave6$Q8E == "Several times", .5,
                                      ifelse(wave6$Q8E == "Many times", .75,
                                             ifelse(wave6$Q8E == "Always", 1, NA)))))


wave6$LPI <- wave6$no.cash + wave6$no.food + wave6$no.water + wave6$no.meds + wave6$no.fuel



#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## age ---------------------------------------------------------------------------------
wave6$age<- wave6$Q1
wave6$age <- gsub("[^[:digit:]]", NA, wave6$age)
wave6$age<- as.numeric(wave6$age)
table(wave6$age)

###### RESUME HERE

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## Q41. Satisfaction with democracy---------------------------------------------------------------------------------

wave6$demo.satisfaction <- ifelse(wave6$Q41 == "The country is not a democracy", 0,
                                ifelse(wave6$Q41 == "Not at all satisfied", .25,
                                       ifelse(wave6$Q41 == "Not very satisfied", .5,
                                              ifelse(wave6$Q41 == "Fairly satisfied", .75,
                                                     ifelse(wave6$Q41 == "Very satisfied", 1, NA)))))
table(wave6$Demo.Perception)
table(wave6$Q41)
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## People must pay taxes Q42C-----------------------------------------------------------------------

wave6$compulsory.tax <- ifelse(wave6$Q42C == "Strongly Disagree", 0,
                               ifelse(wave6$Q42C == "Disagree", .25,
                                      ifelse(wave6$Q42C == "Neither Agree Nor Disagree", .5,
                                             ifelse(wave6$Q42C == "Agree", .75,
                                                    ifelse(wave6$Q42C == "Strongly Agree", 1, NA)))))
table(wave6$compulsory.tax)
table(wave6$Q42C)
##Perceived non-compliance of others ----------------------------------------------------------------------

## tax evasion question not asked in this wave
wave6$tax.evasion <- NA
table(wave6$Q70B)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

#Q94. Employment status

table(wave6$Q95)
table(wave6$employed)
wave6$employed <- ifelse(wave6$Q95 == "No (not looking)"|
                           wave6$Q95 == "No (looking)", 0,
                         ifelse(wave6$Q95 == "Yes, part time"|
                                  wave6$Q95 == "Yes, full time", 1 , NA))


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#Q53C Corruption: government officials--------------------------------------------------------------------------
table(wave6$Q53C)
table(wave6$corrupt.officials)

wave6$corrupt.officials <- ifelse(wave6$Q53C == "None", 0,
                             ifelse(wave6$Q53C == "Some of them", .33,
                                ifelse(wave6$Q53C == "Most of them", .67,
                                  ifelse(wave6$Q53C == "All of them", 1 , NA))))


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

#Q Q54 Level of corruption --------------------------------------------------------------------------

# table(wave6$Q45)
# table(wave6$increased.corruption)
# wave6$increased.corruption <- ifelse(wave6$Q54 == "Decreased a lot", -2,
#                                      ifelse(wave6$Q54 == "Decreased somewhat", -1,
#                                             ifelse(wave6$Q54 == "Stayed the same", 0,
#                                                    ifelse(wave6$Q54 == "Increased somewhat", 1,
#                                                           ifelse(wave6$Q54 == "Increased a lot", 2, NA)))))


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~#
#index for goods and services ---------------------------------------------------------------------------------
#services:
#EA-SVC-A. Electricity
wave6$electric <- ifelse(wave6$EA_SVC_A== "Yes", .1,
                         ifelse(wave6$EA_SVC_A=="No",0,NA))

#EA-SVC-B. Piped water
wave6$water <- ifelse(wave6$EA_SVC_B== "Yes", .1,
                      ifelse(wave6$EA_SVC_B=="No",0,NA))
#EA-SVC-C. Sewage system in the PSU/EA
wave6$sewage <- ifelse(wave6$EA_SVC_C== "Yes", .1,
                       ifelse(wave6$EA_SVC_C=="No",0,NA))
#EA-SVC-D. Cell phone service in the PSU/EA
wave6$cellphone <- ifelse(wave6$EA_SVC_D== "Yes", .1,
                          ifelse(wave6$EA_SVC_D=="No",0,NA))

#EA-FAC-A. Post office in the PSU/EA
wave6$postoffice <- ifelse(wave6$EA_FAC_A=="Yes",.1,
                           ifelse(wave6$EA_FAC_A=="No",0,NA))

#EA-FAC-B. School in the PSU/EA
wave6$school <- ifelse(wave6$EA_FAC_B=="Yes",.1,
                       ifelse(wave6$EA_FAC_B=="No",0,NA))
#EA-FAC-C. Police station in the PSU/EA
wave6$police <- ifelse(wave6$EA_FAC_C=="Yes",.1,
                       ifelse(wave6$EA_FAC_C=="No",0,NA))
#EA-FAC-D. Health Clinic in the PSU/EA
wave6$clinic <- ifelse(wave6$EA_FAC_D=="Yes",.1,
                       ifelse(wave6$EA_FAC_D=="No",0,NA))
#EA-FAC-G. Market stalls in the PSU/EA
wave6$market <- ifelse(wave6$EA_FAC_E=="Yes",.1,
                          ifelse(wave6$EA_FAC_G=="No",0,NA))


# EA-ROAD-C-A. Tarred road
table(wave6$EA_ROAD_A)
wave6$goodroad <- ifelse(wave6$EA_ROAD_A=="Yes",.1,0)
table(wave6$goodroad)

#services index
wave6$serv.index <- wave6$goodroad + wave6$market + wave6$clinic + wave6$police +wave6$school +
  wave6$postoffice + wave6$cellphone + wave6$sewage +wave6$water + wave6$electric
table(wave6$serv.index)
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

wave6$COUNTRY <- as.character(wave6$COUNTRY) 


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

##subsetting and choosing relevant variables


wave6_cleaned <- subset(wave6, select = c(wave, tax.compliance, tax.compliance.1, compliance.binary, trust.president,
                                          trust.parl, education, female,urban, LPI, age, demo.satisfaction, compulsory.tax, tax.evasion, 
                                          employed, corrupt.officials, serv.index, COUNTRY))
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## saving the cleaned data

write.csv(wave6_cleaned, "wave6_cleaned.csv")


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~




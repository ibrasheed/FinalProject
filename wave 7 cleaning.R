#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## downloading and cleaning afrobarometer survey wave 7

setwd("/Users/rasheed/Documents/GitHub/FinalProject")

wave7<- read.spss("https://www.afrobarometer.org/wp-content/uploads/2022/02/r7_merged_data_34ctry.release.sav",
                  to.data.frame = T)


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~


##WAVE7 recoding ---------------------------------------------------------------------------------
wave7$wave <- paste0("wave",7)


# --------------------------------------------------------------------------------

# Refusal to pay tax

# Use an if-else loop to recode the categorical variable to numeric
wave7$tax.compliance <- ifelse(wave7$Q26D == "Yes, often", 0,
                          ifelse(wave7$Q26D == "Yes, several times", 1,
                           ifelse(wave7$Q26D == "Yes, once or twice", 2,
                              ifelse(wave7$Q26D == "No, but would do if had the chance", 3,
                                 ifelse(wave7$Q26D == "No, would never do this", 4, NA)))))


wave7$tax.compliance.1 <- ifelse(wave7$Q26D == "Yes, often", 0,
                                 ifelse(wave7$Q26D == "Yes, several times", 0.25,
                                        ifelse(wave7$Q26D == "Yes, once or twice", 0.5,
                                               ifelse(wave7$Q26D == "No, but would do if had the chance", 0.75,
                                                      ifelse(wave7$Q26D == "No, would never do this", 1, NA)))))

wave7$compliance.binary <- ifelse(wave7$Q26D == "Yes, often", 0,
                           ifelse(wave7$Q26D == "Yes, several times", 0,
                              ifelse(wave7$Q26D == "Yes, once or twice", 0,
                                  ifelse(wave7$Q26D == "No, but would do if had the chance", 0,
                                       ifelse(wave7$Q26D == "No, would never do this", 1, NA)))))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# Trust the president ---------------------------------------------------------------------------------


table(wave7$Q43A)
wave7$trust.president <- ifelse(wave7$Q43A == "Not at all", 0,
                                ifelse(wave7$Q43A == "Just a little", 1,
                                       ifelse(wave7$Q43A == "Somewhat", 2,
                                              ifelse(wave7$Q43A == "A lot", 3, NA))))
mean(wave7$trust.president, na.rm=T)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~


# Trust the parl. ---------------------------------------------------------------------------------
wave7$trust.parl <- ifelse(wave7$Q43B == "Not at all", 0,
                           ifelse(wave7$Q43B == "Just a little", 1,
                                  ifelse(wave7$Q43B == "Somewhat", 2,
                                         ifelse(wave7$Q43B == "A lot", 3, NA))))
mean(wave7$trust.parl, na.rm=T)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# Education ---------------------------------------------------------------------------------

wave7$education <- ifelse(wave7$Q97 == "No formal schooling", 0,
                     ifelse(wave7$Q97 == "Informal schooling only", 1,
                        ifelse(wave7$Q97 == "Some primary schooling", 2,
                             ifelse(wave7$Q97 == "Primary school completed", 3,
                                ifelse(wave7$Q97 == "Some secondary school / high school", 4,
                                   ifelse(wave7$Q97 == "Secondary school / high school completed", 5,
                                     ifelse(wave7$Q97 == "Post-secondary qualifications, other than university", 6,
                                         ifelse(wave7$Q97 == "Some university", 7,
                                             ifelse(wave7$Q97 == "University completed", 8,
                                                 ifelse(wave7$Q97 == "Post-graduate", 9, NA))))))))))
table(wave7$education)
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
# gender ---------------------------------------------------------------------------------
wave7$female <- ifelse(wave7$Q101== "Male", 0,
                       ifelse(wave7$Q101 == "Female", 1, NA))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#urban ---------------------------------------------------------------------------------

wave7$urban <- ifelse(wave7$URBRUR == "Rural", 0, 1)
table(wave7$urban)
table(wave7$URBRUR)
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~


# LPI ---------------------------------------------------------------------------------

#no food Q8A
wave7$no.food <- ifelse(wave7$Q8A == "Never", 0,
                        ifelse(wave7$Q8A == "Just once or twice", .25,
                               ifelse(wave7$Q8A == "Several times", .5,
                                      ifelse(wave7$Q8A == "Many times", .75,
                                             ifelse(wave7$Q8A == "Always", 1, NA)))))
# no water Q8 B
wave7$no.water <- ifelse(wave7$Q8B == "Never", 0,
                         ifelse(wave7$Q8B == "Just once or twice", .25,
                                ifelse(wave7$Q8B == "Several times", .5,
                                       ifelse(wave7$Q8B == "Many times", .75,
                                              ifelse(wave7$Q8B == "Always", 1, NA)))))
#no meds Q8c
wave7$no.meds <- ifelse(wave7$Q8C == "Never", 0,
                        ifelse(wave7$Q8C == "Just once or twice", .25,
                               ifelse(wave7$Q8C == "Several times", .5,
                                      ifelse(wave7$Q8C == "Many times", .75,
                                             ifelse(wave7$Q8C == "Always", 1, NA)))))
table(wave7$no.meds)
# no fuel Q8d
wave7$no.fuel <- ifelse(wave7$Q8D == "Never", 0,
                        ifelse(wave7$Q8D == "Just once or twice", .25,
                               ifelse(wave7$Q8D == "Several times", .5,
                                      ifelse(wave7$Q8D == "Many times", .75,
                                             ifelse(wave7$Q8D == "Always", 1, NA)))))

# no cash Q8e
wave7$no.cash <- ifelse(wave7$Q8E == "Never", 0,
                        ifelse(wave7$Q8E == "Just once or twice", .25,
                               ifelse(wave7$Q8E == "Several times", .5,
                                      ifelse(wave7$Q8E == "Many times", .75,
                                             ifelse(wave7$Q8E == "Always", 1, NA)))))


wave7$LPI <- wave7$no.cash + wave7$no.food + wave7$no.water + wave7$no.meds + wave7$no.fuel

mean(wave7$LPI, na.rm=T )

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## age ---------------------------------------------------------------------------------
wave7$age<- wave7$Q1
wave7$age <- gsub("[^[:digit:]]", NA, wave7$age)
wave7$age<- as.numeric(wave7$age)
table(wave7$age)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## Satisfaction with Democracy Q36---------------------------------------------------------------------------------

wave7$demo.satisfaction <- ifelse(wave7$Q36 == "The country is not a democracy", 0,
                                ifelse(wave7$Q36 == "Not at all satisfied", .25,
                                       ifelse(wave7$Q36 == "Not very satisfied", .5,
                                              ifelse(wave7$Q36 == "Fairly satisfied", .75,
                                                     ifelse(wave7$Q36 == "Very satisfied", 1, NA)))))

table(wave7$Demo.Perception)
table(wave7$Q36)
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## People must pay taxes Q38C-----------------------------------------------------------------------

wave7$compulsory.tax <- ifelse(wave7$Q38C == "Strongly Disagree", 0,
                               ifelse(wave7$Q38C == "Disagree", .25,
                                      ifelse(wave7$Q38C == "Neither Agree Nor Disagree", .5,
                                             ifelse(wave7$Q38C == "Agree", .75,
                                                    ifelse(wave7$Q38C == "Strongly Agree", 1, NA)))))


##Perceived non-compliance of others ----------------------------------------------------------------------

#Q48a. Ordinary person: pay bribe to avoid taxes
table(wave7$Q48A)
table(wave7$easy.taxevasion)

wave7$tax.evasion <- ifelse(wave7$Q48A == "Not at all likely", 0,
                                ifelse(wave7$Q48A == "Not very likely", .33,
                                       ifelse(wave7$Q48A == "Somewhat likely", .67,
                                              ifelse(wave7$Q48A == "Very likely", 1 , NA))))


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

#Q94. Employment status

table(wave7$Q94)
table(wave7$employed)
wave7$employed <- ifelse(wave7$Q94 == "No, not looking"|
                           wave7$Q94 == "No, looking", 0,
                         ifelse(wave7$Q94 == "Yes, part time"|
                                  wave7$Q94 == "Yes, full time", 1 , NA))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
#Q44c. Corruption: government officials--------------------------------------------------------------------------
table(wave7$Q44C)
table(wave7$corrupt.officials)

wave7$corrupt.officials <- ifelse(wave7$Q44C == "None", 0,
                                  ifelse(wave7$Q44C == "Some of them", .33,
                                         ifelse(wave7$Q44C == "Most of them", .67,
                                                ifelse(wave7$Q44C == "All of them", 1 , NA))))


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~#
#index for goods and services ---------------------------------------------------------------------------------
#services:
#EA-SVC-A. Electricity
wave7$electric <- ifelse(wave7$EA_SVC_A== "Yes", .1,
                         ifelse(wave7$EA_SVC_A=="No",0,NA))

#EA-SVC-B. Piped water
wave7$water <- ifelse(wave7$EA_SVC_B== "Yes", .1,
                      ifelse(wave7$EA_SVC_B=="No",0,NA))
#EA-SVC-C. Sewage system in the PSU/EA
wave7$sewage <- ifelse(wave7$EA_SVC_C== "Yes", .1,
                       ifelse(wave7$EA_SVC_C=="No",0,NA))
#EA-SVC-D. Cell phone service in the PSU/EA
wave7$cellphone <- ifelse(wave7$EA_SVC_D== "Yes", .1,
                          ifelse(wave7$EA_SVC_D=="No",0,NA))

#EA-FAC-A. Post office in the PSU/EA
wave7$postoffice <- ifelse(wave7$EA_FAC_A=="Yes",.1,
                           ifelse(wave7$EA_FAC_A=="No",0,NA))

#EA-FAC-B. School in the PSU/EA
wave7$school <- ifelse(wave7$EA_FAC_B=="Yes",.1,
                       ifelse(wave7$EA_FAC_B=="No",0,NA))
#EA-FAC-C. Police station in the PSU/EA
wave7$police <- ifelse(wave7$EA_FAC_C=="Yes",.1,
                       ifelse(wave7$EA_FAC_C=="No",0,NA))
#EA-FAC-D. Health Clinic in the PSU/EA
wave7$clinic <- ifelse(wave7$EA_FAC_D=="Yes",.1,
                       ifelse(wave7$EA_FAC_D=="No",0,NA))
#EA-FAC-G. Paid transport in the PSU/EA
wave7$market <- ifelse(wave7$EA_FAC_E=="Yes",.1,
                          ifelse(wave7$EA_FAC_G=="No",0,NA))


# EA-ROAD-C. Road condition last 5 km
table(wave7$EA_ROAD_C)
wave7$goodroad <- ifelse(wave7$EA_ROAD_C=="Fair"|wave7$EA_ROAD_C=="Good"|wave7$EA_ROAD_C=="Very good",.1,
                         ifelse(wave7$EA_ROAD_C=="Impassable"|wave7$EA_ROAD_C=="Very poor"|wave7$EA_ROAD_C=="Poor",0,NA))

#services index
wave7$serv.index <- wave7$goodroad + wave7$market + wave7$clinic + wave7$police +wave7$school +
  wave7$postoffice + wave7$cellphone + wave7$sewage +wave7$water + wave7$electric
table(wave7$serv.index)
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

##COUNTRY

wave7$COUNTRY <- as.character(wave7$COUNTRY)                                  


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
##subsetting and choosing relevant variables

wave7_cleaned <- subset(wave7, select = c(wave, tax.compliance, tax.compliance.1, compliance.binary, trust.president,
                                          trust.parl, education, female,urban, LPI, age, demo.satisfaction, compulsory.tax, tax.evasion, 
                                          employed, corrupt.officials, serv.index, COUNTRY))

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
## saving the cleaned data
write.csv(wave7_cleaned, "wave7_cleaned.csv")
#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~




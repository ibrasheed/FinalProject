# FinalProject
data cleaning project for programming course

This is a data cleaning and merging process of three waves (waves 5, 6, and 7) of afrobarometer survey data for subsequent data analysis and then merged with liberal democracy indicator from vdem and corruption control data from the world bank


## Cleaning scripts

- "wave 5 cleaning.R" is the R script for scraping and cleaning wave 5 of the afrobarometer survey

- "wave 6 cleaning.R" is the R script for scraping and cleaning wave 6 of the afrobarometer survey

- "wave 7 cleaning.R" is the R script for scraping and cleaning wave 5 of the afrobarometer survey

## cleaned data
- "wave5_cleaned.csv" is the output file after cleaning wave 5, i.e. after running "wave 5 cleaning.R"

- "wave6_cleaned.csv" is the output file after cleaning wave 6, i.e. after running "wave 6 cleaning.R"

- "wave7_cleaned.csv" is the output file after cleaning wave 7, i.e. after running "wave 7 cleaning.R"

## Merging data
- "merging data.R" is the R script that binds all three afrobarometer data from waves 5, 6, 7, and merges them with liberal democracy and corruption control data. 

- "vdem.csv" is a predownloaded data from varieties of democracy website. 

- "forAnalysis.csv" is the final dataset after merging everything, i.e. after running "merging data.R"

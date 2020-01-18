## Creating CLEAN Year long csv file to analyze

## Using stats probided by Metropolitan Police Department - City of St. Louis, Missouri
## Accessed 1/16/20 at ~17:16

## Load in tidyverse library
library(tidyverse)
library(lubridate)

## After storing all files in a single folder in the project, load all their names into a variable
# setwd("months/")
filenames <- list.files(all.files = TRUE)
filenames <- filenames[-(1:2)]

## Next combine all csv files
## make sure to set header as false and skip the first row to get rid of the header
All <- lapply(filenames, function(i){
read.csv(i, header = FALSE, skip = 1)
})

## Transform "All" variable into a single dataframe
df <- do.call(rbind.data.frame, All)

## Add in column headers to dataframe
names(df) <- c("Complaint", "Coded_Month", "Date_Occurance", "Flag_Crime", "Flag_Unfounded",
               "Flag_Administrative", "Count", "Flag_Cleanup", "Crime", "District", "Description",
               "ILEADsAddress", "ILEADSStreet", "Neighborhood", "Location_Name", 
               "Location_Comment", "CADAddress", "CADStreet", "XCoord", "YCoord")

## Convert data frame to a tibble
df <- as.tibble(df)

## Use Lubridate to cleanup "Date_Occurance" column
df <- mutate(df, Date_Occurance = mdy_hm(Date_Occurance))

## Arrange Occurances by Date and then remove any that do not happen within 2019
df <- arrange(df, Date_Occurance)

## Remove any dates that are not in the year 2019
rows_remove <- nrow(filter(df, year(Date_Occurance) != 2019))
df <- df[-(1:rows_remove),]

## Add Column that tells day of the week for crime
df <- mutate(df, Day = wday(day(Date_Occurance), label = TRUE ))

## Generalize Crime in new column based off the first two digits of six digit code
## Reference codebook in github account for codes

## Creating CLEAN Year long csv file to analyze

## Using stats probided by Metropolitan Police Department - City of St. Louis, Missouri
## Accessed 1/16/20 at ~17:16

## Load in tidyverse library
library(tidyverse)
library(lubridate)
library(rvest)

## After storing all files in a single folder in the project, load all their names into a variable
#setwd("months/")
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

## Clean up Crime Codes for easier analysis
df %>% mutate(Crime = sprintf("%06d", Crime))

## Add Column that tells day of the week for crime repeat to add month of the year
df <- mutate(df, Time = ((hour(Date_Occurance)*60) + (minute(Date_Occurance))))
df <- mutate(df, Day = wday(day(Date_Occurance) , label = TRUE ))
df <- mutate(df, Month = month(Date_Occurance, label = TRUE))
## Specify order for Months so that the factors are not arranged alphabetically
## Leaving this block of code out for now because it does not carry between scripts
#df$Month <- factor(df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
#                                        "Sep", "Oct", "Nov", "Dec"))

## URL pulled on 1/22/20 at 22:24 Standard

## Load in libraries for tidying data with tibbles and webscraping to pull table data
library(dplyr)
library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_neighborhoods_of_St._Louis"
## Scrape names of 79 different sections of city and then append 9 extra parks counted in report
neighborhoods <- read_html(url) %>% html_nodes("tbody:nth-child(1) td a") %>% html_text()
neighborhoods <- neighborhoods[1:79]
extra_neighborhoods <- c("Carondelet Park", "Tower Grove Park", "Forest Park", "Fairgrounds Park"
                         , "Penrose Park", "O'Fallon Park", "Cal-Bel Cemetery", "Botanical Garden"
                         , "Wilmore Park")
neighborhoods <- append(neighborhoods, extra_neighborhoods)

## Use For loop to create vector of all neighborhood number identifiers
da <- c()
for(i in 1:20){
  for(j in 0:3) {
    if(i + j*20 <= 79){
      da <- append(da, i + j*20)
    }
  }
}

## Add in numbers for Parks which are designated numeric identifiers
da1 <- c(80, 81, 82, 83, 84, 85, 86, 87, 88)
da <- append(da, da1)

## Change data type of da to numeric so we can reorder later
da <- as.numeric(da)


## Create tibble with two columns for da and neighborhoods
neigh <- as.tibble(cbind(da, neighborhoods))

## Generalize Crime in new column based off the first two digits of six digit code
## Reference codebook in github account for codes
#write.csv(df, file = "sources.csv")
write.csv(df, "C:/Users/Daniel/Desktop/STL-Crime-Stats-2019/source.csv")
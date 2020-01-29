## Creating CLEAN Year long csv file to analyze

## Using stats probided by Metropolitan Police Department - City of St. Louis, Missouri
## Accessed 1/16/20 at ~17:16

## Load in tidyverse library
library(tidyverse)
library(lubridate)
library(rvest)
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
               "ILEADsAddress", "ILEADSStreet", "NeighNumber", "Location_Name", 
               "Location_Comment", "CADAddress", "CADStreet", "XCoord", "YCoord")

## Convert data frame to a tibble
df <- as_tibble(df)

## Use Lubridate to cleanup "Date_Occurance" column
df <- mutate(df, Date_Occurance = mdy_hm(Date_Occurance))

## Arrange Occurances by Date and then remove any that do not happen within 2019
df <- arrange(df, Date_Occurance)

## Remove any dates that are not in the year 2019
rows_remove <- nrow(filter(df, year(Date_Occurance) != 2019))
df <- df[-(1:rows_remove),]

## Clean up Crime Codes for easier analysis by adding leading "0" to strings with less 
## than 5 digits
df <-  mutate(df, Crime = sprintf("%06d", Crime))

## Add Column that tells day of the week for crime repeat to add month of the year
df <- mutate(df, Time = ((hour(Date_Occurance)*60) + (minute(Date_Occurance))))
df <- mutate(df, Day = wday(Date_Occurance, label = TRUE))
df <- mutate(df, Month = month(Date_Occurance, label = TRUE))
## Specify order for Months so that the factors are not arranged alphabetically
## Leaving this block of code out for now because it does not carry between scripts
#df$Month <- factor(df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
#                                        "Sep", "Oct", "Nov", "Dec"))

## URL pulled on 1/22/20 at 22:24 Standard

url <- "https://en.wikipedia.org/wiki/List_of_neighborhoods_of_St._Louis"

## Scrape names of 79 different sections of city and then append 9 extra parks counted in report
NeighName <- read_html(url) %>% html_nodes("tbody:nth-child(1) td a") %>% html_text()
NeighName <- NeighName[1:79]
extra_neighborhoods <- c("Carondelet Park", "Tower Grove Park", "Forest Park", "Fairgrounds Park"
                         , "Penrose Park", "O'Fallon Park", "Cal-Bel Cemetery", "Botanical Garden"
                         , "Wilmore Park")
NeighName <- append(NeighName, extra_neighborhoods)

## Use For loop to create vector of all neighborhood number identifiers
NeighNumber <- c()
for(i in 1:20){
  for(j in 0:3) {
    if(i + j*20 <= 79){
      NeighNumber <- append(NeighNumber, i + j*20)
    }
  }
}

## Add in numbers for Parks which are designated numeric identifiers
extraNeigh <- c(80, 81, 82, 83, 84, 85, 86, 87, 88)
NeighNumber <- append(NeighNumber, extraNeigh)

## Change data type of da to numeric so we can reorder later
NeighNumber <- as.numeric(NeighNumber)

## Create tibble with two columns for da and neighborhoods
neigh <- as.data.frame(cbind(NeighNumber, NeighName))

## Merge df and neigh by their neighborhood id
## First add another column to df to store NeighNames
df <- mutate(df, NeighName = 0)
#df$NeighName <- match(df$NeighNumber, neigh$NeighNumber)
df$NeighName <- neigh[match(df$NeighNumber, neigh$NeighNumber), 2]

## Generalize Crime in new column based off the first two digits of six digit code
## Reference codebook in github account for codes
## Create Column based off 2 digit quick codes based off of 6 digit "Crime" column
df <- mutate(df, Crime_General = substr(df$Crime, start = 1, stop = 2))

## Create string for all offenses
crime_type <- c("Criminal Homicide", "Forceible Rape", "Robbery", "Aggravated Assault", 
                "Burglary", "Larceny", "Motor Vehicle Theft", "Arson", "Other Assualts",
                "Forgery and Counterfeiting", "Fraud", "Embezzlement", "Stolen Property", 
                "Vandalism", "Weapons Possession", "Prosititution", "Sex Offenses", 
                "Drug Abuse Violations", "Gambling", "Offenses Against the Family and Children", 
                "Driving Under the Influence", "Liquor Laws", "Drunkenness",
                "Disorderly Conduct", "Vagrancy", "All Other Offenses")
crime_number <- sprintf("%02d", 1:26)
crime <- as.data.frame(cbind(crime_number, crime_type))

## Match up crime with 2 digit code
df$Crime_Type <- crime[match(df$Crime_General, crime$crime_number), 2]

#write.csv(df, file = "sources.csv")
write.csv(df, "C:/Users/Daniel/Desktop/STL-Crime-Stats-2019/source.csv")
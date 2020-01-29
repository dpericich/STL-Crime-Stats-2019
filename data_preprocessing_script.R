## Data Preprocessing Script for STL City Crime Reports
## CSV files provided by the Metropolitan Police Department - City of St. Louis, Missouri
## CSV files downloaded 1/16/20 at 17:16

## Load in all necessary pacakges
library(dplyr)
library(lubridate)
library(rvest)

## After downloading all csv files, call all files from subdirectory "months" to a single variable
file_names <- list.files(path = "months/", all.files = TRUE)
file_names <- file_names[-(1:2)]

## Use a loop apply function to read all csv files into a sinlge variable
all_files <- lapply(file_names, function(i){
  read.csv(paste0("months/", i), header = FALSE, skip = 1)
})

## Transform "All" variable into a single dataframe
df <- do.call(rbind.data.frame, all_files)

## Add in original csv file headers
names(df) <- c("Complaint", "Coded_Month", "Date_Occurrence", "Flag_Crime", "Flag_Unfounded",
               "Flag_Administrative", "Count", "Flag_Cleanup", "Crime", "District", "Description",
               "ILEADsAddress", "ILEADSStreet", "NeighNumber", "Location_Name", "Location_Comment",
               "CADAddress", "CADStreet", "XCoord", "YCoord")

## Convert data frame to a tibble
df <- as_tibble(df)

## Remove Columns that will not be used during data analysis
df <- df[, -(4:8)]


## 1 - Dates
## Use Lubridate package to clean up "Date_Occurance" column then arrange them by month
df <- mutate(df, Date_Occurrence = mdy_hm(Date_Occurrence)) 
df <- arrange(df, Date_Occurrence)

## Remove all records that do not occur in 2019
rows_to_remove <- nrow(filter(df, year(Date_Occurrence) != 2019))
df <- df[-(1:rows_to_remove),]

## Add in Columns for days of the week and months of the year
df <- mutate(df, Minute = ((hour(Date_Occurrence)*60) + (minute(Date_Occurrence))))
df <- mutate(df, Day = wday(Date_Occurrence, label = TRUE))
df <- mutate(df, Month = month(Date_Occurrence, label = TRUE))


## 2 - Crime Codes and Lables
## Standardize Crime Codes and then Abbreviate for General Crime Code Matching
df <- mutate(df, Crime = sprintf("%06d", Crime))
df <- mutate(df, Crime_General = substr(df$Crime, start = 1, stop =2))

## Create second data frame for Crime Code Description Matching
crime_type <- c("Criminal Homicide", "Forceible Rape", "Robbery", "Aggravated Assault", 
                "Burglary", "Larceny", "Motor Vehicle Theft", "Arson", "Other Assualts",
                "Forgery and Counterfeiting", "Fraud", "Embezzlement", "Stolen Property", 
                "Vandalism", "Weapons Possession", "Prosititution", "Sex Offenses", 
                "Drug Abuse Violations", "Gambling", "Offenses Against the Family and Children", 
                "Driving Under the Influence", "Liquor Laws", "Drunkenness",
                "Disorderly Conduct", "Vagrancy", "All Other Offenses")
crime_number <- sprintf("%02d", 1:26)
crime <- as.data.frame(cbind(crime_number, crime_type))

df$Crime_Type <- crime[match(df$Crime_General, crime$crime_number), 2]

## 3 - Locations
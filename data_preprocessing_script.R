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
df <- mutate(df, Hour = (hour(Date_Occurrence)))
df <- mutate(df, Day = wday(Date_Occurrence, label = TRUE))
df <- mutate(df, Month = month(Date_Occurrence, label = TRUE))


## 2 - Crime Codes and Lables
## Standardize Crime Codes and then Abbreviate for General Crime Code Matching
df <- mutate(df, Crime = sprintf("%06d", Crime))
df <- mutate(df, Crime_General = substr(df$Crime, start = 1, stop =2))

## Create second data frame for Crime Code Description Matching
crime_type <- c("Criminal Homicide", "Forcible Rape", "Robbery", "Aggravated Assault", 
                "Burglary", "Larceny", "Motor Vehicle Theft", "Arson", "Other Assaults",
                "Forgery and Counterfeiting", "Fraud", "Embezzlement", "Stolen Property", 
                "Vandalism", "Weapons Possession", "Prosititution", "Sex Offenses", 
                "Drug Abuse Violations", "Gambling", "Offenses Against the Family and Children", 
                "Driving Under the Influence", "Liquor Laws", "Drunkenness",
                "Disorderly Conduct", "Vagrancy", "All Other Offenses")
crime_number <- sprintf("%02d", 1:26)
crime <- as.data.frame(cbind(crime_number, crime_type))

df$Crime_Type <- crime[match(df$Crime_General, crime$crime_number), 2]

## 3 - Locations
## Use rvest to scrape Neighborhood names and their numbers from wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_neighborhoods_of_St._Louis"
wiki <- read_html(url)

NeighName <- wiki %>% html_nodes("tbody:nth-child(1) td a") %>% html_text()

## Select only the first 79 text elements for the neighborhoods 
NeighName <- NeighName[1:79]

## Naming convetion between Wikipedia list and table are inconsistent which leads to NA values 
## when we use a join based off neighborhood names. Manually correcting inconsistencies
NeighName[7] <- "Clayton-Tamm"
NeighName[23] <- "Skinker-DeBaliviere"
NeighName[25] <- "South Hampton"
NeighName[32] <- "O’Fallon"
NeighName[42] <- "The Gate District"
NeighName[50] <- "Peabody Darst Webbe"

## Combine 79 neighborhood names with 9 public areas defined by SLMPD
extra_neighborhoods <- c("Carondelet Park", "Tower Grove Park", "Forest Park", "Fairgrounds Park"
                         , "Penrose Park", "O'Fallon Park", "Cal-Bel Cemetery", "Botanical Garden"
                         , "Wilmore Park")
NeighName <- append(NeighName, extra_neighborhoods)

## Use for loop to create vector of nummbers that correspond to scraped names above
NeighNumber <- c()
for(i in 1:20){
  for(j in 0:3) {
    if(i + j*20 <= 79){
      NeighNumber <- append(NeighNumber, i + j*20)
    }
  }
}

## Add in the 9 extra neighborhood numbers and add them to NeighNumber
extraNeigh <- (80:88)
NeighNumber <- append(NeighNumber, extraNeigh)

## Create data frame with two columns of neighborhood values
neigh <- as.data.frame(cbind(NeighNumber, NeighName))

## Merge df and neigh by their neighborhood id
df$NeighName <- neigh[match(df$NeighNumber, neigh$NeighNumber), 2]


## 4 - Population
## Load in Neighborhoods and their populations as a table
population <- wiki %>% html_nodes("td:nth-child(2)") %>% html_text()
population <- population[-(1:20)]

NeighName <- read_html(url) %>% html_nodes("td:nth-child(1)") %>% html_text()
NeighName <- NeighName[-c(1:20, 100:103)]

popul <- as.data.frame(cbind(population, NeighName))

## Match the populations to the neighborhoods using the neighborhood name
df$Population <- popul[match(df$NeighName, popul$NeighName), 1]

## create a csv file with data to be loaded for analysis
write.csv(df, file = "source.csv")
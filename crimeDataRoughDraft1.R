## First take on crime data
## Load in necessary libraries
library(ggplot2)
library(dplyr)

## Set working directory
setwd("C:/Users/Daniel/Desktop/STL-Crime-Stats-2019")

## Load in data from source file
source <- read.csv(file = "source.csv")
df <- as.tibble(source)

## Plot Bar Chart for Crime by Month
monthly <- group_by(df, Month)
per_month <- summarise(monthly, Crime = n())
per_month$Month <- factor(per_month$Month, 
                         levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                    "Oct", "Nov", "Dec"))
months_base <- ggplot(data = per_month, mapping = aes(x = Month, y = Crime)) +
  geom_bar(stat = "identity")
## Come back once crime is broken down from 290 factors to 26 and apply fill = type to geom_bar

## Plot Bar Chart for Crime by Day of the Week
daily <- group_by(df, Day)
per_day <- summarise(daily, Crime = n())
per_day$Day <- factor(per_day$Day, 
                      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

wdays_base <- ggplot(data = per_day, aes(x = Day, y = Crime)) +
  geom_bar(stat = "identity")
## First take on crime data
## Load in necessary libraries
library(ggplot2)
library(dplyr)

## Load in data from source file
source <- read.csv(file = "source.csv")
df <- as_tibble(source)

## Plot Bar Chart for Crime by Month
monthly <- group_by(df, Month)
per_month <- summarise(monthly, Crime = n())
per_month$Month <- factor(per_month$Month, 
                         levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                    "Oct", "Nov", "Dec"))
months_base <- ggplot(data = per_month, mapping = aes(x = Month, y = Crime)) +
  geom_bar(stat = "identity")
## Come back once crime is broken down from 290 factors to 26 and apply fill = type to geom_bar

## Show the counts for crimes reported by month
count(monthly)

## Plot Bar Chart for Crime by Day of the Week
daily <- group_by(df, Day)
per_day <- summarise(daily, Crime = n())
per_day$Day <- factor(per_day$Day, 
                      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

wdays_base <- ggplot(data = per_day, aes(x = Day, y = Crime)) +
  geom_bar(stat = "identity")

## Show the counts for crimes reported by day of the week
count(daily)

## Plot Bar Chart for Crime by Hour of the Day
hourly <- group_by(df, Hour)
per_hour <- summarise(hourly, Crime = n())

hours_base <- ggplot(data = per_hour, aes(x = Hour, y = Crime)) +
  geom_bar(stat = "identity", color = "blue") + 
  geom_vline(xintercept = 12,color = "red") + 
  labs(title = "Crimes Committed by Hour in STL in 2019", ylab = "Number of Crimes")

## Show the counts for Crimes reported by hour of the day
count(hourly)


## Plot Bar Chart for Crimes reported by Neighborhood and Area
neighborhood <- group_by(df, NeighName)
per_neigh <- summarise(neighborhood, Crime = n())
neigh_base <- ggplot(data = per_neigh, mapping = aes(x = reorder(NeighName, -Crime), y = Crime)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Show the counts for Crimes reported by neighborhood and area
count(neighborhood)

## Arrange the Neighborhood/Areas by crime and then create top 10 for more readable results
neigh_ranked <- arrange(per_neigh, desc(Crime))
top_ten <- neigh_ranked[1:10,]
top_ten_base <- ggplot(data = top_ten, mapping = aes(x = reorder(NeighName, -Crime), y = Crime)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Plot only neighborhoods that make up at least 2% of crime in the city
ordered_per_neigh <- arrange(per_neigh, desc(Crime))
top_two_percent_neigh <- ordered_per_neigh[1:11,]
top_two_percent_neigh_base <- ggplot(data = top_two_percent_neigh, mapping = aes(x = reorder(
  NeighName, -Crime), y = Crime)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red")

## Create a Time Series Graph for Crime Throughout the day
ts_hour <- df %>% count(Hour, Crime_Type)
ggplot(data = ts_hour, mapping = aes(x = Hour, y = n, color = Crime_Type)) +
  geom_line()

## Create a Time Series Graph for Crime Throughout the week
ts_day <- df %>% count(Day, Crime_Type)
ggplot(data = ts_day, mapping = aes(x = Day, y = n, color = Crime_Type)) +
  geom_line()

## Create a group that shows overall time counts for the year
by_crime <- count(df, Crime_Type) %>% arrange(desc(n))

## Create vector of crimes to not look at in study
non_preventable_crimes <- c("All Other Offense", "Driving Under the Influence", "Drug Abuse Violations",
                            "Embezzlement", "Forgery and Counterfeiting", "Fraud", "Liquor Laws",
                            "Offenses Against the Family and Children", "Prosititution",
                            "Sex Offenses", "Stolen Property", "Vagrancy", "Weapons Possession")
preventable_crimes <- c("Aggravated Assault", "Arson", "Burglary", "Criminal Homicide", 
                        "Disorderly Conduct", "Forcible Rape", "Larceny", "Motor Vehicle Theft",
                        "Other Assualts", "Robbery", "Vandalism")

## Graph Population by Neighborhood
neighborhood_population <- df %>% group_by(NeighName, Population) %>% 
  summarise(Crime = n())
population_vs_crime <- ggplot(data = neighborhood_population, mapping = aes(x = NeighName)) +
##  geom_line(aes(y = Population, color = "blue")) +
  geom_line(aes(y = Crime, color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
                          
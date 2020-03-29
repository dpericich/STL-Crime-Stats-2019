## First take on crime data
## Load in necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

## Load in data from source file
source <- read.csv(file = "source.csv")
df <- as_tibble(source)

##Final Plot - Figure 1
## Plot Bar Chart for Crime by Month
monthly <- df %>% group_by(Month) %>%  summarise(Crime = n())
monthly$Month <- factor(monthly$Month, 
                         levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                    "Oct", "Nov", "Dec"))
months_base <- ggplot(data = monthly, mapping = aes(x = Month, y = Crime)) +
  geom_bar(stat = "identity", fill = "Gold") +
  labs(title = "St. Louis City Crime Counts by Month in 2019", x = "Months in 2019", y = "Number of Crimes",
       caption = "Fig 1 : Chart showing crime counts for every month of 2019 recorded in St. Louis City.") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0))
  
## Come back once crime is broken down from 290 factors to 26 and apply fill = type to geom_bar
## Create Table that shows Crimes by counts and then create table of top ten crimes
crime_count <- df %>% group_by(Crime_Type) %>% summarise(Crime = n()) %>% arrange(desc(Crime))
crime_count_top_10 <- crime_count[1:10,]
## Show the counts for crimes reported by month
count(monthly)

## Plot Bar Chart for Crime by Day of the Week
daily <- df %>% group_by( Day) %>% summarise(Crime = n())
daily$Day <- factor(daily$Day, 
                      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

wdays_base <- ggplot(data = daily, aes(x = Day, y = Crime)) +
  geom_bar(stat = "identity", fill = "black") + 
  labs(title = "St. Louis City Crime Counts by Day of the Week in 2019", x = "Day of the Week", 
       y = "Number of Crimes", caption = "Fig 2 : Total crime counts for days of the week in 2019") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0))

## Show the counts for crimes reported by day of the week
count(daily)

## Plot Bar Chart for Crime by Hour of the Day
hourly <- df %>% group_by(Hour) %>% summarise(Crime = n())

hours_base <- ggplot(data = hourly, aes(x = Hour, y = Crime)) +
  geom_bar(stat = "identity", fill = "orange") + 
  geom_vline(xintercept = 12,color = "black") + 
  labs(title = "St. Louis City Crime Counts by Hour of the Day in 2019", 
       x = "Hour of the Day", y = "Number of Crimes",
       caption = "Fig 3 : A graph of all crimes in STL City graphed by hour of the day occured over the course of 2019.") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0))

## Show the counts for Crimes reported by hour of the day
count(hourly)


## Plot Bar Chart for Crimes reported by Neighborhood and Area
neighborhood <- df %>% filter(is.na(NeighName) != TRUE) %>% group_by(NeighName) %>% summarise(Crime = n())
neigh_base <- ggplot(data = neighborhood, mapping = aes(x = reorder(NeighName, -Crime), y = Crime)) +
  geom_bar(stat = "identity", fill = "Gold") +
  labs(title = "St. Louis City Crime Counts by Neighborhood in 2019", x = "Neighborhood",
       y = "Number of Crimes", caption = "Fig A : Crime counts broken up by neighborhoods and public 
       areas in St. Louis during 2019") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

## Show the counts for Crimes reported by neighborhood and area
count(neighborhood)

## Arrange the Neighborhood/Areas by crime and then create top 10 for more readable results
neighborhood <- df %>% filter(is.na(NeighName) != TRUE) %>% group_by(NeighName) %>% summarise(Crime = n())
neigh_ranked <- neighborhood %>% arrange(desc(Crime))
top_ten <- neigh_ranked[1:10,]
top_ten_base <- ggplot(data = top_ten, mapping = aes(x = reorder(NeighName, -Crime), y = Crime)) +
  geom_bar(stat = "identity", fill = "Gold") +
  labs(title = "St.Louis City Top 10 Neighborhoods by Crime Count in 2019", x = "Neighborhood",
       y = "Number of Crimes", caption = "Fig 4 : The top 10 neighborhoods in St. Louis City from 2019 based off of number of crimes committed.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

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
  geom_line() + 
  labs(title = "St. Louis City Crime Counts by Hour for Individual Crime Types", x = "Hour of the Day",
       y = "Total Number of Crimes", caption = "Fig B : Time Series plot showing the 2019 counts of each individual crime over a 24 hour period.") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0))

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
                            "Sex Offenses", "Stolen Property", "Vagrancy", "Weapons Possession",
                            "Disorderly Conduct")
preventable_crimes <- c("Aggravated Assault", "Arson", "Burglary", "Criminal Homicide", 
                        "Forcible Rape", "Larceny", "Motor Vehicle Theft", "Other Assualts", 
                        "Robbery", "Vandalism")

## Take preventable crimes and further break them down into subcategories
theft <- c("Burglary", "Larceny", "Motor Vehicle Theft", "Robbery")
destruction <- c("Arson", "Vandalism")
violent_crimes <- c("Aggravated Assualt", "Criminal Homicide", "Forcible Rape", "Other Assualts")

## Graph Population by Neighborhood
neighborhood_population <- df %>% group_by(NeighName, Population) %>% 
  summarise(Crime = n())
population_vs_crime <- ggplot(data = neighborhood_population, mapping = aes(x = NeighName)) +
##  geom_line(aes(y = Population, color = "blue")) +
  geom_line(aes(y = Crime, color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
                          
#df_prevent <- df %>% filter(Crime_Type %in% preventable_crimes) %>% group_by(NeighName) %>%
#  summarise(Crime_Type = n())

df_prevent_time <- df %>% filter(Crime_Type %in% preventable_crimes) %>% group_by(Crime_Type, Hour) %>% 
  summarise(Crime = n())

df_prevent_crime_time <- ggplot(data = df_prevent_time, mapping = aes(x = Hour, y = Crime, color = Crime_Type)) +
  geom_line()

df_prevent_count <- df %>% filter(Crime_Type %in% preventable_crimes) %>% group_by(Crime_Type) %>% 
  summarise(Crime = n())

df_prevent_crime_count  <- ggplot(data = df_prevent_count, mapping = aes(x = reorder(Crime_Type, -Crime), y = Crime)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Blues")

grid.arrange(df_prevent_crime_time, df_prevent_crime_count, ncol = 2)

crimes_by_count <- df %>% group_by(Crime_Type) %>% summarise(Crime = n()) %>% arrange(desc(Crime))

## Make graphs top 10 graphs for 3 prevent categories and store them on a 3 grid 
## Theft
prevent_theft <- df %>% filter(Crime_Type %in% theft) %>% group_by(NeighName) %>% summarise(Crime = n()) %>%
  arrange(desc(Crime))
prevent_theft <- prevent_theft[1:10,]
prevent_theft_top10 <- ggplot(data = prevent_theft, mapping = aes(x = reorder(NeighName, -Crime), y = Crime)) +
  geom_bar(stat = "identity", fill = "Orange") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Neighborhoods for Theft in 2019")

## Destruction
prevent_destruction <- df %>% filter(Crime_Type %in% destruction) %>% group_by(NeighName) %>% summarise(Crime = n()) %>%
  arrange(desc(Crime))
prevent_destruction <- prevent_destruction[1:10,]
prevent_destruction_top10 <- ggplot(data = prevent_destruction, mapping = aes(x = reorder(NeighName, -Crime), y = Crime)) +
  geom_bar(stat = "identity", fill = "Gold") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Neighborhoods for Destruciton in 2019")

## Violent Crimes
prevent_violent_crimes <- df %>% filter(Crime_Type %in% violent_crimes) %>% group_by(NeighName) %>% summarise(Crime = n()) %>%
  arrange(desc(Crime))
prevent_violent_crimes <- prevent_violent_crimes[1:10,]
prevent_violent_crimes_top10 <- ggplot(data = prevent_violent_crimes, mapping = aes(x = reorder(NeighName, -Crime), y = Crime)) +
  geom_bar(stat = "identity", fill = "Black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Neighborhoods for Violent Crimes in 2019")

grid.arrange(prevent_theft_top10, prevent_destruction_top10, prevent_violent_crimes_top10, ncol = 3)

## Create Time Series for three subcategories of preventable crimes
## Theft Time Series
prevent_theft_hours <- df %>% filter(Crime_Type %in% theft) %>% group_by(Hour) %>% summarise(Crime = n())
prevent_theft_hours_time <- ggplot(data = prevent_theft_hours, mapping = aes(x = Hour, y = Crime)) +
  geom_line() +
  labs(title = "Theft Crime Occurances by Hour of the Day in 2019") +
  geom_vline(xintercept = 17, color = "red")

## Desctruction
prevent_destruction_hours <- df %>% filter(Crime_Type %in% destruction) %>% group_by(Hour) %>% summarise(Crime = n())
prevent_desctruction_hours_time <- ggplot(data = prevent_destruction_hours, mapping = aes(x = Hour, y = Crime)) +
  geom_line() + 
  labs(title = "Destructive Crimes Occurances by Hour of the Day in 2019") +
  geom_vline(xintercept = 22, color = "red")

## Violent Crimes
prevent_violent_crimes_hours <- df %>% filter(Crime_Type %in% violent_crimes) %>% group_by(Hour) %>% summarise(Crime = n())
prevent_violent_crimes_hours_time <- ggplot(data = prevent_violent_crimes_hours, mapping = aes(x = Hour, y = Crime)) +
  geom_line() +
  labs(title = "Violent Crime Occurances by Hour of the Day in 2019") +
  geom_vline(xintercept = 23, color = "red")

grid.arrange(prevent_theft_hours_time, prevent_desctruction_hours_time, prevent_violent_crimes_hours_time)


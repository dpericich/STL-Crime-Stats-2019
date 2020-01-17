## Creating Year long csv file to analyze

## Using stats probided by Metropolitan Police Department - City of St. Louis, Missouri
## Accessed 1/16/20 at ~17:16

## First Load in all csv files
## Clear out any noise/extra data from the beginning of the files

## January 2019 
jan19 <- read.csv("January2019.csv", header = FALSE, skip = 3)

## February 2019
feb19 <- read.csv("February2019.csv", header = FALSE, skip = 208)

## March 2019
mar19 <- read.csv("March2019.csv", header = FALSE, skip = 268)

## April 2019 
apr19 <- read.csv("April2019.csv", header = FALSE, skip = 307)

## May 2019
may19 <- read.csv("May2019.csv", header = FALSE, skip = 313)

## June 2019
jun19 <- read.csv("June2019.csv", header = FALSE, skip = 487)

## July 2019
jul19 <- read.csv("July2019.csv", header = FALSE, skip = 343)

## August 2019
aug19 <- read.csv("August2019.csv", header = FALSE, skip = 428)

## September 2019 
sep19 <- read.csv("September2019.csv", header = FALSE, skip = 465)

## October 2019
oct19 <- read.csv("October2019.csv", header = FALSE, skip = 446)

## November 2019
nov19 <- read.csv("November2019.csv", header = FALSE, skip = 394)

## December 2019
dec19 <- read.csv("December2019.csv", header = FALSE, skip = 461)

## Read in the January file to get a header object
header <- read.csv("January2019.csv", header = FALSE, nrows = 1)

## Store all months as an object
months <- list(jan19, feb19, mar19, apr19, may19, jun19, jul19, aug19, sep19, oct19, nov19, dec19)

## Combine months into a single data frame
crime2019 <- merge(months, check.names = FALSE)

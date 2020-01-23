## Rolled condensed version of this code into the main data analysis script

## Script to webscrape the neighborhoods and their corresponding numbers from Wikipedia
## URL pulled on 1/22/20 at 22:24 Standard

## Load in libraries for tidying data with tibbles and webscraping to pull table data
library(dplyr)
library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_neighborhoods_of_St._Louis"
neighborhoods <- read_html(url) %>% html_nodes("tbody:nth-child(1) td a") %>% html_text()
neighborhoods <- neighborhoods[1:79]

## Brute Force Method for getting numbers formatted like table
#neigh_nums <- c(1, 21, 41, 61, 2, 22, 42, 62, 3, 23, 43, 63, 4, 24, 44, 64, 5, 25, 45, 65, 6, 26, 46,
#               66, 7, 27, 47, 67, 8, 28, 48, 68, 9, 29, 49, 69, 10, 30, 50, 70, 11, 31, 51, 71, 12,
#               32, 52, 72, 13, 33, 53, 73, 14, 34, 54, 74, 15, 35, 55, 75, 16, 36, 56, 76, 17, 37, 
#               57, 77, 18, 38, 58, 78, 19, 39, 59, 79, 20, 40, 60)

## 2nd most efficient way to produce vector
#la <- c()
#for(i in 1:20) {
#  la <- append(la, i)
#  la <- append(la, i +20)
#  la <- append(la, i +40)
#  if(i + 60 <= 79) {
#    la <- append(la, i + 60)
#  }
#}

## Most efficient way to create vector
da <- c()
for(i in 1:20){
  for(j in 0:3) {
    if(i + j*20 <= 79){
    da <- append(da, i + j*20)
    }
  }
}

## Change data type of da to numeric so we can reorder later
da <- as.numeric(da)

## Create tibble with two columns for da and neighborhoods
neigh <- as.tibble(cbind(da, neighborhoods))
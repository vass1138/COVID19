## ---------------------------
##
## Script name: 
##
## Purpose of script:
##   Download and  clean the COVID19 by LGA Google sheet csv data file
##
## Author: Dr. Emanuel Vassiliadis
##
## Date Created: 2020-08-28
##
## LinkedIn: https://www.linkedin.com/in/evassiliadis/
##
## ---------------------------
##
## Notes:
##   Replaces JSON method (which was terminated unexpectedly 2020-08-20)
##
## ---------------------------

rm(list = ls(all = TRUE))

library(here)
getwd()

library(tidyverse)

# install_this_library("httr")

today <- Sys.Date()

# Google spreadsheet as csv
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ9oKYNQhJ6v85dQ9qsybfMfc-eaJ9oKVDZKx-VGUr6szNoTbvsLTzpEaJ3oW_LZTklZbz70hDBUt-d/pub?gid=0&single=true&output=csv"
data <- read.csv(url,stringsAsFactors = FALSE)

# select columns to be consistent with previous clean data
df <- data %>%
  select(LGADisplay,cases,active,data_date)

rename(df,c("LGA"="LGADisplay","TotalCount"="cases","ActiveCount"="active","SampleDate"="data_date"))

# cleanup LGA
df[,1] <- toupper(df[,1])

# cleanup NA
df[is.na(df)] <- 0

# additional columns - formats other than csv have the following values
# df$ActiveCount <- 0
# df$SampleDate <- today

# save downloaded data
output <- paste0(here("data","dhhs_csv"),"/",format(today,"%y%m%d"),".txt")
write.csv(data,output,row.names=FALSE)

# save clean subset
output <- paste0(here("data","dhhs_clean"),"/",format(today,"%y%m%d"),".txt")
write.csv(df,output,row.names=FALSE)






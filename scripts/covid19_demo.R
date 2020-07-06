## ---------------------------
##
## Script name: covid19_demo.R
##
## Purpose of script:
## Demo script to process world data
##
## Author: Dr. Emanuel Vassiliadis
##
## Date Created: 2020-07-06
##
## LinkedIn: https://www.linkedin.com/in/evassiliadis/
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

rm(list = ls(all = TRUE))

library(here)
getwd()

library(tidyverse)

install_this_library <- function(libname) {

  if (!(libname %in% installed.packages())) {
    install.packages(libname)
  } else {
    print(paste0(libname," already installed"))
  }
}

install_this_library("rvest")

rm(list=ls())

library(here)
getwd()

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# WIDE format

# df <- read.csv("time_series_covid19_confirmed_global.csv",stringsAsFactors = FALSE)
# 
# # drop columns
# # df$Province.State <- NULL
# df$Lat <- NULL
# df$Long <- NULL
# 
# ldf <- gather(df,ConfirmedDate,ConfirmedCases,-Country.Region,-Province.State)
# 
# ldfc <- aggregate(ldf$ConfirmedCases,
#                   by = list(ldf$Country.Region,ldf$ConfirmedDate),
#                   FUN = sum)
# 
# ldfc <- ldfc %>% rename(Country = Group.1, Date = Group.2, Count = x)
# 
# ldfc$Date <- as.Date(ldfc$Date,format="X%m.%d.%Y")

# NARROW format

this_path <- paste0(here("data","misc"),"/time_series_covid19_confirmed_global_narrow.csv")
dn <- read.csv(this_path,stringsAsFactors = FALSE)                         

# drop first data row
dn <- dn[-1,]

# remove columns
dn$Lat <- NULL
dn$Long <- NULL

# convert chr to date
dn$Date <- as.Date(dn$Date)
dn$Value <- as.integer(dn$Value)

# sum by country and date
dnc <- aggregate(dn$Value,
                 by = list(dn$Country.Region,dn$Date),
                 FUN = sum)

# reset column names after aggregation
dnc <- dnc %>% rename(Country = Group.1, Date = Group.2, Count = x)

ggplot(dnc,aes(x=Date,y=Count,color=Country)) +
         geom_line(aes(linetype=Country)) +
  theme(legend.position="NONE") +
labs(x="Date",
     y="Count",
     title="Confirmed cases by country"
) 



## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
##
## Author: Dr. Emanuel Vassiliadis
##
## Date Created: 2020-07-04
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
library(data.table)
library(httr)
library(jsonlite)
library(readr)

install_this_library <- function(libname) {

  if (!(libname %in% installed.packages())) {
    install.packages(libname)
  } else {
    print(paste0(libname," already installed"))
  }
}

# install_this_library("httr")

today <- Sys.Date()

# api endpoint used by DHHS PowerBI solution
url <- "https://wabi-australia-southeast-api.analysis.windows.net/public/reports/querydata?synchronous=true"

# JSON request body saved from fiddler
# File must not have trailing blank lines
file_json <- paste0(here("data","misc"),"/dhhs_powerbi.json")

# read JSON as raw text: DO NOT use fromJSON()
body_txt <- read_file(file_json)

response <- POST(url, body = body_txt, encode="json", add_headers(.headers = c("Content-Type"="application/json")), verbose())
text_json <- content(response, type = 'text', encoding = "UTF-8")
response_list <- fromJSON(content(response),flatten=TRUE)

# ADHOC hard-coded file input
# response_list <- fromJSON(paste0(here("data","json"),"/dhhs_response_200703.txt"),flatten=TRUE)

# list of lists: isolate sublist with data
p <- response_list$results$result.data.dsr.DS[[1]]$PH[[1]]$DM0[[1]]$C

# transpose lists and format as dataframe
df <- as.data.frame(data.table::transpose(p), col.names = c("LGA","TotalCount"),stringsAsFactors = FALSE)

# cleanup LGA
# format locality: remove spaces, remove LGA class, convert to uppercase
# df[,1] <- gsub("\\s+", " ", str_trim(df[,1]))
df[,1] <- gsub("\\(\\w+\\)","",df[,1])
df[,1] <- str_squish(df[,1])
df[,1] <- toupper(df[,1])

# cleanup NA
df[is.na(df)] <- 0

# additional columns
df$ActiveCount <- 0
df$SampleDate <- today

# ADHOC save output - see matching input above
# output <- paste0(here("data","dhhs_clean"),"/","200703.txt")

# save data file
output <- paste0(here("data","dhhs_clean"),"/",format(today,"%y%m%d"),".txt")
write.csv(df,output,row.names=FALSE)






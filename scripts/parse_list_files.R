## ---------------------------
##
## Script name: parse_list_files.R
##
## Purpose of script: Parse text strings into CSV
##
##
## Author: Dr. Emanuel Vassiliadis
##
## Date Created: 2020-07-02
##
## LinkedIn: https://www.linkedin.com/in/evassiliadis/
##
## ---------------------------
##
## Notes: Customised for DHHS COVID-19 input
##   
##
## ---------------------------

rm(list = ls(all = TRUE))

library(here)
here()

library(tidyverse)
library(stringr)

install_this_library <- function(libname) {

  if (!(libname %in% installed.packages())) {
    install.packages(libname)
  } else {
    print(paste0(libname," already installed"))
  }
}


this_path <- here("data","dhhs_lists")
files <- list.files(this_path,pattern="_.*")

col_order <- c("LGA","TotalCount","ActiveCount","SampleDate")

for (i in 1:length(files)) {
  
  this_file <- files[i]

  # read list from text file
  df <- as.data.frame(read_delim(paste0(this_path,"/",this_file),"-",col_names = FALSE,trim_ws = TRUE,skip_empty_rows = TRUE))

  cat(this_file,"\t")
  cat(ncol(df),"\t")
  cat(colnames(df),"\n")
  
  # add an empty "active cases" column
  df$Active <- NA
  
  # add a date column
  df$Date <- paste0("20",substr(this_file,2,3),"-",substr(this_file,4,5),"-",substr(this_file,6,7))
  
  # format locality: remove spaces, remove LGA class, convert to uppercase
  # df[,1] <- gsub("\\s+", " ", str_trim(df[,1]))
  df[,1] <- gsub("\\(\\w+\\)","",df[,1])
  df[,1] <- str_squish(df[,1])
  df[,1] <- toupper(df[,1])
  
  colnames(df) <- col_order

  print(df[1:2,])
  
  cat("##########\n")
  
  # save csv
  output <- paste0(here("data","dhhs_clean"),"/",sub("_","",this_file))
  write.csv(df,output,row.names = FALSE)
}


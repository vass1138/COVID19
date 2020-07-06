## ---------------------------
##
## Script name: parse_table_files.R
##
## Purpose of script: Parse table files
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

install_this_library <- function(libname) {

  if (!(libname %in% installed.packages())) {
    install.packages(libname)
  } else {
    print(paste0(libname," already installed"))
  }
}

this_path <- here("data","dhhs_tables")
files <- list.files(this_path,pattern="\\d+\\.txt$")

col_order <- c("LGA","TotalCount","ActiveCount","SampleDate")

for (i in 1:length(files)) {
  
  this_file <- files[i]

  df <- read_csv(paste0(this_path,"/",this_file),col_names=TRUE,trim_ws=TRUE)

  cat(this_file,"\t")
  cat(ncol(df),"\t")
  cat(colnames(df),"\n")
  
  if (ncol(df)==3) {
    
    colnames(df) <- c("LGA","TotalCount","SampleDate")
    
    df$ActiveCount <- NA
    
    df <- df[,col_order]

  } else if (ncol(df)==4) {
    
    colnames(df) <- col_order
    
  } else {
    
    print(paste0("Unknown column format:", this_file))
    return()
    
  }
  
  df <- df[grep("^LGA",df$LGA,invert=TRUE),]
  df <- df[grep("^Confirmed",df$LGA,invert=TRUE),]
  df <- df[grep("^Local",df$LGA,invert=TRUE),]
  df <- df[grep("Total:",df$LGA,invert=TRUE),]
  df <- df[grep("Total",df$LGA,invert=TRUE,ignore.case = TRUE),]
  
  df$LGA <- trimws(gsub("\\*","",df$LGA))
  
  print(df[1:2,])
  
  cat("##########\n")

  
  # save csv
  output <- paste0(here("data","dhhs_clean"),"/",sub("_","",this_file))
  write.csv(df,output,row.names = FALSE)
}


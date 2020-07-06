## ---------------------------
##
## Script name: scrape_dhhs.R
##
## Purpose of script:
##  Scrape the COVID-19 tabular data from the daily DHHS media releases.
##
## Created By: Dr. Emanuel Vassiliadis
##
## Date Created: 2020-06-05
##
## LinkedIn: https://www.linkedin.com/in/evassiliadis/
##
## ---------------------------
##
## Notes:
##   Does not work reliably because the data format is not consistent:
##   - different column counts
##   - tables replaced with lists
## ---------------------------

rm(list = ls(all = TRUE))

library(here)
getwd()

install_this_library <- function(libname) {

  if (!(libname %in% installed.packages())) {
    install.packages(libname)
  } else {
    print(paste0(libname," already installed"))
  }
}

install_this_library("rvest")

library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)

# DATE = DD-MMM-YYYY
url_template <- "https://www.dhhs.vic.gov.au/coronavirus-update-victoria-{DATE}"

test_daily_data <- function(this_date) {
  
  df <- data.frame()
  result <- vector(mode = "list", length = 0)
  
  date_string <- gsub("^0","",format(this_date,format="%d-%B-%Y"))
  
  # url <- sub("\\{DATE\\}",this_date,url_template)
  url <- sub("\\{DATE\\}",date_string,url_template)
  
  print(url)
}


load_daily_data <- function(this_date) {

  # init  
  df <- data.frame()
  url_list <- list()
  result <- vector(mode = "list", length = 0)

  # setup date formats to try
  date_string <- format(this_date,format="%d-%B-%Y")
  url_list <- append(url_list,sub("\\{DATE\\}",date_string,url_template))
  
  date_string <- format(this_date,format="%d-%B%Y")
  url_list <- append(url_list,sub("\\{DATE\\}",date_string,url_template))
  
  date_string <- format(this_date,format="%d-%B")
  url_list <- append(url_list,sub("\\{DATE\\}",date_string,url_template))
  
  date_string <- format(this_date,format="%A-%d-%B")
  url_list <- append(url_list,sub("\\{DATE\\}",date_string,url_template))
  
  date_string <- format(this_date,format="%A-%d-%B-%Y")
  url_list <- append(url_list,sub("\\{DATE\\}",date_string,url_template))
  
  date_string <- gsub("^0","",format(this_date,format="%d-%B-%Y"))
  url_list <- append(url_list,sub("\\{DATE\\}",date_string,url_template)) 
  
  # 
  for(this_url in url_list) {
    
    # this_url <- "https://www.dhhs.vic.gov.au/media-release-coronavirus-update-victoria-wednesday-10-june"
  
    cat(paste0("Trying ", this_url,"... "))
  
    this_html <- tryCatch ({
      read_html(this_url)
    },
    error = function(e){NA}
    )
    
    if (!is.na(this_html)) {
      
      cat("Got page!\n")
    
      cat("Trying table ...")
      
      dt <- this_html %>% 
        html_nodes("table") %>%
        rvest::html_table(fill = TRUE)
    
      # html table (as list)
      # dt <- html %>% 
      #   rvest::html_nodes('body') %>% 
      #   xml2::xml_find_all(".//table[@data-mce-selected]") %>% 
      #   rvest::html_table()
      
      # dt <- this_html %>% 
      #   rvest::html_nodes('body') %>% 
      #   xml2::xml_find_all(".//table") %>% 
      #   rvest::html_table(fill = TRUE)
      
      if (length(dt) > 0) {
        
        cat("OK!\n")
        
        # assign column names from first row
        df <- as.data.frame(dt)
        colnames(df) <- seq(1,ncol(df))
        
      } else {
        
        cat("Failed!  Skipping. \n")

      }
    
      #if (colnames(df)[1] != "LGA") {
      #  colnames(df) <- df[1,]
      #}
      
      # remove column names in first row
      # df <- df[-1,]
      
      break

    } else {
      
      cat("Failed!\n")
      
    }
    
  }
  
  return(df)

}

#
# MAIN
#

dates <- seq(as.Date("2020-06-21"), as.Date("2020-06-21"), by=1)

data <- data.frame()

for (i in seq_along(dates)) {
  
  df <- load_daily_data(dates[i])
  cat(paste0("Found ",nrow(df)," rows\n"));  
  if (nrow(df) > 0) {

    
    df$date <- as.Date(dates[i])
    
    # if (nrow(data) == 0) {
    #   data <- df
    # } else {
    #   data <- rbind(data,df)
    # }
    
    filename <- paste0(format(dates[i],"%y%m%d"),".txt")
    
    write.csv(df,filename,row.names=FALSE)
  }
}



# colnames(data) <- c("LGA","TotalCases","CurrentCases","Date")
# 
# # remove column names present as rows
# data <- data %>% 
#   filter(LGA != "LGA") %>%
#   arrange(Date) 
# 
# data %>%
#   filter(LGA == "MELBOURNE") %>%
#   ggplot(aes(Date,TotalCases)) +
#   geom_point()



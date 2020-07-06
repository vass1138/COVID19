## ---------------------------
##
## Script name: covid19_vic_200404.R
##
## Purpose of script:
## Combine Covid and ABS data for single date.
## 
##
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

getwd()
setwd('/home/emanuel/R/Covid19')

# install.packages("ggrepel")

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)

covid <- read.csv("covid19_vic_200404.csv",stringsAsFactors = FALSE)   

# ABS data
seifa <- read.csv("seifa_2033055001 - lga indexes.csv",stringsAsFactors = FALSE)
pop <- read.csv("regional_pop_growth_32180ds0002_2017-18.csv",stringsAsFactors = FALSE)

# merge ABS data
df <- merge(seifa,pop,by.x='Code',by.y='Code')

# drop duplicate colum
df$Name.y <- NULL

# Split name
df$Name <- gsub("([A-Za-z]+).*", "\\1", df$Name.x)

# merge Covid data (inner join, ie. if no covid data, row dropped)
dc <- merge(df,covid,by.x='Name',by.y="Name")

dc$Rate <- (dc$Cases / dc$Population) * 100000

dc[, 4:7] <- sapply(dc[, 4:7], as.numeric)

# reclassify deciles into pentiles
dc$ER5 <- ceiling(dc$DecileER / 2)
dc$EO5 <- ceiling(dc$DecileEO / 2)
dc$SEAD5 <- ceiling(dc$DecileSEAD / 2)
dc$SED5 <- ceiling(dc$DecileSED / 2)

dc <- dc %>% arrange(desc(ER5))

xlab <- expression(paste('Population Density (persons/',km^2,')'))

ggplot(dc,aes(x=Density,y=Rate,size=ER5,color=Type)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(2,7), name="Economic\nResource\nPentile") +
  geom_text_repel(data=subset(dc, Density > 1000 | Rate > 20),
            aes(Density,Rate,label=Name,size=2), show.legend = FALSE) +
  labs(x=xlab,
       y="Confirmed Cases (per 100000 people)",
       title="COVID-19 Confirmed Cases vs LGA Population Density\nApril 4th, 2020"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

dc <- dc %>% arrange(desc(EO5))

ggplot(dc,aes(x=Density,y=Rate,size=EO5,color=Type)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(2,7), name="Education\nOccupation\nPentile") +
    geom_text_repel(data=subset(dc, Density > 1000 | Rate > 20),
                  aes(Density,Rate,label=Name,size=2), show.legend = FALSE) +
  labs(x="Population Density (persons/km2)",
       y="Confirmed Cases (per 100000 people)",
       title="Confirmed Cases vs LGA Population Density, April 4th, 2020"
)

dc <- dc %>% arrange(desc(SEAD5))

ggplot(dc,aes(x=Density,y=Rate,size=SEAD5,color=Type)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(2,7), name="Socio-Economic\nDisadvantage\nPentile") +
  geom_text_repel(data=subset(dc, Density > 1000 | Rate > 20),
                  aes(Density,Rate,label=Name,size=2), show.legend = FALSE) +
  labs(x="Population Density (persons/km2)",
       y="Confirmed Cases (per 100000 people)",
       title="Confirmed Cases vs LGA Population Density, April 4th, 2020"
  )

dc <- dc %>% arrange(desc(SED5))

ggplot(dc,aes(x=Density,y=Rate,size=SED5,color=Type)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(2,7), name="Socio-Economic\nDisadvantage\nPentile") +
  geom_text_repel(data=subset(dc, Density > 1000 | Rate > 20),
                  aes(Density,Rate,label=Name,size=2), show.legend = FALSE) +
  labs(x="Population Density (persons/km2)",
       y="Confirmed Cases (per 100000 people)",
       title="Confirmed Cases vs LGA Population Density, April 4th, 2020"
  )



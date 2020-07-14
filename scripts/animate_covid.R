## ---------------------------
##
## Script name: animate_covid.R
##
## Purpose of script: Load all clean data
##
##
## Author: Emanuel Vassiliadis
##
## Date Created: 2020-07-02
##
## LinkedIn: https://www.linkedin.com/in/evassiliadis/
##
## ---------------------------
##
## Notes:
## Customised for DHHS COVID-19 input.
## Use geom_text() so labels remained fixed relative to data points  
##
## ---------------------------

rm(list = ls(all = TRUE))

library(here)
here()

library(tidyverse)
library(stringr)
library(ggrepel)

install_this_library <- function(libname) {
  
  if (!(libname %in% installed.packages())) {
    install.packages(libname)
  } else {
    print(paste0(libname," already installed"))
  }
}


this_path <- here("data","dhhs_clean")
files <- list.files(this_path,pattern="\\d+\\.txt$")

col_order <- c("LGA","TotalCount","ActiveCount","SampleDate")

data <- data.frame()

for (i in 1:length(files)) {
  
  this_file <- files[i]
  
  # read list from text file
  df <- as.data.frame(read_csv(paste0(this_path,"/",this_file),col_names=TRUE,trim_ws=TRUE))

  data <- rbind(df,data)

}

# convert to number
data$ActiveCount <- as.numeric(data$ActiveCount)
data$TotalCount <- as.numeric(data$TotalCount)

# change NAs to zero
data <- data %>%
  mutate(ActiveCount = replace_na(ActiveCount,0),
         TotalCount = replace_na(TotalCount,0))

# remove rows where locality is NA
data <- data %>%
  drop_na(LGA)

data$LGA <- toupper(data$LGA)

covid <- data

##

# ABS data
seifa_file <- paste0(here("data","misc"),"/","seifa_2033055001 - lga indexes.csv")
seifa <- read.csv(seifa_file,stringsAsFactors = FALSE)

pop_file <- paste0(here("data","misc"),"/","regional_pop_growth_32180ds0002_2017-18.csv")
pop <- read.csv(pop_file,stringsAsFactors = FALSE)

# merge ABS data
dabs <- merge(seifa,pop,by.x='Code',by.y='Code')

# drop duplicate colum
dabs$Name.y <- NULL

# Split name
dabs$Name <- trimws(toupper(gsub("\\(\\w+\\)", "", dabs$Name.x)))
dabs$Name <- trimws(gsub("\\(VIC.\\)","",dabs$Name))

# merge Covid data (inner join, ie. if no covid data, row dropped)
dc <- merge(dabs,covid,by.x='Name',by.y="LGA")

dc$Rate <- (dc$TotalCount / dc$Population) * 100000

dc$Type <- "NA"

dc <- dc %>%
  mutate(Type = if_else(str_detect(Name.x,"\\(C\\)"),"C",Type)) %>%
  mutate(Type = if_else(str_detect(Name.x,"\\(RC\\)"),"RC",Type)) %>%
  mutate(Type = if_else(str_detect(Name.x,"\\(S\\)"),"S",Type))

#dc %>%
#  filter(str_detect(Name.x,"^Sto"))

dc[, 4:7] <- sapply(dc[, 4:7], as.numeric)

# reclassify deciles into pentiles
dc$ER5 <- ceiling(dc$DecileER / 2)
dc$EO5 <- ceiling(dc$DecileEO / 2)
dc$SEAD5 <- ceiling(dc$DecileSEAD / 2)
dc$SED5 <- ceiling(dc$DecileSED / 2)

dc <- dc %>% arrange(desc(ER5))

xlab <- expression(paste('Population Density (persons/',km^2,')'))

#install.packages("gganimate")
#install.packages("gifski")
#install.packages("av")
library(gganimate)
library(gifski)
library(av)

#install.ffmpeg()

ds <- dc %>%
  filter(Type=="C")

nsecs <- length(table(ds$SampleDate)) * 0.33

p <- ds %>%
  filter(Type=="C") %>%
  ggplot(aes(x=Density,y=Rate,size=ER5,color=Type)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(2,7), name="Economic\nResource\nPentile") +
#  geom_text_repel(data=subset(ds, Density > 1000 | Rate > 45),
#                  aes(Density,Rate,label=Name,size=1), show.legend = FALSE) +
#  geom_text_repel(data=subset(ds, Rate > 45),
#                aes(Density,Rate,label=Name,size=1.5), seed=10000, point.padding=0.2, show.legend = FALSE) +
geom_text(data=subset(ds, Rate > 45),
                  aes(Density,Rate,label=Name,size=1.5), hjust=0, nudge_x=100, nudge_y=10, show.legend = FALSE) +
  labs(x=xlab,
       y="Confirmed Cases (per 100000 people)",
       title="Total COVID-19 Cases vs LGA Population Density by Day",
       caption="www.linkedin.com/in/evassiliadis/\ngithub.com/vass1138/covid19"
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust=1,face="italic"))

q <- p + transition_time(SampleDate) +labs(subtitle = "Melbourne, Australia - Date: {frame_time}")

a <- animate(q,renderer = ffmpeg_renderer(),width=1280,height=720,res=150,duration=nsecs)
anim_save("foo.mp4",a)





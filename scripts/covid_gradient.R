## ---------------------------
##
## Script name: covid_gradient.R
##
## Purpose of script: Plot gradient of clean data
##
##
## Author: Emanuel Vassiliadis
##
## Date Created: 2020-07-17
##
## LinkedIn: https://www.linkedin.com/in/evassiliadis/
##
## ---------------------------
##
## Notes:
## Uses combined COVID ABS dataset from previous scripts 
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

dc$NormCount <- (dc$TotalCount / dc$Population) * 100000

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

# xlab <- expression(paste('Population Density (persons/',km^2,')'))
xlab <- "Date"

# reformat date
dc <- dc %>%
  mutate(NumericDate = as.numeric(SampleDate))

lga_NormCounts_ordered <- dc %>%
  filter(Type=="C") %>%
  group_by(Name) %>%
  top_n(1,NormCount) %>%
  arrange(-NormCount) %>%
  select(Name,SampleDate,NormCount) %>%
  ungroup()

# top 9 LGAs (used for facets)
lga_NormCounts_top <- lga_NormCounts_ordered[1:9,]$Name

# compute differential, remove rows where Name changes across lag  
ds <- dc %>%
  arrange(Name,SampleDate) %>%
  mutate(InfectionRate = (NormCount-lag(NormCount))/(NumericDate-lag(NumericDate))) %>%
  filter(Name==lag(Name))

# identify latest count per name
ds <- ds %>%
  group_by(Name) %>%
  mutate(MaxCount=max(NormCount)) %>%
  mutate(FacetLabel=paste0(Name," (",format(round(MaxCount,0),nsmall=0),")")) %>%
  ungroup()

# plot
p <- ds %>%
  filter(Name %in% lga_NormCounts_top) %>%
  mutate(NameOrder=fct_reorder(FacetLabel,-MaxCount)) %>%
  ggplot(aes(x=SampleDate,y=InfectionRate)) +
  geom_smooth(alpha=0.5) +
  facet_wrap(~ NameOrder) +
  labs(x=xlab,
       y="Infection Rate (per 100000 people per Day)",
       title="COVID-19 Daily Infection Rate in Victoria, Australia",
       subtitle="Top 9 Local Government Areas by Normalised Total Positive Cases",
       caption="www.linkedin.com/in/evassiliadis/\ngithub.com/vass1138/covid19"
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust=1,face="italic"))

p

ggsave("infection_rate_top9.png")






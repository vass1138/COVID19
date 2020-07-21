## ---------------------------
##
## Script name: covid_animate_bubble.R
##
## Purpose of script: Load all clean data, combine with ABS LGA data
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

#install.packages("gganimate")
#install.packages("gifski")
#install.packages("av")
library(gganimate)
library(gifski)
library(av)

#install.ffmpeg()
if (!exists("dc")) {
  source("load_clean_data.R")
}

xlab <- expression(paste('Population Density (persons/',km^2,')'))

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
       title="Normalised COVID-19 Cases vs LGA Population Density by Day",
       caption="www.linkedin.com/in/evassiliadis/\ngithub.com/vass1138/covid19"
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust=1,face="italic"))

q <- p + transition_time(SampleDate) +labs(subtitle = "Melbourne, Australia - Date: {frame_time}")

a <- animate(q,renderer = ffmpeg_renderer(),width=1280,height=720,res=150,duration=nsecs)
anim_save("foo.mp4",a)





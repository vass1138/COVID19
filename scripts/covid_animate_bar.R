## ---------------------------
##
## Script name: covid_animate_bar.R
##
## Purpose of script: Animated horizontal bar chart of clean data
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
## Loads data from external script
##
## ---------------------------

library(gganimate)

if (!exists("dc")) {
  source("load_clean_data.R")
}

lga_rates_ordered <- dc %>%
  group_by(SampleDate) %>%
  mutate(Rank=rank(-Rate)) %>%
  group_by(Name) %>%
  filter(Rank<=10) %>%
  select(Name,SampleDate,Rate,Rank) %>%
  ungroup()

ds <- dc %>%
  group_by(SampleDate) %>%
  summarise(DailyTotal = sum(TotalCount)) %>%
  select(SampleDate,DailyTotal)
  

# https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da
staticplot <- ggplot(lga_rates_ordered,
                aes(Rank, group = Name, 
                    fill = as.factor(Name),
                    color = as.factor(Name))) +
  geom_tile(aes(y = Rate/2,
                height = Rate,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Name, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Rate,label = format(Rate,digits=0), hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma,limits=c(0,400)) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=24, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=10, hjust=1, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

staticplot

anim <- staticplot + transition_states(SampleDate, transition_length = 4, state_length = 1,wrap=TRUE) +
  view_follow(fixed_x = TRUE)  +
  labs(title =  "Top 10 COVID-19 Localities in Victoria, Australia",
       subtitle  =  'Data sources: DHHS/ABS - Confirmed Cases per 100K People - Date : {closest_state}',  
       caption  = "www.linkedin.com/in/evassiliadis/\ngithub.com/vass1138/covid19")

mp4 <- animate(anim, duration=90, fps = 20,  width = 1280, height = 720, 
        renderer = ffmpeg_renderer())

anim_save("covid_lga_bar.mp4", animation = mp4 )

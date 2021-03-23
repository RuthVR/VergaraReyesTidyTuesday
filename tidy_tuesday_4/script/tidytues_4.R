### This is my forth tidy tuesday ###
### Created by Ruth Vergara Reyes ###
### Updated on: 2021-03-22        ###


########################################################################

##### Load Libraries #####
library(tidytuesdayR)
library(tidyverse)
library(here)
library(RColorBrewer)

########################################################################

##### Load Data #####
tuesdata <- tidytuesdayR::tt_load(2021, week = 12)
game_data <- tuesdata$games

##### Data Analysis #####
Dota_2_even_years <- game_data %>%
  filter(complete.cases(.))%>%
  filter(gamename == "Dota 2")%>%
  group_by(year)%>%
  write_csv(here("tidy_tuesday_4", "data", "Dota_2_data.csv"))

##### Making the Plot #####
ggplot(data = Dota_2_even_years,
       mapping = aes(x = year,
                     y = gain,
                     color = as.factor(year)))+
  geom_point(show.legend = FALSE)+
  facet_wrap(~month)+
  theme_bw()+
  labs(x = "Year",
       y = "Gain",
       title = "Dota 2 Gains Throughout the Years ",
       subtitle = "This plot shows the gains that the game Dota 2 has had since its release in 2012. 
It shows the gains that each month had every year.")+  ### new labels ##
  theme(legend.box = "none",
        axis.title = element_text(size = 10),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 13))+
  scale_color_brewer(palette = "Set1")

ggsave(here("tidy_tuesday_4", "output", "Dota2_plot.png"),
       width = 10, height = 7)  


### This is my first tidy tuesday plot that I will be making. ###
### Created by Ruth Vergara Reyes  ###
### Updated on: 2021-02-23 ###

########################################################################

##### Load Libraries #####
library(tidytuesdayR)
library(tidyverse)
library(here)
library(calecopal)

########################################################################

##### Load Data #####

tuesdata <- tidytuesdayR::tt_load(2021, week = 9)
employed <- tuesdata$employed                                                   ### this is how I got the employed data ###
earn <- tuesdata$earn                                                           ### this is how I got the earned data ###

view(earn)
view(employed)

########################################################################

##### Earned Data Clean Up #####

earn_clean <- earn %>%
  drop_na(sex,age, year, quarter, n_persons, median_weekly_earn)%>%           ### This line of code removes NAs from data ###
  filter(sex != "Both Sexes",
         race != "All Races")%>%
  select(-ethnic_origin, -quarter, -age)%>%
  filter(complete.cases(.)) %>%                                                ### filters out everything that is not a complete row ###
  group_by(sex, race, year, n_persons, median_weekly_earn) %>%
  write_csv(here("tidy_tuesday_1", "data", "earn_clean_data.csv"))  # export as a csv to the right folder

view(earn_clean)

##### Plot Analysis #####

ggplot(data = earn_clean,
       mapping = aes(x = sex, 
                     y = median_weekly_earn,
                     color = race))+                                            ### values I used in my plot's x and y axis and colors are based on the race ###
  geom_violin(show.legend = TRUE)+                                              ### the type of plot I made using "Earn_clean" data ###
  facet_wrap(~ year)+ 
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0))+                                                       ### how I separated the plot into the different years ### 
  labs(x = "Sex",
       y = "Average Weekly Salary",
       title = "Average Weekly Salary")+                    ### New labels for my title and axis titles ###
   theme(axis.title = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        title = element_text(size = 20))+
  scale_color_manual(values = cal_palette(name = "superbloom3"))                ### Code used to customize my colors in my plot ###

ggsave(here("tidy_tuesday_1", "output", "Average_Weekly_Salary.png"),
         width = 10, height = 7)                                               ### How I changed the style of my title ###

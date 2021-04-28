### This is my 8th tidy tuesday. I will be analysing 
### Created by: Ruth Vergara Reyes 
### Updated on: 2021-04-27


##### Load Libraries ##########################################################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(calecopal)
library(patchwork)
###############################################################################


##### Load Data ###############################################################
tuesdata <-  tidytuesdayR::tt_load(2021, week = 18)
departures <- tuesdata$departures
###############################################################################


##### Cleaning up the Data ####################################################
dept_involuntary <- departures %>% 
  arrange(dismissal_dataset_id)%>%               ## Arranging the data by id
  filter(departure_code != 5,
         departure_code != 6, 
         departure_code != 7,
         departure_code != 8,
         departure_code != 9) %>%                ## Getting rid of any code that is not involuntary departure
  select(-(ceo_dismissal:"_merge")) %>%          ## Getting rid of columns I will not use
  filter(fyear > 1999) %>%                       ## Getting rid of any data that is not 2000 - 2020
  mutate(CEO_left_due_to = case_when(
    departure_code == 1 ~ "Death",
    departure_code == 2 ~ "Illness",
    departure_code == 3 ~ "Poor Job Performance",
    departure_code == 4 ~ "Legal Violation"))%>% ## Making a new column that specifies the code of departure
  group_by(fyear, CEO_left_due_to)%>%            ## How I am grouping the data I will use to make a plot
  write_csv(here("tidy_tuesday_8", "data", "Involuntary_data.csv")) %>%
  tally() -> dep1                                ## Counting the number of reasons for the plot
   

Dept_voluntary <- departures %>%
  arrange(dismissal_dataset_id)%>%               ## Arranging the data by id
  filter(departure_code != 1,
         departure_code != 2, 
         departure_code != 3,
         departure_code != 4,
         departure_code != 8,
         departure_code != 9) %>%               ## Getting rid of any code that is not voluntary departure
  select(-(ceo_dismissal:"_merge")) %>%         ## Getting rid of columns I will not use
  filter(fyear > 1999) %>%                      ## How I am grouping the data I will use to make a plot
  mutate(CEO_left_due_to = case_when(
    departure_code == 5 ~ "Retired",
    departure_code == 6 ~ "New Opportunity",
    departure_code == 7 ~ "Other"))%>%          ## Making a new column that specifies the code of departure        
  group_by(fyear, CEO_left_due_to)%>%
  write_csv(here("tidy_tuesday_8", "data", "Voluntary_data.csv"))%>%
  tally() -> dep2                               ## Counting the number of reasons for the plot

###############################################################################


##### Making the Plots ########################################################
colors <- cal_palette("chaparral1",4)           ## Making my color palette for plot 1
cal2 <- cal_palette("tidepool", 3)              ## Making the color palette for plot 2

invol_plot <- dep1 %>%
  ggplot(aes(x = n, 
             y = fyear,
             color = CEO_left_due_to))+         ## Making my plot aesthetics 
  geom_line(aes(group = fyear),
            color = "black")+                   ## How I got the lines in the plot
  geom_point(size = 3,
             alpha = 0.7)+                      ## Making the points in the plot
  labs(title = "Involuntary",
       x = "Count",
       y = "Year",
       color = "Reason for 
  Leaving:")+                                   ## Customizing the titles for the plot
  scale_color_manual(values = colors)+
  theme_classic()+                              ## Theme of plot 
  theme(legend.position = "left")               ## Changing where my legend position is at. 
ggsave(here("tidy_tuesday_8", "output", "Involuntary_plot.png"),
       width = 10, height = 7)                  ## saving the plot 
  
invol_plot

### REPEATED the above steps for the second plot *Just used different data* ###

vol_plot <- dep2 %>%
ggplot(aes(x = n, 
           y = fyear,
           color = CEO_left_due_to))+
  geom_line(aes(group = fyear),
            color = "black")+
  geom_point(size = 3,
             alpha = 0.7)+
  labs(title = "Voluntary",
       x = "Count",
       y = "Year",
       color = "Reason for
  Leaving:")+
  scale_color_manual(values = cal2)+
  theme_classic()+
  theme(legend.position = "right")

ggsave(here("tidy_tuesday_8", "output", "Voluntary_plot.png"),
       width = 10, height = 7)    

vol_plot

##########################

patch <- (invol_plot+vol_plot)+              ## Putting the plots together 
  plot_annotation(
    title="CEO departures between 2000-2020",
    theme=theme(
      plot.title =element_text(size = 30)))  ## Changing plot elements 

ggsave(here("tidy_tuesday_8", "output", "Patch_plot.png"),
       width = 10, height = 7)              ## saving my FINAL Plot 
patch


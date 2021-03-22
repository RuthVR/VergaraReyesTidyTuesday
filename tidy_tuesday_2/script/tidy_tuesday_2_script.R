### This is my second Tidy Tuesday Plot. I will be using data that was collected on youtube videos. 
### Created by: Ruth Vergara Reyes 
### Updated on: 2021-03-05


##### Load Libraries ##################################################

library(tidytuesdayR)
library(tidyverse)
library(here)
library(calecopal)


##### Load Data #######################################################

tuesdata <- tidytuesdayR::tt_load(2021, week = 10)
youtube_data <- tuesdata$youtube

view(youtube_data)

##### Data Analysis ###################################################

superbowl_commercials_drinks <- youtube_data%>%
  filter(complete.cases(.))%>%
  arrange(year) %>%
  select(-superbowl_ads_dot_com_url, -youtube_url, -id, -etag, -favorite_count,
         -description, -thumbnail, -show_product_quickly, -kind) %>%
  rename(Funny = funny,
         Patriotic = patriotic,
         Celebrity = celebrity,
         Danger = danger,
         Animals = animals,
         Sexual = use_sex) %>%
  pivot_longer(cols = Funny: Sexual,      ### the cols you want to pivot ###
               names_to = "Characteristics",                  ### what name of the new cols ###
               values_to = "Value") %>%
  filter(Value != FALSE,
         brand != "E-Trade",
         brand != "NFL",
         brand != "Hynudai",
         brand != "Kia",
         brand != "Toyota",
         brand != "Doritos") %>%
  mutate(percent_liked = (like_count/view_count)*100) %>%
  write_csv(here("tidy_tuesday_2", "data", "Characteristics_of_Superbowl_Beverage_Commercials.csv"))

view(superbowl_commercials_drinks)

##### Plot ############################################################

ggplot(data = superbowl_commercials_drinks,
       mapping = aes(x = year,
                     y = percent_liked,
                     color = brand))+
  geom_point()+
  facet_wrap(~ Characteristics, scales = "free")+
  labs(x = "Year",
       y = "Liked Viewers %",
       title = "Characteristics of Superbowl Beverage Commercials")+
  theme_classic()+
  theme(axis.title = element_text(size = 10,
                                  color = "black"),
        title = element_text(size = 18),
        panel.background = element_rect(fill = "white"))+
  scale_color_manual(values = cal_palette(name = "fire", type = "continuous"))

ggsave(here("tidy_tuesday_2", "output", "Characteristics_Superbowl_beverage_comm_plot.png"),
       width = 10, height = 7)     

### This is my third tidy tuesday ###
### Created by Ruth Vergara Reyes ###
### Updated on: 2021-03-12        ###


########################################################################

##### Load Libraries #####
library(tidytuesdayR)
library(tidyverse)
library(here)
library(viridis)
library(scales)

########################################################################

##### Load Data #####

tuesdata <- tidytuesdayR::tt_load(2021, week = 11)
movie_data <- tuesdata$movies 
raw_bechdel <- tuesdata$raw_bechdel
        
view(movie_data)                                             ### The data I will be using ##
view(raw_bechdel)

########################################################################

##### Cleaning up Data #####

movie_stats <- movie_data %>%
  select(-test, -clean_test, -binary, -code:-actors,
         -awards:-error )%>%                                  ### how I got rid of some cols ###
  filter(complete.cases(.))%>%                               ### Filters out everything that is not a complete row ##
  rename(Budget = budget,
         Title = title,
         DomGross = domgross,
         IntGross = intgross,
         Genre = genre) %>%                     ### how I renames cols. ##
  filter(year > 2011,
         year <2013,
         Budget > 49000000)%>%                 ### How I filtered the data ##
  pivot_longer(cols = DomGross: IntGross,      ### the cols you want to pivot ###
               names_to = "Gross_Type",                  ### what name of the new cols ###
               values_to = "Amount")%>%
  arrange(Amount)
  
view(movie_stats)

##### Plot #####

ggplot(data = movie_stats,
       mapping = aes(x = Title,
                     y = Amount,
                     fill = Title))+           ### How I put my axis ##
  geom_col()+                                 ### type of graph ##
  facet_wrap(~Gross_Type)+                    ### how I facet wraped my plot ##
  labs(x = "Title of Film",
       y = "Gross Amount",
       title = "The Highest Grossing of Movies in 2012",
       subtitle = "This plot is comparing the domestic and internation gross of the highest grossing movies of 2012")+  ### new labels ##
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 13))+  ### how I changed certain aspects of the plot ##
  scale_fill_viridis_d()+                         ### how I got my color theme ##
  scale_x_discrete(guide = guide_axis(angle = 90))+### how i customized my axis scales ##
  scale_y_discrete(breaks = c(50000000, 100000000,150000000,500000000,1000000000,15000000000))   ### how I customized my axis scales ##

ggsave(here("tidy_tuesday_3", "output", "Comparing_gross_of_movies_of_2012.png"),
       width = 10, height = 7)   
  

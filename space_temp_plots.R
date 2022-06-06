library(readr)
library(tidyverse)
library(viridis)
library(gganimate)

#read_Delim because for some reason read_csv did not work at the moment 
#but it kept working afterwards
data_here <- read_delim("Data sets made in process/final", 
                    delim = ",")
# Demographic ---- 

## Sex ----
#bar chart
data_here %>%
  select(country, sex, year) %>%
  group_by(country, sex, year) %>%
  count() %>%
  ggplot(mapping = aes(x = year, 
                       y = n, 
                       #fill according to sex
                       fill = sex)) +
  #dodge, no stack
  geom_col(position = position_dodge(), 
           width = 0.9, 
           color = "black") +
  #facet by country
  facet_wrap(~country) +
  #years rounded in two by two from 2006
  scale_x_continuous(breaks = seq(2006, 2014, 2)) +
  #personalizing colors
  scale_fill_manual(values = c("#9E9E9E", "#973539")) +
  labs(x = "Year of Data Collection", 
       y = "Number of Observations Collected", 
       title = "Gender of Data Collected", 
       fill = "Gender") +
  theme(plot.title = element_text(face = "bold"), 
        legend.position = "bottom") 

## Race ---- 
#horizontal bar chart
data_here %>%
  #recoding race id 
  mutate(race_id = if_else(race_id == "Other", 
                           "No answer", 
                           race_id)) %>%
  select(country, race_id, year) %>%
  group_by(country, race_id, year) %>%
  count() %>%
  ggplot(mapping = aes(x = year, 
                       y = n, 
                       fill = race_id)) +
  geom_col(#position = position_dodge(), 
    width = 0.9, 
    color = "black")+
  facet_wrap(~country) +
  scale_x_continuous(breaks = seq(2006, 2014, 2)) +
  scale_fill_manual(values = c("#9E9E9E", "#973539", 
                               "#65A59D", "#613D76", 
                               "white","#1E4367")) +
  coord_flip() +
  labs(x = "Year of Data Collection", 
       y = "Number of Observations Collected", 
       title = "Race of Data Collected", 
       fill = "Race:") +
  theme(plot.title = element_text(face = "bold"), 
        legend.position = "top") +
  guides(fill = guide_legend(nrow = 1))  

## School Level ----
#histogram 
data_here %>%
  select(country, schooling_completed, year) %>%
  ggplot(mapping = aes(x = schooling_completed, 
                       fill = country)) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = c("#1E4367", "#973539", 
                               "#65A59D", "#613D76")) +
  scale_x_continuous(breaks = seq(0, 18,2)) +
  facet_wrap(~country) +
  labs(x = "Level of Schooling Completed", 
       y = "Number of Observations Collected", 
       title = "Schooling of Data Collected", 
       fill = "Country:") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

#metrics
data_here %>%
  select(country, schooling_completed, year) %>%
  group_by(country, year) %>%
  summarise(median(schooling_completed, na.rm = TRUE)) %>%
  clean_names() %>%
  rename(median = median_schooling_completed_na_rm_true) %>%
  pivot_wider(names_from = year, 
              values_from = median)

#scatterplot
scatter <- data_here %>%
  select(country, schooling_completed, year) %>%
  group_by(country, year) %>%
  summarise(median(schooling_completed, na.rm = TRUE)) %>%
  clean_names() %>%
  rename(median = median_schooling_completed_na_rm_true) %>%
  ungroup() %>%
  pivot_wider(names_from = year, 
              values_from = median) %>%
  pivot_longer(cols = c("2006", "2008", "2010", "2012", "2014"), 
               names_to = "year", 
               values_to = "median") 

ggplot(data = scatter, 
       mapping = aes(x = as.numeric(year), 
                     y = median, 
                     color = country)) +
  geom_line(size = 2) +
  facet_wrap(~country) +
  scale_color_manual(values = c("#1E4367", "#973539", 
                                "#65A59D", "#613D76")) +
  labs(x = "Year", 
       y = "Median Schooling of Observations", 
       title = "Schooling of Data Collected throughout the Years", 
       color = "Country") +
  theme(plot.title = element_text(face = "bold"))

## Urban/Rural ----
data_here %>%
  select(country, urban_rural, year) %>%
  group_by(country, urban_rural, year) %>%
  count() %>%
  ggplot(mapping = aes(x = year, 
                       y = n, 
                       fill = urban_rural)) +
  geom_col(position = position_dodge(), 
           width = 0.9, 
           color = "black") +
  facet_wrap(~country) +
  scale_x_continuous(breaks = seq(2006, 2014, 2)) +
  scale_fill_manual(values = c("#65A59D", "#9E9E9E")) +
  labs(x = "Year of Data Collection", 
       y = "Number of Observations Collected", 
       title = "Urbanization Status of Data Collected", 
       fill = "Urbanization Status") +
 # theme_minimal() +
  theme(plot.title = element_text(face = "bold")) 
  

# Topics ----
#importing data set 
data_topics <- read_csv("Data sets made in process/final")

##Big Topics ---- 
data_topics %>%
  select(country, year, personal_economy_12) %>% 
  group_by(country, year, personal_economy_12) %>%
  count() %>%
  #making time series
  ggplot(mapping = aes(x = year, 
                       y = n, 
                       color = personal_economy_12)) +
  geom_point(size = 2.5) +
  geom_line(size = 1) +
  #personalizing colors
  scale_color_manual(values = c("#1E4367", "#973539", 
                                "#65A59D", "#613D76")) +
  #faceting by country
  facet_grid(country~.) +
  labs(y = "Number of Observations", 
       x = "Year", 
       color = "Personal Economy:", 
       title = "Personal Economy of Data Collected Relative to Past Year") +
  theme(plot.title = element_text(face = "bold"), 
        legend.position = "top") +
  guides(fill = guide_legend(nrow = 1))  
  

##Crime ---- 
data_topics %>%
  select(country, year, country_main_problem) %>%
  #selecting observations where crime was deemed as the main issue
  filter(country_main_problem == "Crime") %>%
  group_by(country, year, country_main_problem) %>%
  count() %>%
  ggplot(mapping = aes(x = year, 
                       y = n)) +
  #making segment to stop at the y level
  geom_segment(aes(x = year, xend = year, y = 0, yend = n),
               color = "dark gray", lwd = 1) +
  #making points 
  geom_point(mapping = aes(color = country), 
             size = 4, 
             show.legend = FALSE) +
  #faceting by country 
  facet_wrap(~country)  +
  #expanding 0 on the bottom and 0.2 on the top relatively to y-axis
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  theme_bw() +
  labs(x = "\nYear", 
       y = "Number of Observations", 
       title = "Crime as the Main Problem the Country is Facing") +
  theme(plot.title = element_text(face = "bold"))


##Corruption ---- 
data_topics %>%
  #select relevant columns
  select(country, police_bribe_12, year) %>%
  group_by(country, police_bribe_12, year) %>%
  #count how many times in a year the police request a bribe per country
  count() %>%
  ggplot(mapping = aes(x = year, 
                       y = n, 
                       fill = police_bribe_12)) +
  #column plot
  geom_col(position = position_dodge(), 
           width = 0.9, 
           color = "black") +
  #facet by country
  facet_wrap(~country) +
  #recalculate labels of x so the years are rounded 
  scale_x_continuous(breaks = seq(2006, 2014, 2)) +
  #personalizing colors: colors were personalized differently in Rmd
  scale_fill_manual(values = c("#9E9E9E", "#973539", "black")) +
  labs(x = "\nYear of Data Collection", 
       y = "Number of Observations Collected", 
       title = "Police Bribe Requests During the Past Year in Data Collected", 
       fill = "Request:") +
  #changing theme
  theme_bw() +
  #legend at the bottom
  theme(plot.title = element_text(face = "bold"), 
        legend.position = "bottom")
  

##Democracy ---- 
data_topics %>%
  #make density plot of respect to political institutions
  ggplot(aes(x = respect_political_institutions)) +
  #personalize color
  geom_density(fill = "#6190A3") +
  #facet by country and year
  facet_grid(country ~ year) +
  labs(x = "\nDegree of Respect to Political Institutions", 
       y = "Density", 
       title = "Respect for Political Institutions in the Data Collected") +
  theme(plot.title = element_text(face = "bold"))



library(tidyverse)
library(readr)
library(tidyverse)
library(maps)
library(sf)
library(ggplot2)
library(sf)
library(rgeos)
library(rnaturalearth) 
library(rnaturalearthdata)
library(ggspatial)
library(patchwork)

#Big indicator ----
data_big_ind <- read_csv("Data sets made in process/final")

##data sets ----
#I am getting the proportions 
#so it is easier to compare
#I will annotate the first data set 
#and the others follow the same format


race_ds <- data_big_ind %>%
  group_by(country_main_problem, race_id) %>%
  #count how many times the same combination of the country main problem and 
  #race_id appears
  count() %>%
  #ungrouping data
  ungroup() %>%
  #grouping by race
  group_by(race_id) %>%
  #calculate the total of n correcponding to that race
  mutate(total = sum(n)) %>%
  #calculate the proportion
  mutate(prop = n/total)


country_ds <- data_big_ind %>%
  group_by(country_main_problem, country) %>%
  count() %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(total = sum(n)) %>%
  mutate(prop = n/total)

polit_ds <- data_big_ind %>%
  group_by(country_main_problem, left_right) %>%
  count() %>%
  ungroup() %>%
  group_by(left_right) %>%
  mutate(total = sum(n)) %>%
  mutate(prop = n/total)

##plots ----

###race ----
ggplot(data = race_ds, 
       #two categorical variables against each other
       mapping = aes(x = country_main_problem,
                     y = as_factor(race_id), 
                     #fill depends on the proportion 
                     fill = prop)) +
         geom_tile() +
  labs(y = "Race of People Surveyed", 
       x = "\nPerception about the Main Issue Facing the Country", 
       title = "Breakdown of Main Issue Facing the Country by Race", 
       fill = "Ratio from total", 
       subtitle = "Aggregate data of all years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -15, 
                                   hjust = 0), 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold")) 

###country ----
ggplot(data = country_ds, 
       mapping = aes(x = country_main_problem,
                     y = as_factor(country), 
                     fill = prop)) +
  geom_tile() +
  labs(y = "Country of People Surveyed", 
       x = "\nPerception about the Main Issue Facing the Country", 
       title = "Breakdown of Main Issue Facing the Country by Country", 
       fill = "Ratio from total", 
       subtitle = "Aggregate data of all years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -15, 
                                   hjust = 0), 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold")) 

###political posture ----
ggplot(data = polit_ds, 
       mapping = aes(x = country_main_problem,
                     y = as_factor(left_right), 
                     fill = prop)) +
  geom_tile() +
  labs(y = "Political Ideology of People Surveyed", 
       x = "\nPerception about the Main Issue Facing the Country", 
       title = "Breakdown of Main Issue Facing the Country by Political Ideology", 
       fill = "Ratio from total", 
       subtitle = "Aggregate data of all years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -15, 
                                   hjust = 0), 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold")) 

#Crime ----
#I built an app for this!! check bivariate_pie_app
    
#Corruption ----

## boxplot 1----

#importing data set 
data_corr <- read_csv("Data sets made in process/final")

#data wrangling and plot
data_corr %>%
  #getting rid of "No answer"
  filter(freq_corrup_public_off != "No answer") %>%
  #as factor so it is ordered 
  mutate(freq_corrup_public_off = factor(freq_corrup_public_off, 
                                            levels = c("Very uncommon", 
                                                       "Uncommon", 
                                                       "Common", 
                                                       "Very common"))) %>%
  ggplot(aes(x = administration_combats_corruption,
             y = freq_corrup_public_off,
             fill = freq_corrup_public_off)) +
  #box plot of both variables
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  #personalizing colors
  scale_fill_manual(values = c("#973539",  "#65A59D",
                               "#613D76", "#B3782b")) +
  labs(x = "Extent of Agreement on whether the Current Administration\n Combats Corruption", 
       y = "Perception of How Frequent is Corruption among Public Officials", 
       title = "Perceptions regarding the Frequency of Corruption and Administration's Action Against It", 
       subtitle = "Aggregate data of all years") +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold")) 



##geom_tile ----
data_corr %>%
  select(administration_combats_corruption, trust_political_parties ) %>%
  #as factor to use numbers as a kind of categorical variable
  mutate(administration_combats_corruption = as.factor(administration_combats_corruption), 
         trust_political_parties = as.factor(trust_political_parties   )) %>%
  group_by(administration_combats_corruption, trust_political_parties) %>%
  count() %>%
  ungroup() %>%
  group_by(trust_political_parties) %>%
  mutate(total = sum(n)) %>%
  #getting the proportion for more accurate comparison
  mutate(prop = n/total) %>%
  ggplot(mapping = aes(x = administration_combats_corruption,
                       y = trust_political_parties, 
                       #fill according to proportion
                       fill = prop)) +
  geom_tile() +
  labs(x = "Extent of Agreement on whether the Current Administration\n Combats Corruption", 
       y = "Extent of Agreement on Trust to Political Parties",
       title = "Perceptions regarding Trust to Political Parties and Administration's Action Against Corruption", 
       subtitle = "Aggregate data of all years") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold")) 

#calculate the correlation coefficient 
cor(data_corr$administration_combats_corruption, 
    data_corr$trust_political_parties,
    use = "complete.obs")

##boxplot 2----
data_corr %>% 
  #getting rid of NA values
  filter(!is.na(as.factor(respect_political_institutions))) %>%
  ggplot(aes(x = administration_combats_corruption, 
             #as factor to use numbers as a kind of categorical variable
             y = as.factor(respect_political_institutions), 
             fill = as.factor(respect_political_institutions))) +
  #make a box plot
  geom_boxplot(show.legend = FALSE, 
               alpha = 0.9) +
  #personalizing colors
  scale_fill_manual(values = c("#9E9E9E", "#973539", 
                               "#65A59D", "#613D76", 
                               "#B3782b", "#D0D0D0", "#6190A3")) +
  labs(x = "Extent of Agreement on whether the Current Administration\n Combats Corruption",
       y = "Degree of Respect to Political Institutions",
       title = "Respect for Political Institutions and Perception on Administration's \nAction Against Corruption",
       subtitle = "Aggregate data of all years") +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold")) 

#calculate the correlation coefficient 
cor(data_corr$administration_combats_corruption, 
    data_corr$respect_political_institutions,
    use = "complete.obs")

#Democracy ----

#loading data set
data_demo <- read_csv("Data sets made in process/final")

##Map: democracy_better by country ----
#getting mean and median
data_demo %>%
  dplyr::select(democracy_better, country) %>%
  group_by(democracy_better, country) %>%
  ungroup() %>%
  group_by(country) %>%
  #median ignoring NA values
  mutate(median = median(democracy_better, na.rm = TRUE)) %>%
  dplyr::select(-democracy_better) %>%
  distinct()

data_demo %>%
  select(democracy_better, country) %>%
  group_by(democracy_better, country) %>%
  ungroup() %>%
  group_by(country) %>%
  #mean ignoring NA values
  mutate(mean = mean(democracy_better, na.rm = TRUE)) %>%
  select(-democracy_better) %>%
  distinct()

#getting countries data
world <- ne_countries(scale = "medium", returnclass = "sf")

#adding mean and median
andean_region <- world %>%
  dplyr::select(c(name, geometry)) %>%
  #selecting data of countries of interest
  filter(name %in% c("Peru", "Colombia", "Ecuador", "Bolivia")) %>%
  #adding data found before
  mutate(median = c(5, 6, 5, 5)) %>%
  mutate(mean = c(5.04, 5.28, 4.98, 4.74))

#plots
median <- ggplot(data = andean_region) +
  geom_sf(aes(fill = median)) +
  labs(title = "Median of Agreement that Democracy is Better than other Forms\nof Governement (1: least, 7: most)", 
       subtitle = "Aggregate data of all years", 
       fill = "Median") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold"), 
        axis.text = element_blank()) 

mean <- ggplot(data = andean_region) +
  geom_sf(aes(fill = mean)) +
  labs(title = "Mean of Agreement that Democracy is Better than other Forms\nof Governement (1: least, 7: most)", 
       subtitle = "Aggregate data of all years", 
       fill = "Mean") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold"), 
        axis.text = element_blank()) 

#plots next to each other
median + mean

##Point plot salary satisfaction ----

#making labels to use later as axis text
label_a <- c("Good enough and\ncan save from it", 
             "Good", 
             "No answer", 
             "Not enough", 
             "Having a hard time")

#wrangling and plot
data_demo %>%
  select(salary_satisf, democracy_better) %>%
  
  mutate(democracy_better = factor(democracy_better)) %>%
  count(democracy_better, salary_satisf) %>%
  group_by(salary_satisf) %>%
  mutate(total = sum(n)) %>%
  mutate(prop = n/total) %>%
  ggplot(aes(x = democracy_better, 
             y = salary_satisf)) +
  geom_point(mapping = aes(size = prop), 
             color = "#613D76") +
  labs(y = "Salary", 
       x = "Extent of Agreement that Democracy is Better\nthan other Forms of Government", 
       title = "Salary and View on Democracy", 
       size = "Ratio from total", 
       subtitle = "Aggregate data of all years") +
  scale_y_discrete(labels = label_a) +
  coord_flip() +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold")) 

##Point plot education NA ----
data_demo %>%
  select(schooling_completed, democracy_better) %>%
  #as factor to use numbers as a kind of categorical variable
  mutate(democracy_better = factor(democracy_better)) %>%
  mutate(schooling_completed = factor(schooling_completed)) %>%
  count(democracy_better, schooling_completed) %>%
  group_by(schooling_completed) %>%
  mutate(total = sum(n)) %>%
  #getting proportion for better comparison
  mutate(prop = n/total) %>%
  #only get NA 
  filter(is.na(democracy_better)) %>%
  #get everything but NA
  filter(!is.na(schooling_completed)) %>%
  ggplot(aes(x = democracy_better, 
             y = schooling_completed)) +
  geom_point(mapping = aes(size = prop), 
             color = "#613D76") +
  labs(y = "Education Completed", 
       x = "Extent of Agreement that Democracy is Better\nthan other Forms of Government", 
       size = "Ratio from total", 
       title = "Education and View on Democracy", 
       subtitle = "Aggregate data of all years") +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold")) 



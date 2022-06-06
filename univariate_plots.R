library(readr)
library(patchwork)
library(tidyverse)

#loading data set 
data_uni_num <- read_csv("Data sets made in process/final")

#Demographic: schooling ----

#I built an app for demographics! check univariate_demo_app

#distribution of schooling completed
ggplot(data = data_uni_num, 
       mapping = aes(x = schooling_completed)) +
  #adjusting binwidth for better visualization
  geom_histogram(binwidth = .5, 
                 fill = "#1E4367") +
  scale_x_continuous(breaks = seq(0, 18,2)) +
  labs(x = "Schooling of Observations", 
       y = "Number of Observations in the Data Set", 
       title = "Schooling in the Data Set (2006 - 2014)", 
       subtitle = "Aggregate data of all years") +
  theme(plot.title = element_text(face = "bold"))

#summary
summary(data_uni_num$schooling_completed)

#making mode function
Mode <- function(x) {
  ux <- unique(x)
  paste("Mode is", ux[which.max(tabulate(match(x, ux)))])
}

#calculation mode of schooling completed
Mode(data_uni_num$schooling_completed)

#Topics ---- 

## Big indicators: Economy, Education, Health ----

#country_main_problem (cat) 

#get the 5 highest categories, ignore "No answer"
data_uni_num %>%
  select(country_main_problem) %>%
  group_by(country_main_problem) %>%
  count() %>%
  #2000 chosen from visualization
  filter(n > 2000) %>%
  #arranging by n 
  arrange(desc(n))

#get the lowest
data_uni_num %>%
  select(country_main_problem) %>%
  group_by(country_main_problem) %>%
  count() %>%
  arrange(n) %>%
  #getting the 5 country main problems with least observations
  head(5)

#plot
data_uni_num %>%
  select(country_main_problem) %>%
  group_by(country_main_problem) %>%
  count() %>%
  #making column to add specific color to top 5 issues
  mutate(fill_country_main_problem = if_else(country_main_problem %in% c("Corruption", 
                                                                      "Crime", 
                                                                      "Economy (problems with)", 
                                                                      "Poverty", 
                                                                      "Unemployment"), 
                                             TRUE, 
                                             FALSE)) %>%
  #making the plot
  ggplot(mapping = aes(x = country_main_problem, 
                       fill = fill_country_main_problem, 
                       y = n)) +
  geom_col(show.legend = FALSE) +
  #personalizing colors
  scale_fill_manual(values = c("#9E9E9E", "#973539")) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(y = "Number of Observations in the Data Set",
       x = "Perception about Countries' Main Problem",
       title = "Public's Perception on the Main Problem their Country is Facing in the Data Set (2006 - 2014)", 
       subtitle = "Aggregate data of all years") +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold"), 
        #making axis text of 5 main problems bold
        axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain',
                                            'bold', 'plain', 'plain', 'plain',
                                            'bold', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain',
                                            'plain', 'bold', 'plain', 'plain',
                                            'plain', 'plain', 'bold', 'plain',
                                            'plain', 'plain'), 
                                   #making axis text of 5 main problems red
                                   color = c('grey30', 'grey30', '#973539', 'grey30',
                                             '#973539', 'grey30', 'grey30', 'grey30',
                                             '#973539', 'grey30', 'grey30', 'grey30',
                                             'grey30', 'grey30', 'grey30', 'grey30',
                                             'grey30', 'grey30', 'grey30', 'grey30',
                                             'grey30', 'grey30', 'grey30', 'grey30',
                                             'grey30', 'grey30', 'grey30', 'grey30',
                                             'grey30', '#973539', 'grey30', 'grey30',
                                             'grey30', 'grey30', '#973539', 'grey30',
                                             'grey30', 'grey30')))

#I built an app for the others!!!! check univariate_big_topics_app.R
  #economy_compared_12 (cat)
  #satisf_health_services (cat)
  #satisf_publi_education (cat)

#table to make comparison of satisfaction of health services
data_uni_num %>%
  select(satisf_health_services) %>%
  group_by(satisf_health_services) %>%
  count() %>%
  #wrangling data 
  pivot_wider(names_from = satisf_health_services, 
              values_from = n) %>%
  clean_names() %>%
  #making new columns from previous columns
  #and limiting satisfaction to 
    #overall satisfied 
    #overall dissatisfied
  mutate(overall_dissatisfied = very_dissatisfied + dissatisfied, 
         overall_satisfied = somewhat_satisfied + very_satisfied) %>%
  select(overall_dissatisfied, overall_satisfied)

  #satisf_public_schools (cat)
data_uni_num %>%
  select(satisf_public_schools) %>%
  group_by(satisf_public_schools) %>%
  count() %>%
  pivot_wider(names_from = satisf_public_schools, 
              values_from = n) %>%
  clean_names() %>%
  mutate(overall_dissatisfied = very_dissatisfied + dissatisfied, 
         kind_of_satisfied = somewhat_satisfied) %>%
  select(overall_dissatisfied, kind_of_satisfied)

## Crime ----
#victim_crime_12 (cat)
ggplot(data = data_uni_num, 
       mapping = aes(x = victim_crime_12, 
                     fill = victim_crime_12)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("#1E4367", "#9E9E9E", 
                               "#973539")) +
  labs(y = "Number of Observations in the Data Set",
       x = "Victim of Crime during the Past Year (Yes/No)",
       fill = "Were you victim of crime \n during the past year?",
       title = "Whether Someone was Victime of Crime during the Past Year in the Data Set (2006 - 2014)", 
       subtitle = "Aggregate data of all years") +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold"), 
        legend.position = "top")

#proportion of people who were victimis of crime 
data_uni_num %>%
  select(victim_crime_12) %>%
  table()

#times_victim_crime_12 
data_uni_num %>%
  #filter by people who were victims of crime during the past year
  filter(victim_crime_12 == "Yes") %>%
  select(times_victim_crime_12) %>%
  ggplot(mapping = aes(x = times_victim_crime_12)) +
  #get density plot of the times a person was victim of crime
  geom_density(fill = "#613D76", 
               #adjust the curvature
               adjust = 5, 
               alpha = 0.4) +
  labs(y = "Density in the Data Set",
       x = "Victim of Crime during the Past Year (times)", 
       title = "Density Plot of the Times Someone was Victim of Crime \n During the Past Year in the Data Set (2006 - 2014)", 
       subtitle = "Aggregate data of all years") +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold"))

#wrangligh data with metrics
data_uni_num %>%
  filter(victim_crime_12 == "Yes") %>%
  select(times_victim_crime_12) %>%
  summary()
  

## Corruption ----
#freq_corrup_public_off 
data_uni_num %>%
  select(freq_corrup_public_off) %>%
  #as factor to control the order
  mutate(freq_corrup_public_off = factor(freq_corrup_public_off, 
                                         levels = c("No answer", 
                                                    "Very uncommon", 
                                                    "Uncommon", 
                                                    "Common", 
                                                    "Very common"), 
                                         ordered = TRUE)) %>%
  ggplot(mapping = aes(x = freq_corrup_public_off, 
                       fill = freq_corrup_public_off)) +
  geom_bar(color = "black", 
           show.legend = FALSE) +
  #personalizing colors
  scale_fill_manual(values = c("#9E9E9E", "#973539", 
                               "#65A59D", "#613D76", 
                               "#1E4367")) +
  labs(y = "Number of Observations in the Data Set",
       x = "\nFrequency of Corruption among Public Officials",
       title = "Public's Perception of Frequency of Corruption \namong Public Officials in the Data Set (2006 - 2014)", 
       subtitle = "Aggregate data of all years") +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold")) +
  #row chart
  coord_flip()


## Democracy ----

#satisf_democracy_country 
ggplot(data = data_uni_num, 
       mapping = aes(x = satisf_democracy_country, 
                     fill = satisf_democracy_country)) +
  #bar chart
  geom_bar(color = "black", 
           show.legend = FALSE) +
  #personalizing colors
  scale_fill_manual(values = c("#9E9E9E", "#973539", 
                               "#65A59D", "#613D76", 
                               "#1E4367")) +
  labs(y = "Number of Observations in the Data Set",
       x = "\nSatisfaction with Democracy in Country",
       title = "Public's Satisfaction with Democracy in the Data Set (2006 - 2014)", 
       subtitle = "Aggregate data of all years") +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold")) 

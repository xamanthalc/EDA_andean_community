library(tidyverse)
library(readr)
library(tidyverse)
library(GGally)
library(corrplot)
library(ggradar)
library(scales)
library(patchwork)

#loading data
data_multi <- read_csv("Data sets made in process/final")

#Assembly ---- 

#schooling complete

#wrangling data 
data_radar <- data_multi %>%
  select(schooling_completed, satisf_public_schools) %>%
  #get rid of "No answer"
  filter(satisf_public_schools != "No answer") %>%
  group_by(schooling_completed, satisf_public_schools) %>%
  #make factor to use numbers as a kind of categorical variable
  mutate(schooling_completed = as.factor(schooling_completed)) %>%
  count() %>%
  ungroup() %>%
  #pivot wider to have ready for ggradar
  pivot_wider(names_from = schooling_completed, 
              values_from = n) %>%
  select(-"NA")

#reescaling data
radar_schooling <- data_radar %>%
  mutate_each(funs(rescale), -satisf_public_schools) %>%
  mutate(satisf_public_schools = factor(satisf_public_schools, 
                                        levels = c("Very satisfied", 
                                                   "Somewhat satisfied", 
                                                   "Dissatisfied", 
                                                   "Very dissatisfied"))) %>%
  #calling ggradar to make radar plot
  ggradar(group.point.size = 1.5, 
          legend.position = "right", 
          #personalizing colors
          group.colours = c("#1E4367", "#973539", 
                            "#65A59D", "#613D76"), 
          group.line.width = .7, 
          #no percentage text
          values.radar = c("", "", ""), 
          plot.title = "Satisfaction with Public Education by\nSchool Level Completed") +
  theme(plot.title = element_text(size = 15, 
                                  face = "bold", 
                                  family = "sans"), 
        legend.position = "none") +
  #two columns for legend
  guides(color = guide_legend(ncol = 2))

#radar race
#same process as bove
data_radar_race <- data_multi %>%
  select(race_id, satisf_public_schools) %>%
  filter(satisf_public_schools != "No answer") %>%
  filter(race_id != "No answer") %>%
  filter(race_id != "Other") %>%
  group_by(race_id, satisf_public_schools) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = race_id, 
              values_from = n) 

radar_race <- data_radar_race %>%
  mutate_each(funs(rescale), -satisf_public_schools) %>%
  mutate(satisf_public_schools = factor(satisf_public_schools, 
                                        levels = c("Very satisfied", 
                                                   "Somewhat satisfied", 
                                                   "Dissatisfied", 
                                                   "Very dissatisfied"))) %>%
  ggradar(group.point.size = 1.5, 
          legend.position = "right", 
          group.colours = c("#1E4367", "#973539", 
                            "#65A59D", "#613D76"), 
          group.line.width = .7, 
          values.radar = c("", "", ""), 
          plot.title = "Satisfaction with Public Education\nby Race") +
  theme(plot.title = element_text(size = 15, 
                                  face = "bold", 
                                  family = "sans"), 
        legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2))

#putting both radar plots next to each other
radar_schooling + radar_race

#GGanimate ---- 
## administration_imp_safety, administration_imp_safety, 
data_multi %>%
  select(year, 
         country, 
         administration_imp_safety, 
         pride_living_political_system) %>% 
  group_by(year, country) %>%
  #getting median value of extent of agreement that administration imposes safety
  mutate(n_adm_imposes_safety = median(administration_imp_safety, na.rm = TRUE), 
         #getting median value of extent of pride of living under the current political system
         n_pride_living_poli = median(pride_living_political_system, na.rm = TRUE)) %>%
  #making scatterplot
  ggplot(aes(x = n_adm_imposes_safety,
             y = n_pride_living_poli, 
             color = country)) +
  #geom jitter to avoid over plotting 
  geom_jitter() +
  labs(x = "Extent of Agreement that the Current Administration Imposes Safety\n", 
       y = "\nPride of Living under the Current Political System", 
       color = "Country") +
  scale_color_manual(values = c("#1E4367", "#973539", 
                                "#65A59D", "#613D76")) +
  #animation throughout years
  transition_time(year) + 
  labs(title = "Year: {frame_time}") 


#Correlation plot ----

#selecting numerical variables
data_multi_nu <- select_if(data_fin, is.numeric)  

#making correlation data frame
data_corr <- cor(data_multi_nu,
                 use = "complete.obs")

#making correlation plot 
corrplot(data_corr,
         method = "square",
         tl.col = "black",
         tl.cex = 0.8, 
         tl.srt = 70)


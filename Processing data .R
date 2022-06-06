library(readr)
library(skimr)
library(tidyverse)

raw_data <- read_tsv("Data Collected/Raw Data.tsv")

#SUBSETTING TO THE ANDEAN REGION----

#I am only interested in the data regarding countries that are part of the Andean community
#Peru, Bolivia, Colombia, and Ecuador.

#Therefore, I am going to subset by observations where the countries are part of the Andean community 

#the variable regarding country is called "PAIS" and the numbers corresponding to 
#countries in the Andean community are 
#Peru: 11
#Bolivia: 10
#Colombia: 8
#Ecuador: 9

#SUBSETTING MY VARIABLES OF INTEREST----

data_countries_interest <- raw_data %>%
  filter(PAIS %in% c(11, 10, 9, 8))

str(data_countries_interest) #49596 observations in total

#I only want to keep variable that could be of potential interest for my EDA
#I have decided that those will be: 
#PAIS: country.
#WAVE: not sure what this is, but I suspect it is duplicate of the variable year.
#YEAR: year when the survey was taken.
#ESTRATOSEC: size of municipality. 
#UR : whether the survey was taken in a rural or urban area. 
#TAMANO: size of the place where the survey was taken. 
#IDIOMAQ: language in which the survey was filled. 
#Q1: sex of the person. 
#LS3: how satisfied the interviewee is with their life. 
#A4: most serious problem the country is facing. 
#SOCT2: perception on whether the country's economy is better/same/worse than 12 months ago. 
#IDIO2: perception on whether the person's economy is better/same/worse than 12 months ago. 
#CP5: whether the person has tried to solve a problem in their community during the past year. 
#L1: Scale left to right for political parties. 
#PROT3: whether the person has participated in a protest during the past year. 
#etc ....

data_subset_1 <- data_countries_interest %>%
  select(c(PAIS, WAVE, YEAR, ESTRATOSEC, UR, TAMANO, IDIOMAQ, Q1, LS3,
           A4, SOCT2, IDIO2, CP5, L1, PROT3, JC10, JC13, JC15A, VIC1EXT,
           VIC1EXTA, VIC1HOGAR, PESE1, PESE2, AOJ12, B2, B4, B6, B10A, B12,
           B13, B18, B21, B21A, B21, B47A, N9, N11, N15, M1, SD2NEW2, SD3NEW2,
           SD6NEW2, ROS4, ING4, MIL7, PN4, EXC2, EXC7, POL1, ED, Q10NEW_12, 
           Q10NEW_14, Q10D, ETID))

#data_subset_1 still has too many variables (50), so I will start narrowing down by 
  #getting rid of variable that could be misleading 
  #getting rid of potential duplicate variables

#misleading due to the value of currency in each country 
#Q10NEW_12 has 42987 NAS (out of 49596)
#Q10NEW_14 has 43247 NAS (out of 49596)

setequal(data_subset_1$WAVE, data_subset_1$YEAR) #TRUE

data_subset_2 <- data_subset_1 %>%
  select(-c(Q10NEW_12, Q10NEW_14, WAVE))

#the variables in this data set are:
#PAIS: country.
#YEAR: year when the survey was taken.
#ESTRATOSEC: size of municipality. 
#UR : whether the survey was taken in a rural or urban area. 
#TAMANO: size of the place where the survey was taken. 
#IDIOMAQ: language in which the survey was filled. 
#Q1: sex of the person. 
#LS3: how satisfied the interviewee is with their life. 
#A4: most serious problem the country is facing. 
#SOCT2: perception on whether the country's economy is better/same/worse than 12 months ago. 
#IDIO2: perception on whether the person's economy is better/same/worse than 12 months ago. 
#CP5: whether the person has tried to solve a problem in their community during the past year. 
#L1: Scale left to right for political parties. 
#PROT3: whether the person has participated in a protest during the past year. 
#JC10: views of military intervention when there is a lot of crime. 
#JC13: views of military intervention when there is a lot of corruption. 
#JC15A: views on closing congress given that the country is in a difficult time. 
#VIC1EXT: whether the person has been victim of crime in the past year. 
#VIC1EXTA: how many times a person has been victim of crime during the past year.
#VIC1HOGAR: whether the person's family has been victim of crime in the past year. 
#PESE1: perception on level of violence in neighborhood compared to other neighborhoods.
#PESE2: perception on level of violence in neighborhood compared to last year.
#AOJ12: trust on the justice system given that the person is victim of crime to punish the guilty.
#B2: respect to politician institutions in country. 
#B4: pride of living under the current political system in country. 
#B6: whether the person should support the political system in country.
#B10A: trust on the justice system. 
#B12: trust on armed forces.
#B13: trust on national congress.
#B18: trust on national police. 
#B21: trust on political parties.
#B21A: trust on president/prime minister.
#B47A: trust in electoral process.
#N9: perception on how the current administration combats corruption. 
#N11:perception on how the current administration improves safety
#N15: perception on how the current administration manages the economy well
#M1: rating the performance of current president
#SD2NEW2: satisfaction with the condition of streets, roads, and highways
#SD3NEW2: satisfaction with the quality of public schools
#SD6NEW2: satisfaction with the quality of public and health services
#ROS4: agreement with the statement that the government should implement policies to close the economic gap
#ING4: agreement that democracy is the best form of government
#MIL7: perception about whether armed forces shpuld participate in combating crime and violence
#PN4: satisfaction level regarding how democracy works in their country
#EXC2: whether the police has asked for a bribe during the past year
#EXC7: perception on how common is corruption among public officials 
#POL1: level of interest in politics
#ED: last year of education the person passed
#Q10D: whether their salary and total household income is enough
#ETID: self-identification race

#CHECKING YEARS RECORDED FOR EACH COUNTRY---- 
#Peru
peru_subset <- data_subset_2 %>%
  filter(PAIS == 11)
unique(peru_subset$YEAR)

#Bolivia
bolivia_subset <- data_subset_2 %>%
  filter(PAIS == 10)
unique(bolivia_subset$YEAR)

#Colombia
colombia_subset <- data_subset_2 %>%
  filter(PAIS == 8)
unique(colombia_subset$YEAR)

#Ecuador
ecuador_subset <- data_subset_2 %>%
  filter(PAIS == 9)
unique(ecuador_subset$YEAR)

#the results show that Peru is the only country that was not surveyed during 2004, therefore, I prefer dropping 
#all observations for that year so I can work with equivalent data for all countries
ecuador_subset <- ecuador_subset %>%
  filter(YEAR != 2004)

bolivia_subset <- bolivia_subset %>%
  filter(YEAR != 2004)

colombia_subset <- colombia_subset %>%
  filter(YEAR != 2004)

data_subset_2 <- data_subset_2 %>%
  filter(YEAR != 2004)

#checking
subsets_rows <- nrow(ecuador_subset) + nrow(peru_subset) + nrow(colombia_subset) + nrow(bolivia_subset)
setequal(subsets_rows, nrow(data_subset_2)) #TRUE

#ADJUSTING NUMBER OF OBSERVATIONS PER YEAR AND COUNTRY ----
data_subset_2 %>%
  group_by(PAIS, YEAR) %>%
  count() %>%
  ggplot(mapping = aes(x = as.character(YEAR), 
                       y = n,
                       fill = as.character(PAIS))) +
  scale_fill_discrete(name = "Country",
                      labels = c("Bolivia", "Peru", "Colombia", "Ecuador")) +
  geom_col(position = position_dodge2(), 
           width = 0.7) +
  labs(x = "Year of Survey", 
       y = "Number of Surveys", 
       title = "Number of Surveys Collected per Coutry in a Given Year") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

#the plot shows that twice as many observations were collected 
#in Bolivia every year
#the same trend can be seen for Ecuador in 2006, 2008, and 2010
#this inequality in the number of observations can be dangerous
#for analysis because it could lead to misleading results 
#my solution is to set the numbers of observations that will be studied 
#per country and year to the minimum of all the sample 
observations_year_country <- data_subset_2 %>%
  group_by(PAIS, YEAR) %>%
  count() 

min(observations_year_country$n) #1489

observations_year_country %>%
  filter(n == 1489)
#the lowest number of surveys collected in a single country 
#in a specific year was 1489 (Ecuador, 2014)
#therefore, I will pick 1489 random observations per country and year

data_sample_equal <- data_subset_2 %>%
  group_by(PAIS, YEAR) %>%
  sample_n(1489)


#checking
data_sample_equal %>%
  group_by(PAIS, YEAR) %>%
  count()

data_sample_equal %>%
  group_by(PAIS, YEAR) %>%
  count() %>%
  ggplot(mapping = aes(x = as.character(YEAR), 
                       y = n,
                       fill = as.character(PAIS))) +
  scale_fill_discrete(name = "Country",
                      labels = c("Bolivia", "Peru", "Colombia", "Ecuador")) +
  geom_col(position = position_dodge2(), 
           width = 0.7) +
  labs(x = "Year of Survey", 
       y = "Number of Surveys", 
       title = "Number of Surveys Collected per Coutry in a Given Year", 
       subtitle = "Sample : 1489") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold"))

data_sample_equal <- data_sample_equal %>% ungroup()
#making subsets per country 
peru_sample <- data_sample_equal %>%
  filter(PAIS == 11)

colombia_sample <- data_sample_equal %>%
  filter(PAIS == 8)

ecuador_cample <- data_sample_equal %>%
  filter(PAIS == 9)

bolivia_sample <- data_sample_equal %>%
  filter(PAIS == 10)

#saving current sample dataset 
write_csv(data_sample_equal, 
          file = "Data sets made in process/sample_of_data_andean")

#CLEANING ENTRIES IN VARIABLES----

#for a lot of values, data that was supposed to be discrete was entered as numeric 
#for example, if the answer was "yes," it was recorded as 1
#even though this could be beneficial in some cases
#it is not in many
#so I will check and adjust (if needed) every variable 

#overview
glimpse(data_sample_equal)
skim(data_sample_equal) #shows that all columns are numeric, which is a problem 

data_edited <- data_sample_equal %>%
  mutate(PAIS = recode(PAIS, 
                       "8" = "Colombia", 
                       "9" = "Ecuador", 
                       "10" = "Bolivia",
                       "11" = "Peru")) %>%
  mutate(UR = recode(UR, 
                     "1" = "Urban", 
                     "2" = "Rural")) %>%
  mutate(IDIOMAQ = recode(IDIOMAQ, 
                     "1" = "Spanish", 
                     "902" = "Other", 
                     "1002" = "Other", 
                     "1003" = "Other",
                     .default = NA_character_)) %>%
  mutate(Q1 = recode(Q1, 
                     "1" = "Male", 
                     "2" = "Female")) %>%
  #character for now
  mutate(LS3 = recode(LS3, 
                      "1" = "Very satisfied", 
                      "2" = "Somewhat satisfied", 
                      "3" = "Somewhat dissatisfied", 
                      "4" = "Very dissatisfied", 
                      "5" = "Don't know", 
                      "6" = "No answer",
                      .missing = "No answer", 
                      .default = "No answer")) %>%
  mutate(A4 = recode(A4, 
                     "30" = "Armed conflict", 
                     "15" = "Bad government", 
                     "13" = "Corruption", 
                     "09" = "Credit (lack of)", 
                     "05" = "Crime", 
                     "25" = "Discrimination", 
                     "11" = "Drug addiction/consumption", 
                     "12" = "Drug trafficking", 
                     "01" = "Economy (problems with)", 
                     "21" = "Education (quality, lack of)", 
                     "24" = "Electricity (lack of)", 
                     "10" = "Environment", 
                     "26" = "External debt",
                     "32" = "Forced displacement of people", 
                     "14" = "Gangs", 
                     "22" = "Health service (lack of)", 
                     "55" = "Housing", 
                     "61" = "Human right violations", 
                     "88" = "Don't know", 
                     "58" = "Inequality", 
                     "02" = "Inflation", 
                     "31" = "Kidnappings", 
                     "07" = "Land to farm (lack of)",
                     "23" = "Malnutrition", 
                     "16" = "Migrations", 
                     "59" = "Politicians", 
                     "06" = "Popular protests", 
                     "20" = "Over population", 
                     "04" = "Poverty", 
                     "18" = "Roads in poor condition",
                     "27" = "Security (lack of)", 
                     "33" = "Terrorism", 
                     "60" = "Transpotation (problems with)", 
                     "03" = "Unemployment", 
                     "57" = "Violence", 
                     "17" = "War against terrorism", 
                     "19" = "Water (lack of)", 
                     "70" = "Other", 
                     "98" = "No answer", 
                     .missing = "No answer", 
                     .default = "No answer")) %>%
  mutate(SOCT2 = recode(SOCT2, 
                        "1" = "Better", 
                        "2" = "Same", 
                        "3" = "Worse", 
                        "88" = "Don't know", 
                        "98" = "No answer")) %>%
  mutate(IDIO2 = recode(IDIO2, 
                        "1" = "Better", 
                        "2" = "Same", 
                        "3" = "Worse", 
                        "88" = "Don't know", 
                        "98" = "No answer")) %>%
  mutate(CP5 = recode(CP5, 
                      "1" = "Once a week", 
                      "2" = "Once or twice a month", 
                      "3" = "Once or twice a year", 
                      "4" = "Never", 
                      "88" = "Don't know", 
                      "98" = "No answer", 
                      .missing = "No answer")) %>%
  #get rid of L1
  mutate(PROT3 = recode(PROT3, 
                        "1" = "Yes", 
                        "2" = "No", 
                        "88" = "Don't know", 
                        "98" = "No answer", 
                        .default = "No answer", 
                        .missing = "No answer")) %>%
  mutate(JC10 = recode(JC10, 
                       "1" = "Justified", 
                       "2" = "Not justified", 
                       "88" = "Don't know", 
                       "98" = "No answer", 
                       .default = "No answer", 
                       .missing = "No answer")) %>%
  mutate(JC13 = recode(JC13, 
                       "1" = "Justified", 
                       "2" = "Not justified", 
                       "88" = "Don't know", 
                       "98" = "No answer", 
                       .default = "No answer", 
                       .missing = "No answer")) %>%
  mutate(JC15A = recode(JC15A, 
                       "1" = "Justified", 
                       "2" = "Not justified", 
                       "88" = "Don't know", 
                       "98" = "No answer", 
                       .default = "No answer", 
                       .missing = "No answer")) %>%
  mutate(VIC1EXT = recode(VIC1EXT, 
                       "1" = "Yes", 
                       "2" = "No", 
                       "88" = "Don't know", 
                       "98" = "No answer", 
                       .default = "No answer", 
                       .missing = "No answer")) %>%
  #figure what to do with VICEXTA
  mutate(VIC1HOGAR = recode(VIC1HOGAR, 
                          "1" = "Yes", 
                          "2" = "No", 
                          "88" = "Don't know", 
                          "98" = "No answer", 
                          .default = "No answer", 
                          .missing = "No answer")) %>%
  mutate(PESE1 = recode(PESE1, 
                            "1" = "Higher", 
                            "2" = "Same", 
                            "3" = "Lower",
                            "88" = "Don't know", 
                            "98" = "No answer", 
                            .default = "No answer", 
                            .missing = "No answer")) %>%
  mutate(PESE2 = recode(PESE2, 
                        "1" = "Higher", 
                        "2" = "Same", 
                        "3" = "Lower",
                        "88" = "Don't know", 
                        "98" = "No answer", 
                        .default = "No answer", 
                        .missing = "No answer")) %>%
  mutate(AOJ12 = recode(AOJ12, 
                        "1" = "A lot", 
                        "2" = "Some", 
                        "3" = "Little",
                        "4" = "None",
                        "88" = "Don't know", 
                        "98" = "No answer", 
                        .default = "No answer", 
                        .missing = "No answer")) %>%
  #leave B#, N# as they are because it asks about extent, 
  #better numerical
  #leave M1 as numerical because it is a rating
  mutate(SD2NEW2 = recode(SD2NEW2, 
                      "1" = "Very satisfied", 
                      "2" = "Somewhat satisfied", 
                      "3" = "Dissatisfied", 
                      "4" = "Very dissatisfied", 
                      "88" = "Don't know", 
                      "99" = "Don't use",
                      "98" = "No answer",
                      .missing = "No answer", 
                      .default = "No answer")) %>%
  mutate(SD3NEW2 = recode(SD3NEW2, 
                          "1" = "Very satisfied", 
                          "2" = "Somewhat satisfied", 
                          "3" = "Dissatisfied", 
                          "4" = "Very dissatisfied", 
                          "88" = "Don't know", 
                          "99" = "Don't use",
                          "98" = "No answer",
                          .missing = "No answer", 
                          .default = "No answer")) %>%
  mutate(SD6NEW2 = recode(SD6NEW2, 
                          "1" = "Very satisfied", 
                          "2" = "Somewhat satisfied", 
                          "3" = "Dissatisfied", 
                          "4" = "Very dissatisfied", 
                          "88" = "Don't know", 
                          "99" = "Don't use",
                          "98" = "No answer",
                          .missing = "No answer", 
                          .default = "No answer")) %>%
  #leave ROS4, ING4, and MIL7 as they are because it asks about extent, 
  #better numerical
  mutate(PN4 = recode(PN4, 
                          "1" = "Very satisfied", 
                          "2" = "Somewhat satisfied", 
                          "3" = "Dissatisfied", 
                          "4" = "Very dissatisfied", 
                          "88" = "Don't know", 
                          "98" = "No answer",
                          .missing = "No answer", 
                          .default = "No answer")) %>%
  mutate(EXC2 = recode(EXC2, 
                       "0" = "No", 
                       "1" = "Yes", 
                       .missing = "No answer/no contact", 
                       .default = "No answer/no contact")) %>%
  mutate(EXC7 = recode(EXC7, 
                       "1" = "Very common", 
                       "2" = "Common", 
                       "3" = "Uncommon", 
                       "4" = "Very uncommon",
                       "88" = "Don't know", 
                       "98" = "No answer",
                       .missing = "No answer", 
                       .default = "No answer")) %>%
  #leave POL1 as numerical for now because it asks about level of interest
  #leave ED as number because a higher number is more education 
  mutate(Q10D = recode(Q10D, 
                       "1" = "Good enough and can save from it", 
                       "2" = "Good enough, no major problesm", 
                       "3" = "Not enough", 
                       "4" = "Not enough, having a hard time", 
                       "88" = "Don't know", 
                       "98" = "No answer", 
                       .missing = "No answer", 
                       .default = "No answer")) %>%
  mutate(ETID = recode(ETID, 
                       "1" = "White", 
                       "2" = "Mestizo", 
                       "3" = "Indigenous", 
                       "4" = "Black", 
                       "5" = "Mulatto", 
                       "7" = "Other",
                       "88" = "Don't know", 
                       "98" = "No answer", 
                       .missing = "No answer", 
                       .default = "Other"))

#RENAMING COLUMNS FOR BETTER UNDERSTANDING----

#Other small changes
data_edited <- data_edited %>%
  select(-c(ESTRATOSEC)) #not really relevant

#renaming
data_edited <- data_edited %>%
  rename(country = PAIS, 
         year = YEAR, 
         urban_rural = UR,
         size_place = TAMANO, 
         language_form = IDIOMAQ, 
         left_right = L1,
         sex = Q1, 
         life_satisfaction = LS3, 
         country_main_problem = A4, 
         economy_compared_12 = SOCT2, 
         personal_economy_12 = IDIO2, 
         times_solving_community_problem_12 = CP5, 
         demonstration_participation_12 = PROT3, 
         military_takeover_crime = JC10, 
         military_takeover_corruption = JC13, 
         close_congress_difficult_times = JC15A, 
         victim_crime_12 = VIC1EXT, 
         times_victim_crime_12 = VIC1EXTA, 
         household_victim_crime_12 = VIC1HOGAR, 
         violence_neighborhoods_compared = PESE1, 
         violece_neighborhood_12 = PESE2,
         trust_judicial_punishment = AOJ12, 
         respect_political_institutions = B2, 
         pride_living_political_system = B4, 
         should_suppot_political_system = B6, 
         trust_justice_system = B10A, 
         trust_armed_forces = B12, 
         trust_national_congress = B13, 
         trust_national_police = B18, 
         trust_political_parties = B21, 
         trust_president = B21A, 
         trust_elections = B47A,
         administration_combats_corruption = N9, 
         administration_imp_safety = N11, 
         administration_good_economy_mgm = N15, 
         rate_president_performance = M1, 
         satisf_road_streets_highw = SD2NEW2, 
         satisf_public_schools = SD3NEW2, 
         satisf_health_services = SD6NEW2, 
         strong_pol_inequality = ROS4, 
         democracy_better = ING4, 
         aaff_should_combate_crime = MIL7, 
         satisf_democracy_country = PN4, 
         police_bribe_12 = EXC2, 
         freq_corrup_public_off = EXC7, 
         interest_politics = POL1, 
         schooling_completed = ED, 
         salary_satisf = Q10D, 
         race_id = ETID)

#save cleaner data 

write_csv(data_edited, 
          file = "Data sets made in process/ready_to_use_49")

#LAST SUBSETTING ----

#even though I have already subset the data set to 49 variables,
#I will subset again
#because I rather do a an in-deep analysis for my EDA
#than cover many variables superficially 
#still, now that I have a clean data and the well-named columns, 
#it is easier to identify what variables I want to keep since I can see a story line 
#given that I have many variables around topics such as: crime, democracy, corruption, economy, education
#I will keep those and I will get rid of the others, these being: 

#language_form, overall irrelevant because most of the forms were in Spanish
table(data_edited$language_form)
#times_solving_community_problem_12 does not seem of too much importance for the issues I will discuss
#demonstration_participation_12 does not seem of too much importance for the issues I will discuss
#satisf_road_streets_highw would be good if there were more questions related to infrastructure
#even though 

data_final <- data_edited %>%
  select(-c(language_form, 
            times_solving_community_problem_12, 
            demonstration_participation_12,
            satisf_road_streets_highw))

#saving clean data set 
write_csv(data_final, 
          file = "Data sets made in process/final")









  
library(readr)
library(skimr)

#loading data 
raw_data <- read_tsv("Data Collected/Raw Data.tsv")

#checking the structure of the data 
str(raw_data)

#other operations to explore original data set
skim(raw_data) #236754 rows and 266 columns/variables
#2 variables are character type
#264 variables are numeric type 
glimpse(raw_data)

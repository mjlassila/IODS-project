# IODS Week 4
# Matti Lassila 13.11.2018

# Read the “Human development” and “Gender inequality” datas into R. 

# Set up

library(httr)
library(data.table)
library(dplyr)
library(skimr)
library(here)


# Set the working directory

setwd(here("/data/"))

# Load data from the internet

url_hd <- "https://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv"
GET(url_hd, write_disk("human_development.csv", overwrite = TRUE))

url_gii <- "https://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv"
GET(url_gii, write_disk("gender_inequality.csv", overwrite = TRUE))


# Prepare column names for human development dataset
raw_data_hd <- fread("human_development.csv",nrows = 2)
column_names_hd <- tolower(colnames(raw_data_hd))

# Read full human development dataset in using proper columnnames
raw_data_hd <- fread(
  "human_development.csv",
  col.names = column_names_hd,
  check.names = TRUE, na.strings = "..")

# Prepare column names for gender inequality dataset
raw_data_gii <- fread("gender_inequality.csv",nrows = 2)
column_names_gii <- tolower(colnames(raw_data_gii))

# Read full human development dataset in using proper columnnames
raw_data_gii <- fread(
  "gender_inequality.csv",
  col.names = column_names_gii,
  check.names = TRUE, na.strings = "..")


# Explore the datasets: see the structure and dimensions of the data. 
# Create summaries of the variables. (1 point)

dim(raw_data_hd)
dim(raw_data_gii)

skim(raw_data_hd)

skim(raw_data_gii)

# Look at the meta files and rename the variables with (shorter) 
# descriptive names. (1 point)

hd_names <- c(
  "hdi_rank",
  "country",
  "hdi",
  "life_exp",
  "edu_exp",
  "edu_mean",
  "gni",
  "gni_hdi"
)
colnames(raw_data_hd) <- hd_names

gii_names <- c(
  "gii_rank",
  "country",
  "gii",
  "mat_mortality",
  "birth_rate",
  "repr_parlament",
  "sec_edu_female",
  "sec_edu_male",
  "labour_rate_female",
  "labour_rate_male"
)

colnames(raw_data_gii) <- gii_names


# Mutate the “Gender inequality” data and create two new variables. 
# The first one should be the ratio of Female and Male populations 
# with secondary education in each country. (i.e. edu2F / edu2M). 
# The second new variable should be the ratio of 
# labour force participation of females and males in each country 
# (i.e. labF / labM). (1 point)

# Join together the two datasets using the variable 
# Country as the identifier. Keep only the countries in both data sets 
# (Hint: inner join). 

human <- raw_data_gii %>% 
  mutate(
    female_male_edu_ratio = sec_edu_female / sec_edu_male,
    female_male_labour_ratio = labour_rate_female / labour_rate_male
  ) %>%
  inner_join(raw_data_hd, by = "country")





# The joined data should have 195 observations and 19 variables. 

dim(human)

# We are clear

# [1] 195 19

# Call the new joined data "human" and save it in your data folder. (1 point)

write.csv(human,"human.csv",row.names = FALSE)

# Housekeeping

gii_file <- "gender_inequality.csv"
hd_file <- "human_development.csv"

if (file.exists(gii_file)) file.remove(gii_file)
if (file.exists(hd_file)) file.remove(hd_file)

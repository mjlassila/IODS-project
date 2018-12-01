# IODS Week 4
# Matti Lassila 13.11.2018

# Read the “Human development” and “Gender inequality” datas into R. 

# Set up

library(httr)
library(data.table)
library(dplyr)
library(skimr)
library(here)
library(magrittr)
# For overriding MASS select() 
select <- dplyr::select

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

# =============================
# Data wrangling for week 5


# Load the ‘human’ data into R. Explore the structure and the dimensions of the
# data and describe the dataset briefly, assuming the reader has no previous
# knowledge of it (this is now close to the reality, since you have named the
# variables yourself). (0-1 point)

# set working directory
setwd(here("/data/"))
human <- fread("human.csv")


# Check check

skim(human)
# Skim summary statistics
# n obs: 195 
# n variables: 19 
# 
# ── Variable type:character ───────────────────────────────────────────────────────────
# variable missing complete   n min max empty n_unique
# country       0      195 195   4  41     0      195
# gni       0      195 195   3   7     0      194
# 
# ── Variable type:integer ─────────────────────────────────────────────────────────────
# variable missing complete   n   mean     sd  p0   p25  p50    p75 p100     hist
# gii_rank       7      188 195  94.31  54.43   1 47.75 94   141.25  188 ▇▇▇▇▇▇▇▇
# gni_hdi       7      188 195   0.19  17.33 -84 -9     1.5  11      47 ▁▁▁▂▃▇▂▁
# hdi_rank       7      188 195  94.31  54.43   1 47.75 94   141.25  188 ▇▇▇▇▇▇▇▇
# mat_mortality      10      185 195 163.23 213.23   1 16    69   230    1100 ▇▂▁▁▁▁▁▁
# 
# ── Variable type:numeric ─────────────────────────────────────────────────────────────
# variable missing complete   n  mean    sd     p0   p25   p50   p75
# birth_rate       5      190 195 49.55 40.74  0.6   15.45 40.95 71.78
# edu_exp       0      195 195 12.86  2.83  4.1   11.1  13.1  14.9 
# edu_mean       0      195 195  8.08  3.06  1.4    5.55  8.4  10.6 
# female_male_edu_ratio      26      169 195  0.85  0.24  0.17   0.73  0.93  1   
# female_male_labour_ratio      11      184 195  0.7   0.2   0.19   0.6   0.75  0.85
# gii      33      162 195  0.37  0.19  0.016  0.2   0.39  0.53
# hdi       0      195 195  0.69  0.15  0.35   0.58  0.72  0.8 
# labour_rate_female      11      184 195 52.61 16.23 13.5   44.5  53.3  62.62
# labour_rate_male      11      184 195 74.74  8.47 44.2   68.88 75.55 80.15
# life_exp       0      195 195 71.07  8.29 49     65.75 73.1  76.8 
# repr_parlament       3      192 195 20.6  11.41  0     12.47 19.5  27.02
# sec_edu_female      26      169 195 54.8  30.08  0.9   27.8  55.7  81.8 
# sec_edu_male      26      169 195 60.29 27.39  3.2   38.3  60    85.8 
# p100     hist
# 204.8  ▇▆▃▂▂▁▁▁
# 20.2  ▁▁▃▅▇▆▂▁
# 13.1  ▂▃▅▅▇▆▇▆
# 1.5  ▁▂▂▂▇▃▁▁
# 1.04 ▁▂▁▃▅▇▇▂
# 0.74 ▅▆▅▆▇▇▅▂
# 0.94 ▂▃▃▃▅▇▃▅
# 88.1  ▁▂▂▆▇▅▂▂
# 95.5  ▁▁▂▅▆▇▂▂
# 84    ▁▁▂▃▃▇▅▅
# 57.5  ▃▆▇▆▂▃▁▁
# 100    ▅▅▅▃▆▅▅▇
# 100    ▂▃▅▅▅▃▅▇




# Mutate the data: transform the Gross National Income (GNI) variable to numeric
# (Using string manipulation. Note that the mutation of 'human' was not done on
# DataCamp). (1 point)

human %<>% mutate(gni = as.numeric(stringr::str_replace(gni,',','')))

# Exclude unneeded variables: keep only the columns matching the following
# variable names (described in the meta file above):  "Country", "Edu2.FM",
# "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F" (1
# point)

# Remove all rows with missing values (1 point).

human %<>%  select(
  country,
  edu_exp,
  sec_edu_female,
  labour_rate_female,
  edu_exp,
  life_exp,
  gni,
  mat_mortality,
  birth_rate,
  repr_parlament) %>%
  drop_na


# Remove the observations which relate to regions instead of countries. (1
# point)

human$country

# Based on visual inspection of the data
# it seems that region rows are the very last rows
# of the dataset

rownames(human) <- 1:dim(human)[1]

# Checking the data using View() we can see that the last row to
# be included is row no 155.
human %<>% slice(1:155)

# Define the row names of the data by the country names and remove the country
# name column from the data. The data should now have 155 observations and 8
# variables. Save the human data in your data folder including the row names.
# You can overwrite your old ‘human’ data. (1 point)

countries <- human$country
rownames(human) <- countries
human %<>% select(-country)

dim(human)
# We are clear
# [1] 155   8

write.csv(human,"human.csv",row.names = TRUE)

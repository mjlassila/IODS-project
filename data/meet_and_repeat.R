# IODS Week 6
# Matti Lassila 3.12.2018


# Set up

library(httr)
library(data.table)
library(dplyr)
library(skimr)
library(here)
library(magrittr)
library(tidyr)
# For overriding MASS select() 
select <- dplyr::select

# Set the working directory

setwd(here("/data/"))



# 1. Load the data sets (BPRS and RATS) into R using as the source the GitHub
# repository of MABS, where they are given in the wide form:

# Load data from the internet
url_bprs <- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt"
GET(url_bprs, write_disk("bprs.txt", overwrite = TRUE))

url_rats <- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt"
GET(url_rats, write_disk("rats.txt", overwrite = TRUE))

# Also, take a look at the data sets: check their variable names, view the data
# contents and structures, and create some brief summaries of the variables, so
# that you understand the point of the wide form data. (1 point)

raw_data_rats <- fread("rats.txt")
raw_data_bprs <- fread("bprs.txt")

skim(raw_data_rats)

raw_data_rats %<>% select(-V1)

colnames_rats <- tolower(names(raw_data_rats))

colnames(raw_data_rats) <- colnames_rats

# It seems that the dataset is the same as week 5
# so we can reuse the column definitions from the previous week



skim(raw_data_rats)

skim(raw_data_bprs)
# 2. Convert the categorical variables of both data sets to factors. (1 point)


raw_data_bprs %<>% mutate(
  subject = as.factor(subject),
  treatment = as.factor(treatment)
  )

raw_data_rats %<>% mutate(
  group = as.factor(group),
  id = as.factor(id)
)

# 3. Convert the data sets to long form. Add a week variable to BPRS and a Time
# variable to RATS. (1 point)

bprs <- raw_data_bprs %>% 
  gather(key = weeks, value = bprs, -treatment, -subject) %>%
  mutate(week = as.integer(substr(weeks, 5, 5)))

rats <- raw_data_rats %>% 
  gather(key = weeks, value = rats, -group, -id) %>%
  mutate(time = as.integer(substr(weeks, 3, 4)))
  


# 4. Now, take a serious look at the new data sets and compare them with their
# wide form versions: check the variable names, view the data contents and
# structures, and create some brief summaries of the variables. Make sure that
# you understand the point of the long form data and the crucial difference
# between the wide and the long forms before proceeding the to Analysis
# exercise. (2 points)

skim(bprs)

# Skim summary statistics
# n obs: 360 
# n variables: 4 
# 
# ── Variable type:character ───────────────────────────────────────────────────
# variable missing complete   n min max empty n_unique
# week       0      360 360   5   5     0        9
# 
# ── Variable type:factor ──────────────────────────────────────────────────────
# variable missing complete   n n_unique                 top_counts ordered
# subject       0      360 360       20 1: 18, 2: 18, 3: 18, 4: 18   FALSE
# treatment       0      360 360        2      1: 180, 2: 180, NA: 0   FALSE
# 
# ── Variable type:integer ─────────────────────────────────────────────────────
# variable missing complete   n  mean    sd p0 p25 p50 p75 p100     hist
# value       0      360 360 37.66 13.66 18  27  35  43   95 ▆▇▅▃▂▁▁▁

skim(rats)

# Skim summary statistics
# n obs: 144 
# n variables: 6 
# 
# ── Variable type:character ───────────────────────────────────────────────────
# variable missing complete   n min max empty n_unique
# wd       0      144 144   3   4     0        9
# 
# ── Variable type:factor ──────────────────────────────────────────────────────
# variable missing complete   n n_unique                 top_counts ordered
# group       0      144 144        3 1: 72, 2: 36, 3: 36, NA: 0   FALSE
# id       0      144 144       16     1: 9, 2: 9, 3: 9, 4: 9   FALSE
# 
# ── Variable type:integer ─────────────────────────────────────────────────────
# variable missing complete   n   mean     sd  p0    p25   p50    p75 p100
# value       0      144 144 380.74 125.37 225 265    344.5 507.75  612
# wd57       0      144 144 398.62 132.4  247 273.75 373.5 524.5   618
# wd64       0      144 144 404.06 135.6  245 278    378   530.75  628
# hist
# ▇▂▁▁▃▂▃▁
# ▇▁▁▁▂▂▃▁
# ▇▁▁▁▂▂▃▁

# Save the data

write.csv(rats,"rats.csv",row.names = FALSE)
write.csv(rats,"bprs.csv",row.names = FALSE)

# Housekeeping

rats_raw <- "rats.txt"
bprs_raw <- "bprs.txt"

if (file.exists(rats_raw)) file.remove(rats_raw)
if (file.exists(bprs_raw)) file.remove(bprs_raw)

# IODS Week 3
# Load and prepare the dataset for analysis
# Matti Lassila, 9.10.2018

# Set up

library(httr)
library(data.table)
library(dplyr)
library(skimr)
library(here)


# Set the working directory

setwd(here("/data/"))

# Load data from the internet

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
GET(url, write_disk("student.zip", overwrite = TRUE))
unzip("student.zip", overwrite = TRUE)

# Remove junk
fn <- "student-merge.R"
if (file.exists(fn)) file.remove(fn)

# Read both student-mat.csv and student-por.csv into R (from the data folder) 
# and explore the structure and dimensions of the data. (1 point)

# Prepare column names
student_mat <- fread("student-mat.csv",nrows = 2)
student_mat_colnames <- tolower(colnames(student_mat))


student_por <- fread("student-por.csv",nrows = 2)
student_por_colnames <- tolower(colnames(student_por))

# Read full data with proper columnnames
student_mat <- fread(
  "student-mat.csv",
  col.names = student_mat_colnames,
  check.names = TRUE
)

student_por <- fread(
  "student-por.csv",
  col.names = student_por_colnames,
  check.names = TRUE
)

# Get row and column count and check variable distributions

dim(student_mat)
dim(student_por)

skim(student_mat)
skim(student_por)


# Join the two data sets using the variables 
# "school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", 
# "Mjob", "Fjob", "reason", "nursery","internet" as (student) identifiers.
# Keep only the students present in both data sets. 
# Explore the structure and dimensions of the joined data. (1 point)

join_by <- c(
  "school",
  "sex",
  "age",
  "address",
  "famsize",
  "pstatus",
  "medu",
  "fedu",
  "mjob",
  "fjob",
  "reason",
  "nursery",
  "internet"
)


students <- student_mat %>% 
  inner_join(
    student_por,
    by = join_by
)

alc <- select(students, one_of(join_by))

notjoined_columns <- colnames(student_mat)[!colnames(student_mat) %in% join_by]

for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(students, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# Take the average of the answers related to weekday and weekend alcohol 
# consumption to create a new column 'alc_use' to the joined data. 
# Then use 'alc_use' to create a new logical column 'high_use' which is TRUE 
# for students for which 'alc_use' is greater than 2 (and FALSE otherwise).


# From the dataset descroption

# # Attributes for both student-mat.csv (Math course) and student-por.csv (Portuguese language course) datasets: 
# 1 school - student's school (binary: 'GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira) 
# 2 sex - student's sex (binary: 'F' - female or 'M' - male) 
# 3 age - student's age (numeric: from 15 to 22) 
# 4 address - student's home address type (binary: 'U' - urban or 'R' - rural) 
# 5 famsize - family size (binary: 'LE3' - less or equal to 3 or 'GT3' - greater than 3) 
# 6 Pstatus - parent's cohabitation status (binary: 'T' - living together or 'A' - apart) 
# 7 Medu - mother's education (numeric: 0 - none, 1 - primary education (4th grade), 2 â€“ 5th to 9th grade, 3 â€“ secondary education or 4 â€“ higher education) 
# 8 Fedu - father's education (numeric: 0 - none, 1 - primary education (4th grade), 2 â€“ 5th to 9th grade, 3 â€“ secondary education or 4 â€“ higher education) 
# 9 Mjob - mother's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other') 
# 10 Fjob - father's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other') 
# 11 reason - reason to choose this school (nominal: close to 'home', school 'reputation', 'course' preference or 'other') 
# 12 guardian - student's guardian (nominal: 'mother', 'father' or 'other') 
# 13 traveltime - home to school travel time (numeric: 1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour) 
# 14 studytime - weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours) 
# 15 failures - number of past class failures (numeric: n if 1<=n<3, else 4) 
# 16 schoolsup - extra educational support (binary: yes or no) 
# 17 famsup - family educational support (binary: yes or no) 
# 18 paid - extra paid classes within the course subject (Math or Portuguese) (binary: yes or no) 
# 19 activities - extra-curricular activities (binary: yes or no) 
# 20 nursery - attended nursery school (binary: yes or no) 
# 21 higher - wants to take higher education (binary: yes or no) 
# 22 internet - Internet access at home (binary: yes or no) 
# 23 romantic - with a romantic relationship (binary: yes or no) 
# 24 famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent) 
# 25 freetime - free time after school (numeric: from 1 - very low to 5 - very high) 
# 26 goout - going out with friends (numeric: from 1 - very low to 5 - very high) 
# 27 Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high) 
# 28 Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high) 
# 29 health - current health status (numeric: from 1 - very bad to 5 - very good) 
# 30 absences - number of school absences (numeric: from 0 to 93) 
# 
# # these grades are related with the course subject, Math or Portuguese: 
# 31 G1 - first period grade (numeric: from 0 to 20) 
# 31 G2 - second period grade (numeric: from 0 to 20) 
# 32 G3 - final grade (numeric: from 0 to 20, output target)

alc$id <- 1:dim(alc)[1]
alc$high_use <- FALSE

alc <- 
  alc %>% 
  group_by(id) %>%
  mutate(alc_use = mean(c(dalc, walc))) %>%
  mutate(high_use = alc_use > 2) %>%
  ungroup %>%
  select(-id)



# Glimpse at the joined and modified data to make sure everything is in order.
# The joined data should now have 382 observations of 35 variables. 
# Save the joined and modified data set to the ‘data’ folder

glimpse(alc)
write.csv(alc,"chapter3_alc.csv",row.names = FALSE)



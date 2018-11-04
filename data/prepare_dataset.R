# IODS Week 2
# Load and prepare the dataset for analysis
# Matti Lassila, 4.10.2018

# Set up

library(httr)
library(data.table)
library(dplyr)
library(skimr)
library(here)


# Set the working directory

setwd(here("/data/"))

# Load data from the internet

url <- "http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt"
GET(url, write_disk("JYTOPKYS3-data.txt", overwrite = TRUE))

# Prepare column names
raw_data <- fread("JYTOPKYS3-data.txt",nrows = 2)
column_names <- tolower(colnames(raw_data))

# Read full dataset in using proper columnnames
raw_data <- fread(
  "JYTOPKYS3-data.txt",
  col.names = column_names,
  check.names = TRUE)



# Get row and column count

dim(raw_data)


# Get overview of all variables

skim(raw_data)

# Skim summary statistics
# n obs: 183 
# n variables: 60 
# 
# ── Variable type:character ───────────────────────────────────────────────────────────────────
# variable missing complete   n min max empty n_unique
# gender       0      183 183   1   1     0        2
# 
# ── Variable type:integer ─────────────────────────────────────────────────────────────────────
# variable missing complete   n  mean   sd p0  p25 p50  p75 p100     hist
# aa       0      183 183  3.06 0.98  1  2     3  4      5 ▁▆▁▇▁▇▁▁
# ab       0      183 183  1.79 1.03  1  1     2  2      5 ▇▆▁▁▁▁▁▁
# ac       0      183 183  2.08 1     1  1     2  2      5 ▅▇▁▂▁▁▁▁
# ad       0      183 183  1.62 1.1   1  1     1  2      5 ▇▂▁▁▁▁▁▁
# ae       0      183 183  1.44 1.08  1  1     1  1      5 ▇▁▁▁▁▁▁▁
# af       0      183 183  1.58 1.08  1  1     1  2      5 ▇▂▁▁▁▁▁▁
# age       0      183 183 25.58 7.68 17 21    22 27     55 ▇▇▂▁▁▁▁▁
# attitude       0      183 183 31.21 7.34 14 26    32 37     50 ▁▃▅▇▆▆▂▁
# ca       0      183 183  3.01 1.22  1  2     3  4      5 ▃▇▁▇▁▆▁▃
# cb       0      183 183  4.37 0.88  1  4     5  5      5 ▁▁▁▁▁▆▁▇
# cc       0      183 183  3.69 0.84  1  3     4  4      5 ▁▁▁▅▁▇▁▂
# cd       0      183 183  3.59 1.04  1  3     4  4      5 ▁▃▁▆▁▇▁▅
# ce       0      183 183  3.99 1.02  1  3.5   4  5      5 ▁▂▁▃▁▇▁▇
# cf       0      183 183  2.98 0.98  1  2     3  4      5 ▁▆▁▇▁▆▁▁
# cg       0      183 183  3.85 0.99  1  3     4  5      5 ▁▁▁▅▁▇▁▆
# ch       0      183 183  3.67 0.99  1  3     4  4      5 ▁▂▁▅▁▇▁▃
# d03       0      183 183  4.36 0.69  1  4     4  5      5 ▁▁▁▁▁▇▁▇
# d06       0      183 183  3.45 0.95  1  3     4  4      5 ▁▃▁▆▁▇▁▂
# d07       0      183 183  3.94 0.93  1  4     4  5      5 ▁▂▁▂▁▇▁▅
# d11       0      183 183  3.93 0.84  2  4     4  4      5 ▁▁▂▁▁▇▁▃
# d14       0      183 183  4.09 0.75  2  4     4  5      5 ▁▁▁▁▁▇▁▃
# d15       0      183 183  3.22 0.99  1  2     3  4      5 ▁▆▁▅▁▇▁▂
# d19       0      183 183  3.71 0.92  1  3     4  4      5 ▁▂▁▃▁▇▁▂
# d22       0      183 183  3.34 1.07  1  3     3  4      5 ▁▅▁▇▁▇▁▃
# d23       0      183 183  3.14 1.01  1  2     3  4      5 ▁▇▁▅▁▇▁▂
# d27       0      183 183  3.45 0.97  1  3     4  4      5 ▁▃▁▆▁▇▁▂
# d30       0      183 183  3.69 1.04  1  3     4  4      5 ▁▂▁▅▁▇▁▅
# d31       0      183 183  4.02 0.94  1  4     4  5      5 ▁▁▁▂▁▇▁▅
# da       0      183 183  2.57 1.04  1  2     2  3      5 ▃▇▁▅▁▅▁▁
# db       0      183 183  3.87 0.9   1  4     4  4      5 ▁▁▁▂▁▇▁▃
# dc       0      183 183  3.71 1.09  1  3     4  4      5 ▁▂▁▃▁▇▁▅
# dd       0      183 183  3.3  1.34  1  2     4  4      5 ▃▃▁▅▁▇▁▅
# de       0      183 183  3.48 1.14  1  3     4  4      5 ▁▃▁▃▁▇▁▃
# df       0      183 183  1.85 1.06  1  1     2  2      5 ▇▅▁▂▁▂▁▁
# dg       0      183 183  2.93 1.1   1  2     3  4      5 ▂▅▁▇▁▆▁▂
# dh       0      183 183  2.91 1.44  1  2     3  4      5 ▆▇▁▅▁▅▁▆
# di       0      183 183  2.64 1.29  1  2     2  4      5 ▇▇▁▅▁▇▁▂
# dj       0      183 183  3.95 1     1  3     4  5      5 ▁▂▁▃▁▇▁▆
# points       0      183 183 20.61 8.67  0 18    22 26     33 ▃▁▂▁▆▇▆▅
# st01       0      183 183  3.77 0.9   1  3     4  4      5 ▁▂▁▂▁▇▁▃
# st04       0      183 183  3.21 1.11  1  2     3  4      5 ▁▆▁▅▁▇▁▂
# st09       0      183 183  3.22 1.09  1  2     4  4      5 ▁▅▁▃▁▇▁▂
# st12       0      183 183  2.92 1.18  1  2     3  4      5 ▃▇▁▆▁▇▁▂
# st17       0      183 183  3.18 0.96  1  3     3  4      5 ▁▅▁▇▁▆▁▂
# st20       0      183 183  2.56 1.27  1  2     2  4      5 ▆▇▁▃▁▅▁▂
# st25       0      183 183  3.34 1.45  1  2     4  4      5 ▅▂▁▂▁▇▁▆
# st28       0      183 183  2.48 1.27  1  1     2  4      5 ▆▇▁▃▁▃▁▂
# su02       0      183 183  2.61 1.26  1  2     2  4      5 ▅▇▁▃▁▅▁▂
# su05       0      183 183  2.87 1.02  1  2     3  4      5 ▁▇▁▅▁▆▁▁
# su08       0      183 183  2.51 1.07  1  2     2  3.5    5 ▂▇▁▂▁▃▁▁
# su10       0      183 183  1.99 1.05  1  1     2  2      5 ▇▇▁▂▁▂▁▁
# su13       0      183 183  2.55 1.03  1  2     2  3      5 ▂▇▁▅▁▃▁▁
# su16       0      183 183  2.81 1.08  1  2     3  4      5 ▂▇▁▆▁▆▁▁
# su18       0      183 183  1.9  1.13  1  1     1  2      5 ▇▅▁▂▁▂▁▁
# su21       0      183 183  2.55 1.13  1  2     2  4      5 ▃▇▁▃▁▅▁▁
# su24       0      183 183  3.05 1.1   1  2     3  4      5 ▁▇▁▃▁▇▁▂
# su26       0      183 183  4.13 1.03  1  4     4  5      5 ▁▂▁▂▁▇▁▇
# su29       0      183 183  2.47 1.01  1  2     2  3      5 ▃▇▁▆▁▂▁▁
# su32       0      183 183  4.06 0.94  1  4     4  5      5 ▁▁▁▂▁▇▁▆


# Create analysis dataset

# Age      Age (in years) derived from the date of birth
# Gender   Male = 1  Female = 2
# d_sm     Seeking Meaning           ~D03+D11+D19+D27
# d_ri     Relating Ideas            ~D07+D14+D22+D30
# d_ue     Use of Evidence           ~D06+D15+D23+D31
# su_lp    Lack of Purpose           ~SU02+SU10+SU18+SU26
# su_um    Unrelated Memorising      ~SU05+SU13+SU21+SU29
# su_sb    Syllabus-boundness        ~SU08+SU16+SU24+SU32
# st_os    Organized Studying        ~ST01+ST09+ST17+ST25
# st_tm    Time Management           ~ST04+ST12+ST20+ST28
# Deep     Deep approach             ~d_sm+d_ri+d_ue
# Surf     Surface approach          ~su_lp+su_um+su_sb
# Stra     Strategic approach        ~st_os+st_tm
# Attitude Global attitude toward statistics ~Da+Db+Dc+Dd+De+Df+Dg+Dh+Di+Dj

learning2014 <- raw_data %>%
  transmute(
    gender = gender,
    age = age,
    attitude = (da + db + dc + dd + de + df + dg + dh + di + dj) / 10,
    deep = ((d03 + d11 + d19 + d27) + (d07 + d14 + d22 + d30) + (d06 + d15 + d23 + d31)) / 12,
    stra = ((st01 + st09 + st17 + st25) + (st04 + st12 + st20 + st28)) / 8,
    surf = ((su02 + su10 + su18 + su26) + (su05 + su13 + su21 + su29) + (su08 + su16 + su24 + su32)) / 12,
    points = points,
    ) %>%
  filter(points > 0)


write.csv(learning2014,"learning2014.csv",row.names = FALSE)

# Let's read the data back in can check if the the dataset is identical
# compared to the data we just wrote to the disk.

learning_analysis_data <- fread("learning2014.csv")

head(learning_analysis_data)
tail(learning_analysis_data)


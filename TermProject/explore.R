# =============== FILE INFO ================ #
# :: ECS 189G: Term Project
# :: Authored by Arjun Ashok, HW Group 7
#
# :: This file is an exploration into the `drug.consumption` dataset in FairML by Scutari

source("utility.R")


# =============== DATA FORMAT INFO =============== #
# load data
load("drug.consumption.rda")
dc <- drug.consumption

# metrics & high-level exploration
print("=========== DIMENSIONS ===========")
print(dim(dc))
print("=========== SUMMARY ===========")
print(summary(dc))

# features exploration
print("=========== EXPLORE DATA ===========")
print(head(dc))
print("=========== FEATURES ===========")
print(names(dc))
feature_types <- sapply(dc, class)

# entries exploration
unique_values <- lapply(dc, unique)         # apply unique to each column
unique_lengths <- lengths(unique_values)    # find number of unique entries per column
na_entries <- colSums(is.na(dc))            # for each column, sum NA entries

print("=========== UNIQUE VALUES ===========")
print(unique_values)
print("=========== UNIQUE LENGTHS ===========")
print(unique_lengths)
print("=========== NA ENTRIES ===========")
print(na_entries)


# =============== NOTED RESULTS =============== #
# FEATURES:
#  - 1885 rows, 31 features => fine with the square root rule, mostly comes from number of drugs considered

# SUMMARY:
#  ::: categorical ::
#  - Age:         more data from younger age groups, steep dropoff towards 55+
#  - Gender:      even split of gender data
#  - Education:   more data from groups that have gone (or attempted) beyond high-school education, less from high school diploma and below
#  - Country:     high concentration of data from USA & UK, low from others
#  - Race:        mostly white participants by far
#  ::: numeric :::
#  - Nscore:      [12, 29, 36, 42, 60],   mean: 35.92
#  - Escore:      [16, 35, 40, 44, 59],   mean: 39.58
#  - Oscore:      [24, 41, 46, 51, 60],   mean: 45.76
#  - Ascore:      [12, 39, 43, 48, 60],   mean: 42.87
#  - Cscore:      [17, 37, 42, 46, 59],   mean: 41.44
#  - Impulsive:   [1, 3, 4, 6, 10],       mean: 4.801
#  - SS:          [0, 4, 6, 8, 10],       mean: 5.561
# ::: drugs :::
# - Most drugs tend to have a high bias towards never used, but caffeine is the outlier
#    - This is in line with likely what the population does
# - This is the predicted variable, drug usage with respect to each drug:
#    - Never Used         => 0
#    - Over a decade      => 1 / (average_age - 10) => since most drug users will start ~ 11+ 
#    - Last decade        => 1 / 10
#    - Last year          => 1 / 1
#    - Last month         => 12 * 1
#    - Last week          => 52 * 1
#    - Last day           => 365.25 * 1
#    - The weighting is based on the time, proportional to its recency to account for non-linear
#      risk as the recency grows closer. This is centered around 1 year as a base time frame for
#      scaling the recency (as in last year = 1, last month is 12 * risk for a year, etc.)

# REST OF EXPLORATION:
#  - Mostly factor variables except the personality scores (drugs always have 7 levels, rest vary)
#  - No missing entries, no need to account for imputing data etc.


# =============== CORRELATIONS =============== #
# define features types
sensitive_cols <- c("Age", "Gender", "Race")
sensitive_feats <- dc[, sensitive_cols]

potential_proxy_cols <- strsplit("Education,Country,Nscore,Escore,Oscore,Ascore,Cscore,Impulsive,SS", ",")[[1]]
potential_proxy_feats <- dc[, potential_proxy_cols]

predictive_vars_cols <- setdiff(names(dc), c(sensitive_cols, potential_proxy_cols))
predictive_vars_feats <- dc[, predictive_vars_cols]

# convert numeric factors to proportional numeric data
age_weighting <- c(21, 29.5, 39.5, 49.5, 59.5, 75)                          # average ages (approx)
age_frequency <- as.vector(table(factor(dc$Age)))                           # frequency
mean_age <- sum(age_frequency * age_weighting) / nrow(dc)                   # average

timeline_weighting <- c(0, 1 / (mean_age - 10), 1 / 10, 1, 12, 52, 365.25)
prediction_levels <- levels(factor(predictive_vars_feats[, 1]))

match_weights <- data.frame(level = prediction_levels, weight = timeline_weighting)
dc <- conv_numeric_factors(data=dc, cols=predictive_vars_cols, weighting=match_weights)

# find correlations from every potential proxy to every sensitive feature
# numerical correlations

# categorical correlations


# measure utility gained from each sensitive var


# measure utility gained from each potential proxy


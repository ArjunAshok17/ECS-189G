# =============== FILE INFO ================ #
# :: ECS 189G: Term Project
# :: Authored by Arjun Ashok, HW Group 7
#
# :: This file is an exploration into the `drug.consumption` dataset in FairML by Scutari

source("utility.R")
library(corrplot)
library(kendall)


# =============== DATA FORMAT INFO =============== #
sink("data_exploration.txt")

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

sink()


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


# =============== DATA ENGINEERING =============== #
sink("data_engineeering.txt")

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

# remove useless columns
remove_cols <- c("Race", "Country")
keep_cols <- setdiff(names(dc), remove_cols)
predictive_vars_cols <- setdiff(predictive_vars_cols, remove_cols)
potential_proxy_cols <- setdiff(potential_proxy_cols, remove_cols)
sensitive_cols <- setdiff(sensitive_cols, remove_cols)

dc <- dc[, keep_cols]
print(head(dc))

# one-hot encoding + numerical conversions for features & proxies
numeric_cols <- strsplit("Nscore,Escore,Oscore,Ascore,Cscore,Impulsive,SS", ",")[[1]]
categ_cols <- setdiff(setdiff(names(dc), numeric_cols), predictive_vars_cols)
dc <- one_hot_encode(data=dc, cols=categ_cols)

print(names(dc))
print(head(dc))
sink()


# =============== CORRELATIONS =============== #
sink("data_correlations.txt")

print("=========== SENSITIVE CORRELATIONS ===========")
# find all correlations
# cor_matrix <- cor_matrix(data=dc, sens_cols=sensitive_cols, numeric_cols=numeric_cols)
categ_cols <- setdiff(setdiff(names(dc), numeric_cols), predictive_vars_cols)
num_cor_matrix <- cor(dc[, numeric_cols])
cat_cor_matrix <- cor(dc[, categ_cols], method = "kendall")

print("=========== CORRELATION MATRIX ===========")
print.table(num_cor_matrix)
print.table(cat_cor_matrix)
write.csv(num_cor_matrix, file = "num_correlation_matrix.csv", row.names = TRUE)
write.csv(cat_cor_matrix, file = "cat_correlation_matrix.csv", row.names = TRUE)

# save graphics
par(mfrow = c(1, 2))
png("num_correlation_plot.png", width = 2500, height = 2500, res = 300)
colnames(num_cor_matrix) <- substr(names(num_cor_matrix), 1, 8)
row.names(num_cor_matrix) <- colnames(num_cor_matrix)
corrplot(num_cor_matrix, method = "color", tl.col = "Black", col.lab = "Black")

png("cat_correlation_plot.png", width = 2500, height = 2500, res = 300)
colnames(cat_cor_matrix) <- substr(colnames(cat_cor_matrix), 1, 8)
row.names(cat_cor_matrix) <- colnames(cat_cor_matrix)
corrplot(cat_cor_matrix, method = "color", tl.col = "Black", col.lab = "Black")
dev.off()

sink()


# =============== TRADEOFFS =============== #
##### not working #####
sink("tradeoffs.txt")
print("=========== Fairness-Utility Tradeoffs ===========")

# measure utility gained from each sensitive var
utility <- data.frame( matrix(nrow=0, ncol=length(sensitive_cols)) )
names(utility) <- sensitive_cols

for (y in predictive_vars_cols)
{
    # setup storage
    utility_row <- data.frame()

    # add each tradeoff
    for (s in sensitive_cols)
    {
        # conversion
        s_levels <- levels(factor(sensitive_feats[[s]]))

        # get ratio of fairness / utility
        u <- fair_util_tradeoff(data=dc, yName=y, sName=s_levels)
        fu_ratio <- (u[["Y Accuracy w/ S"]] - u[["Y Accuracy"]]) / u[["S Accuracy"]]          # store a ratio which loses on feature selection, but still good
        utility_row <- cbind(utility_row, fu_ratio)
    }

    # add entry
    names(utility_row) <- sensitive_cols
    utility <- rbind(utility, utility_row)
}
row.names(utility) <- predictive_vars_cols

print.table(utility)
sink()

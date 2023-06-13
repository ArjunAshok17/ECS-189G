# =============== FILE INFO ================ #
# :: ECS 189G: Term Project
# :: Authored by Arjun Ashok, HW Group 7
#
# :: This file is an alternative analysis into the `drug.consumption` dataset in FairML by Scutari
#    that attempts to create a better balance of fairness and utility. Given the context and potential
#    scenarios that arise from this kind of issue, we favored fairness as a metric for how performative
#    our model is.


# =============== MODEL TRAINING SETUP ================ #
# setup model training #
print("setting up model training...")
library(qeML)
source("utility.R")
source("metrics.R")
load("drug.consumption.rda")
dc <- drug.consumption

# engineer data #
print("engineering data...")

# define features types
print("    defining features...")
sensitive_cols <- c("Age", "Gender", "Race")
sensitive_feats <- dc[, sensitive_cols]

potential_proxy_cols <- strsplit("Education,Country,Nscore,Escore,Oscore,Ascore,Cscore,Impulsive,SS", ",")[[1]]
potential_proxy_feats <- dc[, potential_proxy_cols]

predictive_vars_cols <- setdiff(names(dc), c(sensitive_cols, potential_proxy_cols))
predictive_vars_feats <- dc[, predictive_vars_cols]

# convert drug consumption to proportional numeric data
print("    converting to numerical data...")
age_weighting <- c(21, 29.5, 39.5, 49.5, 59.5, 75)                          # average ages (approx)
age_frequency <- as.vector(table(factor(dc$Age)))                           # frequency
mean_age <- sum(age_frequency * age_weighting) / nrow(dc)                   # average

timeline_weighting <- c(0, 1 / (mean_age - 10), 1 / 10, 1, 12, 52, 365.25)
prediction_levels <- levels(factor(predictive_vars_feats[, 1]))

match_weights <- data.frame(level = prediction_levels, weight = timeline_weighting)
dc <- conv_numeric_factors(data=dc, cols=predictive_vars_cols, weighting=match_weights)

# remove useless columns
print("    removing unnecessary columns...")
remove_cols <- c("Race", "Country")
keep_cols <- setdiff(names(dc), remove_cols)
predictive_vars_cols <- setdiff(predictive_vars_cols, remove_cols)
potential_proxy_cols <- setdiff(potential_proxy_cols, remove_cols)

dc <- dc[, keep_cols]

# one-hot encoding + numerical conversions for features & proxies
print("    one-hot encoding for cateogrical features...")
numeric_cols <- strsplit("Nscore,Escore,Oscore,Ascore,Cscore,Impulsive,SS", ",")[[1]]
categ_cols <- setdiff(setdiff(names(dc), numeric_cols), predictive_vars_cols)
dc <- one_hot_encode(data=dc, cols=categ_cols)

print("finished data engineering!")


#######################################################
# =============== ITERATIVE BUILDING ================ #
#######################################################
print("iteratively building model...")

# variable handling #
deweight_cols <- c("Age", "Gender", "Education")
sens_levels <- get_column_levels(sensitive_feats)

num_sens <- length(deweight_cols)
deweighting_cols_lvls <- vector(length=0)
deweighting_cols <- vector(length=0)
for (d in deweight_cols)
{
    deweighting_cols <- c(deweighting_cols, levels(factor(drug.consumption[[d]])))
    deweighting_cols_lvls <- c(deweighting_cols_lvls, length(levels(factor(drug.consumption[[d]]))))
}

# construct deweighting combinations
values <- seq(0, 1, by = 0.25)
combinations <- expand.grid(rep(list(values), length(deweight_cols)))
deweighting_vecs <- apply(combinations, 1, as.vector)
deweighting_vecs <- matrix(nrow=1, ncol=0)

# iterative building #
for (deweighting_vec_num in 1:nrow(deweighting_vecs))
{
    # =============== MODEL TRAINING ================ #
    print("    training the model...")

    # iterative design + tracking #
    # uncode encoded features
    print("    deweighting data...")
    deweighting_vec <- deweighting_vecs[deweighting_vec_num, ]
    dw_vec <- vector(length=0)

    for (i in 1:num_sens)
    {
        dw_vec <- c(dw_vec, rep(deweighting_vec[i], deweighting_cols_lvls[i]))
    }

    # deweight data
    # mod_dc <- dc
    # print(deweighting_cols)
    # mod_dc[, deweighting_cols] <- mod_dc[, deweighting_cols] * dw_vec

    # train each drug model
    for (y in predictive_vars_cols)
    {
        unique_values <- lapply(dc, unique)     
        model <- qeKNN(
            data = dc,
            yName = y
            # expandVars = deweighting_cols,
            # expandVals = dw_vec,
        )

        # =============== MODEL EVALUATION ================ #
        print("    evaluating model metrics...")
        print(paste("training for ", y))
        print(paste("using weights: ", deweighting_vec))

        sink("model_evaluation.txt", append = TRUE)

        x_cols <- setdiff(names(dc), c(y, sens_levels))
        fairness <- fairness_metric(dc, x_cols, s_levels=sens_levels, model)

        preds <- predict(model, dc[, x_cols])
        result <- sensitivity_and_specificity(preds, dc[[y]], match_weights)
        sensitivity <- result[1]
        specificity <- result[2]

        print(paste(y, ",", fairness, ",", sensitivity, ",", specificity, ","))

        sink()
    }
}


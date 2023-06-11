# ======================= FILE INFO ======================= #
# :: ECS 189G: Term Project
# :: Authored by Arjun Ashok, HW Group 7
# 
# :: This file defines utility for exploring, analyzing, and experimenting with
# :: the FairML dataset 'drug.consumption' by Scutari


# =============== FEATURE ENGINEEERING =============== #
# :: Converts a predictive, sensitive, proxy, or other feature into its numeric 
#    constituents on the assumption that they can be converted into a numeric representation
# :: Returns a dataframe with the converted columns
conv_numeric_factors <- function(data, cols, weighting)
{
    # for each column
    for (col in cols)
    {
        # replace w/ matching values
        data[[col]] <- ifelse(
            data[[col]] %in% weighting$level, 
            weighting$weight[match(data[[col]], weighting$level)], 
            data[[col]]
        )
    }

    return(data)
}


# =============== FAIRNESS-UTILITY TRADEOFF =============== #
# :: Fairness-utility tradeoff with or without a specific sensitive var
# :: Originally written for Homework 1, Problem1. Original source code in Homework1>Problem1.R
# :: Returns a dataframe with fairness-utility for the sensitive feature
fair_util_tradeoff <- function(data, yName, sName, maxFeatureSetSize=1)
{
    # libraries #
    require(qeML)

    # generate feature sets #
    features <- names(data)[ !( names(data) %in% c(yName, sName) ) ]

    feature_set <- list()
    for (i in 1:maxFeatureSetSize)
    {
        subsets <- combn(features, i, simplify = FALSE)
        feature_set <- c(feature_set, subsets)
    }

    # initialize dataframe #
    comparison_names <- c("feature set", "Y Accuracy", "Y Accuracy w/ S", "S Accuracy")
    comparison <- data.frame( matrix(nrow = 0, ncol = length(comparison_names)) )
    names(comparison) <- comparison_names

    for (feature_subset in feature_set)
    {
        # create subset string #
        feature_subset_string <- paste(feature_subset, collapse = ",")

        # calculate comparison (a) & (b) #
        df_feature_set <- data[ , c(feature_subset, yName) ]
        df_feature_set_s <- data[ , c(feature_subset, sName, yName) ]

        # check numeric
        if ( is.numeric(data[ 1, yName ]) )
        {
            y_pred <- qeLin(df_feature_set, yName)
            y_pred_s <- qeLin(df_feature_set_s, yName)
        }
        else
        {
            y_pred <- qeLogit(df_feature_set, yName)
            y_pred_s <- qeLogit(df_feature_set_s, yName)
        }

        # calculate comparison (c) #
        df_feature_set_pred_s <- data[ , c(feature_subset, sName) ]

        # check numeric
        if ( is.numeric(data[ 1, sName]) )
        {
            s_pred <- qeLin(df_feature_set_pred_s, sName)
        }
        else
        {
            s_pred <- qeLogit(df_feature_set_pred_s, sName)
        }

        # add entry #
        entry <- data.frame( feature_subset_string, 
                             y_pred$testAcc, 
                             y_pred_s$testAcc, 
                             s_pred$testAcc
        )
        names(entry) <- comparison_names

        comparison <- rbind(comparison, entry)
    }

    comparison
}


# =============== CORRELATIONS =============== #
# :: Calculates the correlation between a dataframe with 1+ sensitive vars, 0+ numerical columns,
#    and 0+ categorical columns. Uses pearson correlation for numeric, kendall-tau correlation for
#    categorical correlation. Y must be an underlying numeric variable, though this can be in the 
#    form of a factor with numeric-based levels.
# :: Returns a dataframe with correlation values for each feature against each sensitive feature
sens_cors <- function(data, sens_cols, y_col, numeric_cols=vector(length=0), categ_cols=vector(length=0))
{
    # error check #
    num_numeric <- length(numeric_cols)
    num_categ <- length(categ_cols)
    if (num_numeric == 0 && num_categ == 0)
    {
        # error message, empty return
        print(" ### ERROR: invalid use of sens_cors(), must include values for one of [numeric_cols, categ_cols]")
        return(NULL)
    }

    # calculate correlations #
    # define subset data
    data <- data[, !names(data) %in% sens_cols]
    data[, numeric_cols] <- lapply(data[, numeric_cols], as.numeric)

    # matrices
    numeric_matrix <- cor(data[, numeric_cols], method = "pearson")
    categ_matrix <- cor(data[, categ_cols], method = "kendall")

    # combine into one
    corr_matrix <- matrix(NA, nrow = num_numeric_cols + num_categ_cols, ncol = num_numeric_cols + num_categ_cols)
    corr_matrix[1:num_numeric, 1:num_numeric] <- numeric_matrix
    corr_matrix[(num_numeric + 1):(num_numeric + num_categ), (num_numeric + 1):(num_numeric + num_categ)] <- categ_matrix


    # labeled dataframe #
    labels <- c(numeric_cols, categ_cols)
    corr_df <- data.frame(corr_matrix, row.names = labels)
    names(corr_df) <- labels

    return(corr_df)
}

# :: Calculates a correlation matrix for all columns in a dataframe to detect/diagnose collinearity
#    between multiple features, etc. Sensitive columns are excluded since their correlation is 
#    better diagnosed in a different context. Numeric columns will be forced to numeric values.
# :: Returns a correlation matrix with correlation values for each column against each other column
cor_matrix <- function(data, sens_cols, y_col, numeric_cols=vector(length=0), categ_cols=vector(length=0))
{
    # error check #
    num_numeric <- length(numeric_cols)
    num_categ <- length(categ_cols)
    if (num_numeric == 0 && num_categ == 0)
    {
        # error message, empty return
        print(" ### ERROR: invalid use of sens_cors(), must include values for one of [numeric_cols, categ_cols]")
        return(NULL)
    }


    # calculate correlation matrix #
    # define subset data
    data <- data[, !names(data) %in% sens_cols]
    data[, numeric_cols] <- lapply(data[, numeric_cols], as.numeric)

    # matrices
    numeric_matrix <- cor(data[, numeric_cols], method = "pearson")
    categ_matrix <- cor(data[, categ_cols], method = "kendall")

    # combine into one
    corr_matrix <- matrix(NA, nrow = num_numeric_cols + num_categ_cols, ncol = num_numeric_cols + num_categ_cols)
    corr_matrix[1:num_numeric, 1:num_numeric] <- numeric_matrix
    corr_matrix[(num_numeric + 1):(num_numeric + num_categ), (num_numeric + 1):(num_numeric + num_categ)] <- categ_matrix


    # labeled dataframe #
    labels <- c(numeric_cols, categ_cols)
    corr_df <- data.frame(corr_matrix, row.names = labels)
    names(corr_df) <- labels

    return(corr_df)
}


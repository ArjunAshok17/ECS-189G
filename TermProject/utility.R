# ======================= FILE INFO ======================= #
# :: ECS 189G: Term Project
# :: Authored by Arjun A., HW Group 7
# 
# :: This file defines utility for exploring, analyzing, and experimenting with
# :: the FairML dataset 'drug.consumption' by Scutari


# =============== FAIRNESS-UTILITY TRADEOFF =============== #
# :: Fairness-utility tradeoff with or without a specific sensitive var
# :: Originally written for Homework 1, Problem1. Original source code in Homework1>Problem1.R
# :: Returns a dataframe with fairness-utility for the sensitive feature
fu_tradeoff <- function(data, yName, sName, maxFeatureSetSize)
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


# =============== SENSITIVE CORRELATIONS =============== #
# :: Calculates the correlation between a dataframe with 1+ sensitive vars, 0+ numerical columns,
#    and 0+ categorical columns. Uses pearson correlation for numeric, kendall-tau correlation for
#    categorical correlation. Y must be an underlying numeric variable, though this can be in the 
#    form of a factor with numeric-based levels.
# :: Returns a dataframe with correlation values for each column against each other column
sens_cors <- function(data, sens_cols, y_col, numeric_cols=NULL, categ_cols=NULL)
{

}

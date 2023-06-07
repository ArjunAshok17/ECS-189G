takeALookAround <- function(data, yName, sName, maxFeatureSetSize)
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
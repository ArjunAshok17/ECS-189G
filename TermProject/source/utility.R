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


# :: Converts factor variables with no underlying numeric basis into their one-hot encoded
#    forms, allowing us to utilize them with model training
# :: Returns a dataframe with the replaced columns
one_hot_encode <- function(data, cols)
{
    # for each categorical column
    for (col in cols)
    {
        # model matrix for one-hot encoding
        encoded <- model.matrix(~ -1 + data[[col]])
        
        # get names + remove naming artifact
        col_names <- colnames(encoded)
        col_names <- sub("^.*]]", "", col_names)
        
        # replace column
        data[col_names] <- encoded
        data[[col]] <- NULL
    }
  
    # return
    return(data)
}


# =============== FAIRNESS-UTILITY TRADEOFF =============== #
# :: Fairness-utility tradeoff with or without a specific sensitive var
# :: Originally written for Homework 1, Problem1. Original source code in Homework1>Problem1.R
# :: Returns a dataframe with fairness-utility for the sensitive feature
fair_util_tradeoff <- function(data, yName, sName)
{
    # libraries #
    require(qeML)

    # generate feature sets #
    features <- names(data)[ !( names(data) %in% c(yName, sName) ) ]

    # initialize dataframe #
    comparison_names <- c("feature", "Y Accuracy", "Y Accuracy w/ S", "S Accuracy")
    comparison <- data.frame( matrix(nrow = 0, ncol = length(comparison_names)) )
    names(comparison) <- comparison_names

    # calculate comparison (a) & (b) #
    df_feature_set <- data[ , c(features, yName) ]
    df_feature_set_s <- data[ , c(features, sName, yName) ]

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
    df_feature_set_pred_s <- data[ , c(features, sName) ]

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
    entry <- data.frame(
        y_pred$testAcc, 
        y_pred_s$testAcc, 
        s_pred$testAcc
    )
    names(entry) <- comparison_names
    comparison <- rbind(comparison, entry)

    comparison
}


# =============== CORRELATIONS =============== #
###### not currently working ######
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
    num_sens <- length(sens_cols)

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

    # find correlations
    correlations <- data.frame( matrix(nrow = 0, ncol = num_sens) )
    names(correlations) <- sens_cols

    for (num_feature in numeric_cols)
    {
        cors = vector(length = num_sens)

        for (i in 1:num_sens)
        {
            cor[i] <- cor(x=data[[num_feature]], y=as.numeric(data[[sens_cols[i]]]), method = "spearman")
        }
        
        correlations <- rbind(correlations, cor)
    }

    for (cat_feature in numeric_cols)
    {
        cors = vector(length = num_sens)

        for (i in 1:num_sens)
        {
            cor[i] <- cor(x=data[[cat_feature]], y=data[[sens_cols[i]]], method = "kendall")
        }
        
        correlations <- rbind(correlations, cor)
    }

    return(correlations)

    # matrices
    # numeric_matrix <- cor(data[, numeric_cols], method = "pearson")
    # categ_matrix <- cor(data[, categ_cols], method = "kendall")

    # # combine into one
    # corr_matrix <- matrix(NA, nrow = num_numeric + num_categ, ncol = num_numeric + num_categ)
    # corr_matrix[1:num_numeric, 1:num_numeric] <- numeric_matrix
    # corr_matrix[(num_numeric + 1):(num_numeric + num_categ), (num_numeric + 1):(num_numeric + num_categ)] <- categ_matrix
    # print("TEST")


    # # labeled dataframe #
    # labels <- c(numeric_cols, categ_cols)
    # corr_df <- data.frame(corr_matrix, row.names = labels)
    # names(corr_df) <- labels

    # return(corr_df)
}


###### not currently working ######
# :: Calculates a correlation matrix for all columns in a dataframe to detect/diagnose collinearity
#    between multiple features, etc. Sensitive columns are excluded since their correlation is 
#    better diagnosed in a different context. Numeric columns will be forced to numeric values.
# :: Returns a correlation matrix with correlation values for each column against each other column
cor_matrix <- function(data, sens_cols, numeric_cols=vector(length=0), categ_cols=vector(length=0))
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
    corr_matrix <- matrix(NA, nrow = num_numeric + num_categ, ncol = num_numeric + num_categ)
    corr_matrix[1:num_numeric, 1:num_numeric] <- numeric_matrix
    corr_matrix[(num_numeric + 1):(num_numeric + num_categ), (num_numeric + 1):(num_numeric + num_categ)] <- categ_matrix


    # labeled dataframe #
    labels <- c(numeric_cols, categ_cols)
    corr_df <- data.frame(corr_matrix, row.names = labels)
    names(corr_df) <- labels

    return(corr_df)
}


# =============== SAVING GRAPHICS =============== #
# :: Authored by Professor Matloff, taken from ECS 189G Blog
pr2file <- function(filename)
{
    origdev <- dev.cur()
    parts <- strsplit(filename,".",fixed=TRUE)
    nparts <- length(parts[[1]])
    suff <- parts[[1]][nparts]
    if (suff == "pdf") {
        pdf(filename)
    }
    else if (suff == "png") {
        png(filename,bg='white')
    }
    else jpeg(filename)
    devnum <- dev.cur()
    dev.set(origdev)
    dev.copy(which = devnum)
    dev.set(devnum)
    dev.off()
    dev.set(origdev)
}


# =============== MISC =============== #
get_column_levels <- function(data) {
  levels_list <- vector("list", length = ncol(data))
  for (i in seq_along(data)) {
    levels_list[[i]] <- levels(data[[i]])
  }
  levels_list
}


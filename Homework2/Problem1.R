relativeProxy <- function(data, proxyName, catName, nQuantls, nCut=NULL)
{
    # require libraries #
    library(qeML)

    # split dataset #
    # check factor column
    if (!is.null(nCut))
    {
        # get intervals and cut
        intervals <- as.vector( quantile(data[, catName], probs = seq(0, 1, 1 / nCut)) )
        data[, catName] <- cut(data[, catName], breaks = intervals, labels = FALSE, include.lowest = TRUE)
    }
    else
    {
        # interval are the levels if already category
        intervals <- levels(factor(data[[catName]]))
    }

    # split dataset
    lvl_num <- 1
    quantls <- list()
    for (lvl in levels(factor(data[, catName])))
    {
        # append family income level
        quantls[[lvl_num]] <- data[ data[, catName] == lvl, ]
        lvl_num <- lvl_num + 1
    }

    # replace proxy #
    # for each family income level data
    lvl_num <- 1
    for (lvl_data in quantls)
    {
        # get quantiles
        quantile_values <- quantile(lvl_data[[proxyName]], probs = seq(0, 1, length.out = nQuantls))

        # Replace the values with their quantile decimal values
        lvl_data[[proxyName]] <- findInterval(lvl_data[[proxyName]], quantile_values) / nQuantls

        # replace in quantls
        quantls[[lvl_num]] <- lvl_data
        lvl_num <- lvl_num + 1
    }

    # make relProx object #
            # relProxObj <- list(quantls)
            # class(relProxObj) <- "relProx"
            # attr(relProx, "intervals") <- intervals
            # relProxObj
    class(quantls) <- "relProx"
    quantls$intervals <- intervals
    attr(quantls, "intervals") <- intervals
    quantls
}


predict.relProx <- function(relProxObj, newCases, proxyName, catName)
{
    # calculate quantiles #
    # variable handling
    num_cases <- nrow(newCases)
    preds <- vector(length = 0)

    # for every new case
    for (row in 1:num_cases)
    {
        # calculate quantile using intervals
        d <- newCases[row, ]
        interval_num <- 0               # initialize to zero to compensate for + 1 iteration in loop

        # calculate interval
        for (i in relProxObj$intervals)
        {
            # if we pass correct cutoff for numeric
            if (d[[catName]] > i)
            {
                break
            }

            # if we are in the interval for factor
            if (d[[catName]] == i)
            {
                interval_num <- interval_num + 1
                break
            }
            
            interval_num <- interval_num + 1
        }

        # use interval to calculate quantile wrt correct income level
        lvl_data <- relProxObj[[interval_num]]
        new_quantile <- sum( lvl_data[[proxyName]] <= d[[proxyName]]) / nrow(lvl_data)      # proportion below = quantile

        # add new quantile
        preds <- c(preds, new_quantile)
    }

    # return results #
    preds
}


# UNNEEDED CODE
        # newCut <- function(data, col, nCut)
        # {
        #     # variable handling #
        #     d <- data[, col]                        # column vector
        #     num_entries <- length(d)                # helper var
        #     cuts <- vector(length = nCut)           # inititalize cuts
        #     delta <- floor(num_entries / nCut)      # delta in number of elements for each cut

        #     # find edge values
        #     sort(d)                                 # efficient boundary finding
        #     cuts[1] <- d[1]                         # min
        #     cuts[nCut] <- d[num_entries]            # max

        #     # find boundaries #
        #     for (cut in 2:(nCut - 1))
        #     {

        #     }

        #     r <- nrow(data)
        #     floor(n)
        # }


empiricalStudy <- function()
{
    # requirements #
    library(qeML)
    load("./law.school.admissions.rda")

    # data management & setup #
    lsa <- law.school.admissions                            # LSAT used
    lsa <- lsa[, names(lsa) %in% c(
        "fam_inc", 
        "lsat", 
        "ugpa", 
        "cluster", 
        "bar"
    )]
    lsa$bar <- as.numeric(factor(lsa$bar))                  # T/F to numeric

    w <- relativeProxy(lsa, "lsat", "fam_inc", 5, NULL)     # divide by income
    u <- predict(w, lsa, "lsat", "fam_inc")                 # predict on training set

    # replace columns
    lsa_mod <- lsa
    lsa_mod$lsat <- u                                       # relative LSAT used
    lsa_mod$fam_inc <- NULL                                 

    lsa_none <- lsa_mod                                     # neither used
    lsa_none$lsat <- NULL                                    

    # modeling w/ qeKNN() #
    trad_model <- qeKNN(lsa, "bar")
    mod_model <- qeKNN(lsa_mod, "bar")
    none_model <- qeKNN(lsa_none, "bar")

    # assessment #
    # data setup for fairness
    family_inc <- lsa$fam_inc
    trad_pred_cases <- lsa[, !names(lsa) %in% c("bar")]
    mod_pred_cases <- lsa_mod[, !names(lsa_mod) %in% c("bar")]
    none_pred_cases <- lsa_none[, !names(lsa_none) %in% c("bar")]

    # kendall correlation between income and prediction bar is our Y
    fair_vec <- c(
        cor.test( family_inc, predict(trad_model, trad_pred_cases), method = "kendall" )$estimate,
        cor.test( family_inc, predict(mod_model, mod_pred_cases), method = "kendall" )$estimate,
        cor.test( family_inc, predict(none_model, none_pred_cases), method = "kendall" )$estimate
    )

    # test accuracy is for our utility
    util_vec <- c(
        trad_model$testAcc, 
        mod_model$testAcc,
        none_model$testAcc
    )

    study <- data.frame(
        model_names = c("Traditional w/ LSAT", "Relative LSAT w/ Income", "No LSAT or Income"),
        fairness = fair_vec,
        utility = util_vec
    )

    print(study)
}


empiricalStudy()


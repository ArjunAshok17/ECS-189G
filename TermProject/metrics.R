# =============== FILE INFO ================ #
# :: ECS 189G: Term Project
# :: Authored by Arjun Ashok, HW Group 7
#
# :: This file defines utility for gauging both fairness and utility with respect
#    to our model's function. We use the same metrics as the paper we are comparing 
#    ourselves to, as well as some of our own metrics to ensure we've met the criteria
#    laid out (in terms of providing a fair, unbiased algorithm).


library(qeML)
library(Kendall)

fairness_metric <- function(data, x_cols, s_levels, model)
{
    pred <- as.numeric(predict(model, data[, x_cols]))

    fair <- 0

    # for (s in 1:length(s_levels))
    # {
    #     fair <- fair + cor(data[, s_levels[[s]]], pred, method = "kendall")
    # }

    fair <- fair + cor(pred, data[, "Male"], method = "kendall")

    return(fair / 1) #length(s_levels))
}

reencode_preds <- function(preds, weighting)
{
    # classifications <- cut(preds, breaks = weighting[, "weight"], labels = weighting[, "level"], include.lowest = TRUE)
    classifications <- weighting$label[findInterval(preds, weighting$weight)]
    return(classifications)
}

sensitivity_and_specificity <- function(preds, expected, weighting)
{
    reencoded_pred <- reencode_preds(preds, weighting)

    confusion_matrix <- table(preds, expected)
    TN <- confusion_matrix[1, 1]
    FP <- confusion_matrix[1, 2]
    FN <- confusion_matrix[2, 1]
    TP <- confusion_matrix[2, 2]
    
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    
    return(c(sensitivity, specificity))
}

weightedPredict <- function(endpoints = c(.1, 1, .1))
{
    # libraries #
    library(qeML)
    library(Kendall)
    library(regtools)
    library(ggplot2)
    data(pef)

    # variable handling #
    # define columns
    yName <- "wageinc"
    sName <- "sex"
    cName <- "occ"
    num_factors <- 5

    # unpack weights
    start_d <- endpoints[1]
    end_d <- endpoints[2]
    step_d <- endpoints[3]
    weights <- seq(from = start_d, to = end_d, step_d)

    uf_track <- data.frame( matrix(nrow = 0, ncol = 2) )
    names(uf_track) <- c("utility", "fairness")

    # data cleansing #
        # we only need 5 dummies since the abscence of 5 implies the prescence of the sixth,
        # minimizes features => drop last column to do so
        #
        # as.numeric() would imply it's a regression problem, but we're trying to model categorical data 
        # -> vector of classes makes more sense
    # dummy variable replacement
    occ_vec <- factorToDummies(pef$occ, "occ")
    occ_vec <- occ_vec[ , -ncol(occ_vec)]       # drop last column

    # join results & clean
    cNames <- colnames(occ_vec)
    pef1 <- cbind(pef, occ_vec)
    pef1$occ <- NULL
    pef1$sex <- NULL

    # numeric sex vector for correlation
    s_vec <- as.numeric(pef$sex)
            # s_vec <- as.numeric(pef$sex)
            # s_vec[ s_vec == "1" ] <- 0
            # s_vec[ s_vec == "2" ] <- 1

    # test multiple weightings #
    # for different weighting values
    for (weight in weights)
    {
        # create model
        weighted_model <- qeKNN(
            pef1,
            yName,
            expandVars = cNames,
            expandVals = rep(weight, num_factors),
            scaleX = TRUE
        )

        # gauge fairness & accuracy
        pef_pred <- pef[ weighted_model$holdldxs, ]
        pef_pred$wageinc <- NULL
        prediction <- as.vector(predict(weighted_model, pef_pred))

        new_entry <- data.frame(
                        utility = weighted_model$testAcc,
                        fairness = cor.test( s_vec, prediction, method = "kendall" )$estimate
                     )

        uf_track <- rbind(
            uf_track, 
            new_entry
        )
    }

    # plot results #
    # get data & scale
            # utility <- (utility - mean(utility)) / sd(utility)
    utility <- uf_track$utility
    utility <- 1 / utility
    fairness <- uf_track$fairness

    # plotting
    plot(weights, fairness, type = "o", col = "blue", xlab = "weights", ylab = "fairness", xlim = range(weights), ylim = range(-1, 1))
    lines(weights, utility, type = "o", col = "red")
    legend("topleft", legend = c("Fairness (S vs Y Cor)", "Utility (1 / testacc)"), col = c("blue", "red"), lty = 1)

    # double y-axis plot
            # uf_track <- cbind(uf_track, data.frame("weights" = weights))
            # ggplot(uf_track, aes(x = weights)) + 
            #     geom_line(aes(y = fairness, color = "Red")) + 
            #     geom_line(aes(y = scale(utility), color = "Blue"), linetype = "dashed") + 
            #     scale_y_continuous(
            #         name = "Fairness"
            #         sec.axis = sec_axis(trans = ~.*25000, name = "Utility"),
            #     ) + 
            #     labs(x = "Weights") + 
            #     theme_minimal()
}


# call function as script
weightedPredict()


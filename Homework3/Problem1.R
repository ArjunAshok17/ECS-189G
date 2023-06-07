conditDisparity <- function(data, yName, sName, xName, condits, qeFtn, minS = 50, yLim = NULL, useLoess = TRUE)
{
    # libraries #
    library(qeML)


    # initialize datasets #
    # load conditions
    for (condit in condits)
    {
        # add condition
        data <- eval(parse(text = (sprintf("data[ data$%s, ]", condit))))
    }

    # check levels
    for (s_lvl in levels(factor(data[[sName]])))
    {
        # if min isn't met
        if (nrow(data[ data[[sName]] == s_lvl, ]) < minS)
        {
            data <- data[ data[[sName]] != s_lvl, ]         # remove data points, disclude level
        }
    }

    s_levels <- levels(factor(data[[sName]]))               # for labeling in legend
    num_levels <- length(s_levels)                          # for labeling in plotting

    # load cols
    y_data <- data[[yName]]                                 # expected output
    x_data <- data[[xName]]                                 # input
    s_data <- data[[sName]]                                 # sensitive var


    # modeling #
    # variable handling
    colors <- colorRampPalette(c("blue", "red"))(num_levels)

    # model initialization, prediction, & plotting
    for (lvl in 1:num_levels)
    {
        # setup data #
        svar = s_levels[lvl]
        x_subset <- x_data[ s_data == svar ]                # subset of x data for current sensitive level
        y_subset <- y_data[ s_data == svar ]                # subset of y data for current sensitive level
        data_subset <- eval(parse(text = sprintf("data.frame(%s = x_subset, %s = y_subset)", xName, yName)))

        # model dependent on sensitive var level
        model <- qeFtn(
            data = data_subset,
            yName = yName,
            holdout = NULL
        )
        
        # predict & store
        pred <- as.numeric(predict(model, data.frame(x_subset)))

        # setup plotting
        if (useLoess)
        {
            loess_func <- loess(pred ~ x_subset)
            pred <- predict(loess_func, new_data = data.frame(x_subset))
        }

        if (is.null(yLim))
        {
            yLim <- c(
                min(y_data),
                max(y_data)
            )
        }

        sort_x <- order(x_subset)
        x_subset <- x_subset[ sort_x ]
        pred <- pred[ sort_x ]

        # plot data
        if (lvl == 1)
        {
            plot(
                x = x_subset,
                y = pred,
                type = 'l',
                ylim = yLim,
                col = colors[lvl],
                xlab = xName, 
                ylab = yName, 
                main = paste("Underlying Effects of ", sName, " on ", yName, " wrt ", xName)
            )
        }
        else
        {
            lines(x_subset, pred, col = colors[lvl])
        }
    }

    legend("topright", legend = s_levels, col = colors, lty = 1, bty = "n")
}
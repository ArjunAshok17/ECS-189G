numericSimpson <- function(data, xName, yName, zName, numZvals = NULL)
{
    # libraries #
    require(qeML)
    require(Kendall)

    # unconditional correlation #
    kendall_cor = FALSE
    if ( is.numeric(data[1, xName]) & is.numeric(data[1, yName]) )
    {
        correlation <- cor( data[, xName], data[, yName] )
    }
    else
    {
        # change flag
        kendall_cor = TRUE

        # convert to numeric
        if (is.factor( data[1, xName] ))
        {
            data[, xName] <- as.numeric(factor(data[, xName]))
        }
        if (is.factor( data[1, yName] ))
        {
            data[, yName] <- as.numeric(factor(data[, yName]))
        }

        correlation <- cor.test( data[, xName], data[, yName], method = "kendall" )$estimate
    }

    # partitions #

    # check null value
    if ( is.null(numZvals) )
    {
        numZvals = 1
    }
    
    # force conforming to Z #
    if ( is.factor(data[1, zName]) )
    {
        # force z intervals
        intervals <- levels(data[, zName])
        numZvals <- length(intervals)
        
        # store correlations
        correlations <- vector(length = numZvals)

        # calculate correlations
        for (p in 1:numZvals)
        {
            # divide data
            interval <- data[data[, zName] == intervals[p], ]

            # calculate correlation dependent on type
            if (!kendall_cor)
            {
                c <- cor( interval[, xName], interval[, yName] )
            }
            else
            {
                c <- cor.test( interval[, xName], interval[, yName], method = "kendall" )$estimate
            }

            # store results
            correlations[p] <- c
        }
    }
    else
    {
        # store correlations
        correlations <- vector(length = numZvals)

        # partitioning w/ cut()
        partitions <- cut( data[, zName], breaks = numZvals )
        intervals <- levels(partitions)

        # calculate correlations
        for (p in 1:numZvals)
        {
            # divide data
            interval <- data[partitions %in% intervals[p], ]

            # calculate correlation dependent on type
            if (!kendall_cor)
            {
                c <- cor( interval[, xName], interval[, yName] )
            }
            else
            {
                c <- cor.test( interval[, xName], interval[, yName], method = "kendall" )$estimate
            }

            # store results
            correlations[p] <- c
        }
    }

    # plot correlations #
    colors <- colorRampPalette( c("blue", "red") )(numZvals)
    barplot(correlations,
            names.arg = intervals,
            ylim = c( -1, 1 ),
            xlab = "zLvls",
            ylab = "cors",
            main = paste("X = ", xName, ", Y = ", yName, ", Z = ", zName),
            col = colors
    )
    abline(h = 0, col = "black")                        # x-axis
    abline(h = correlation, col = "black", lty = 2)     # correlation line
}

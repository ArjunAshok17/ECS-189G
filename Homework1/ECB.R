superFactor <- function(f1, f2)
{
    # calculate length for new factor #
    f1_cat <- levels(f1)
    f2_cat <- levels(f2)
    f1_len <- length(f1_cat)
    f2_len <- length(f2_cat)

    sf <- vector(length = f1_len * f2_len)

    # populate vector #
    for (i in 1:length(sf))
    {
        new_factor <- paste(f1_cat[i %/% f2_len], f2_cat[i %% f2_len], sep = "-")
        sf[i] <- new_factor
    }

    # create factor variable #
    factor(sf)
}
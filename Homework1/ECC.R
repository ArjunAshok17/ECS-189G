bootstrap <- function(data, estimator, m = 1000, n_k = 30) {
    T_values <- numeric(m)

    for (i in 1:m) {
        indices_sub_set <- sample(1:nrow(data), n_k, replace = TRUE)
        data_of_subset <- data[indices_sub_set, ]
        T_values[i] <- estimator(data_of_subset)
    }

    return(quantile(T_values, c(0.025, 0.975)))
}

addErrorBars <- function(data, estimator, m = 1000, n_k = 30) {
    numericSimpson(data)
    ci <- bootstrap(data, estimator, m, n_k)

    arrows(x0 = 1, y0 = ci[1], x1 = 1, y1 = ci[2], lwd = 2)
    segment(x0 = 1, y0 = ci[1], x1 = 1, y1 = ci[1], angle = 90, length = 0.05, code = 3)
    segement(x0 = 1, y0 = ci[2], x1 = 1, y1 = ci[2], angle = 90, length = 0.05, code = 3)

    return(ci)
}
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  n <- length(regressionSource)
  stopifnot(is.numeric(regressionSource),
            length(regressionLength) == 1L, is.finite(regressionLength),
            regressionLength == floor(regressionLength),
            regressionLength >= 2, regressionLength <= n,
            length(regressionOffset) == 1L, is.finite(regressionOffset),
            regressionOffset == floor(regressionOffset),
            regressionOffset >= 0, regressionOffset < regressionLength)
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  if (end_index - start_index < 1L) {
    stop("The regression window must contain at least two points")
  }
  source_subset <- regressionSource[seq.int(start_index, end_index)]
  index_values <- seq_along(source_subset)
  centered_index <- index_values - mean(index_values)
  slope <- sum(centered_index * (source_subset - mean(source_subset))) /
    sum(centered_index^2)
  intercept <- mean(source_subset) - slope * mean(index_values)
  list(slope = slope, intercept = intercept,
       predicted_values = slope * index_values + intercept)
}

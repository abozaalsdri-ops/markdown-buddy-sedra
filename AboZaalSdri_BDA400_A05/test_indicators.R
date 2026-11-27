for (name in c("sma", "ema", "macd", "stdev", "linreg", "rsi",
               "stoch_rsi", "crossover", "crossunder")) {
  source(paste0(name, ".R"))
}

expect <- function(actual, expected) {
  stopifnot(isTRUE(all.equal(actual, expected)))
}
expect(sma(c(2, 4, 8), 2), c(3, 6))
expect(ema(c(2, 4, 8), 3), c(2, 3, 5.5))
expect(macd(c(2, 4, 8), 1, 3, 3),
       list(macd_line = c(0, 1, 2.5), signal_line = c(0, 0.5, 1.5),
            histogram = c(0, 0.5, 1)))
expect(stdev(c(2, 4)), 1)
expect(linreg(c(2, 4, 6, 8), 3, 0),
       list(slope = 2, intercept = 2, predicted_values = c(4, 6, 8)))
expect(rsi(c(10, 12, 11, 14), 2),
       c(NA_real_, NA_real_, 200 / 3, 800 / 9))
expect(stoch_rsi(c(1, 2, 1, 2), 1, 1, 2),
       list(k_line = c(NA_real_, 1, 0, 1), d_line = c(NA_real_, 0.5, 0.5)))
expect(crossover(c(2, 3, 1), rep(2, 3)), c("None", "Up", "Down"))
expect(crossunder(c(2, 3, 1), rep(2, 3)), c("None", "False", "True"))
cat("All nine indicator checks passed.\n")

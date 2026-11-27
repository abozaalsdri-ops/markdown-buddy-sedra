# Run from the directory containing the indicator scripts.
for (name in c("sma", "ema", "macd", "stdev", "linreg", "rsi",
               "stoch_rsi", "crossover", "crossunder")) {
  source(paste0(name, ".R"))
}

analyze_stocks <- function(csv, start_date = NULL, end_date = NULL,
                           timeframe = c("daily", "weekly", "monthly"),
                           data_source = NULL) {
  timeframe <- match.arg(timeframe)
  stock <- read.csv(csv, stringsAsFactors = FALSE)
  names(stock) <- tolower(trimws(names(stock)))
  required <- c("symbol", "date", "open", "high", "low", "close", "volume")
  if (!all(required %in% names(stock))) {
    stop("Expected columns: ", paste(required, collapse = ", "))
  }
  stock$date <- as.Date(stock$date)
  price_columns <- c("open", "high", "low", "close", "volume")
  valid_numeric <- vapply(stock[price_columns], function(values) {
    is.numeric(values) && all(is.finite(values))
  }, logical(1))
  if (anyNA(stock$date) || anyNA(stock$symbol) ||
      any(!nzchar(trimws(stock$symbol))) || !all(valid_numeric)) {
    stop("Check dates, symbols and numeric OHLCV columns")
  }
  if (!is.null(data_source)) {
    if (!"source" %in% names(stock)) stop("A Source column is required")
    stock <- stock[stock$source %in% data_source, , drop = FALSE]
  }
  if (!is.null(start_date)) stock <- stock[stock$date >= as.Date(start_date), ]
  if (!is.null(end_date)) stock <- stock[stock$date <= as.Date(end_date), ]
  if (anyDuplicated(stock[c("symbol", "date")])) {
    stop("Select one source or remove duplicate dates within each symbol")
  }

  frames <- lapply(split(stock[required], stock$symbol), function(frame) {
    frame <- frame[order(frame$date), ]
    if (timeframe != "daily") {
      groups <- if (timeframe == "weekly") {
        format(frame$date, "%G-%V")
      } else {
        format(frame$date, "%Y-%m")
      }
      frame <- do.call(rbind, lapply(split(frame, groups), function(rows) {
        last <- nrow(rows)
        data.frame(symbol = rows$symbol[1], date = rows$date[last],
                   open = rows$open[1], high = max(rows$high),
                   low = min(rows$low), close = rows$close[last],
                   volume = sum(rows$volume))
      }))
    }
    close <- frame$close
    aligned_sma <- function(period) {
      if (length(close) < period) return(rep(NA_real_, length(close)))
      c(rep(NA_real_, period - 1L), sma(close, period))
    }
    frame$sma_fast <- aligned_sma(5)
    frame$sma_slow <- aligned_sma(20)
    frame$ema <- ema(close, 12)
    trend <- macd(close, 12, 26, 9)
    frame$macd <- trend$macd_line
    frame$macd_signal <- trend$signal_line
    frame$macd_histogram <- trend$histogram
    frame$rsi <- rsi(close, 14)
    momentum <- stoch_rsi(close, 14, 3, 3)
    frame$stoch_k <- c(rep(NA_real_, nrow(frame) - length(momentum$k_line)),
                       momentum$k_line)
    frame$stoch_d <- c(rep(NA_real_, nrow(frame) - length(momentum$d_line)),
                       momentum$d_line)
    frame$crossover <- crossover(frame$sma_fast, frame$sma_slow)
    frame$crossunder <- crossunder(frame$sma_fast, frame$sma_slow)
    frame$signal <- "Hold"
    frame$signal[which(frame$crossover == "Up" & frame$rsi < 70)] <- "Buy"
    frame$signal[which(frame$crossunder == "True")] <- "Sell"
    frame
  })
  candidates <- vapply(frames, function(frame) {
    latest <- frame[nrow(frame), ]
    isTRUE(latest$sma_fast > latest$sma_slow & latest$rsi < 70)
  }, logical(1))
  summaries <- lapply(frames, function(frame) {
    close <- frame$close
    list(close_stdev = stdev(close),
         close_regression = if (length(close) >= 2L) {
           linreg(close, min(20L, length(close)), 0)
         } else {
           NULL
         })
  })
  list(by_symbol = frames, summaries = summaries,
       screened_symbols = names(frames)[candidates])
}


rolling_avg_df <- function(df, window_length, threshold) {
  
  required_cols <- c("Time_Start", "Time_End", "preds")
  if (!all(required_cols %in% names(df))) {
    stop("Data frame must contain: Time_Start, Time_End, preds")
  }
  
  df <- df %>%
    mutate(preds = as.numeric(as.character(preds)))
  
  n <- nrow(df)
  rolling_avg <- numeric(n)
  
  for (i in seq_len(n)) {
    center <- (df$Time_Start[i] + df$Time_End[i]) / 2
    window_start <- center - window_length / 2
    window_end   <- center + window_length / 2
    
    idx <- which(df$Time_Start < window_end & df$Time_End > window_start)
    
    rolling_avg[i] <- if (base::length(idx) > 0) {
      mean(df$preds[idx], na.rm = TRUE)
    } else {
      NA_real_
    }
  }
  
  df$rolling_avg <- rolling_avg
  df$rolling_avg_prediction <- as.factor(as.numeric(df$rolling_avg > threshold))
  
  return(df)
}

df <- data.frame(
  Time_Start = seq(0, 0.4, by = 0.1),
  Time_End = seq(0.1, 0.5, by = 0.1),
  preds = rbinom(5, 1, 0.5)
)

df

result <- rolling_avg_df(df, window_length = 0.3, threshold = 0.5)
result


library(dplyr)

creating_prediction_windows <- function(data, time_start, time_end, value_col) {
  
  data %>%
    arrange({{ time_start }}, {{ time_end }}) %>%
    mutate(
      run_id = cumsum(
        {{ value_col }} != lag({{ value_col }}, default = first({{ value_col }}))
      )
    ) %>%
    filter({{ value_col }} == 1) %>%
    group_by(run_id) %>%
    summarise(
      start_time = first({{ time_start }}),
      end_time   = last({{ time_end }}),
      length     = n(),
      .groups = "drop"
    )
}


# df <- data.frame(
#   Time_Start = seq(0, 4.9, by = 0.1),
#   Time_End = seq(0.1, 5, by = 0.1),
#   preds = rbinom(50, 1, 0.5)
# )
# 
# df
# 
# creating_prediction_windows(df, Time_Start, Time_End, preds)

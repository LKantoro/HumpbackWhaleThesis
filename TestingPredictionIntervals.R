library(dplyr)
library(tidyr)

confusion_from_intervals <- function(data_true, data_pred,
                                     true_interval_start, true_interval_end,
                                     pred_interval_start, pred_interval_end) {
  
  true_df <- data_true %>%
    mutate(true_id = row_number()) %>%
    rename(
      true_start = {{ true_interval_start }},
      true_end   = {{ true_interval_end }}
    )
  
  pred_df <- data_pred %>%
    mutate(pred_id = row_number()) %>%
    rename(
      pred_start = {{ pred_interval_start }},
      pred_end   = {{ pred_interval_end }}
    )
  
  # Handle edge cases
  if (nrow(true_df) == 0 && nrow(pred_df) == 0) {
    return(tibble(TP = 0, FP = 0, FN = 0))
  }
  if (nrow(true_df) == 0) {
    return(tibble(TP = 0, FP = nrow(pred_df), FN = 0))
  }
  if (nrow(pred_df) == 0) {
    return(tibble(TP = 0, FP = 0, FN = nrow(true_df)))
  }
  
  overlaps <- expand_grid(true_df, pred_df) %>%
    filter(
      true_start < pred_end,   # non-inclusive boundary
      true_end   > pred_start
    )
  
  true_hit <- unique(overlaps$true_id)
  pred_hit <- unique(overlaps$pred_id)
  
  TP <- length(true_hit)
  FP <- nrow(pred_df) - length(pred_hit)
  FN <- nrow(true_df) - length(true_hit)
  
  data.frame(
    TP = TP,
    FP = FP,
    FN = FN
  )
}



true_intervals <- tibble(dummy = c(0,0,0),
  start_time = c(2, 8, 14),
  banana   = c(5, 10, 15)
  
)

pred_intervals <- tibble(
  start_time = c(1),
  end_time   = c(30)
)

metrics = confusion_from_intervals(
  true_intervals,
  pred_intervals,
  start_time, banana,
  start_time, end_time
)

(metrics)

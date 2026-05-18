testing_eval_windows_linear_base = function(predictions, length) {
  
  n = nrow(predictions)
  if (n == 0) {
    return(data.frame(
      Time_Start = numeric(0),
      Time_End = numeric(0),
      proportion = numeric(0),
      Humpback = integer(0)
    ))
  }
  
  time_start = predictions$Time_Start
  time_end   = predictions$Time_End
  pred_flag  = as.integer(predictions$preds == 1)
  whale_flag = as.integer(predictions$Humpback == 1)
  
  maxtime = max(time_end)
  
  rows = vector("list", n)
  iter = 0L
  
  j = 1L
  count_preds1 = 0L
  count_whales = 0L
  
  for (k in seq_len(n)) {
    i = time_start[k]
    window_end = i + length
    
    if (window_end > maxtime) {
      break
    }
    
    while (j <= n && time_start[j] < window_end) {
      count_preds1 = count_preds1 + pred_flag[j]
      count_whales = count_whales + whale_flag[j]
      j = j + 1L
    }
    
    window_size = j - k
    proportion = if (window_size > 0L) count_preds1 / window_size else NA_real_
    humpback_present = as.integer(count_whales >= 1L)
    
    iter = iter + 1L
    rows[[iter]] = data.frame(
      Time_Start = i,
      Time_End = window_end,
      proportion = proportion,
      Humpback = humpback_present
    )
    
    if (k < j) {
      count_preds1 = count_preds1 - pred_flag[k]
      count_whales = count_whales - whale_flag[k]
    }
  }
  
  do.call(rbind, rows[seq_len(iter)])
}
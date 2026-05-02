initializing_names <- function(numcep, num_time_steps) {
  base_names <- paste0(
    "V",
    rep(seq_len(numcep), each = num_time_steps),
    "_",
    rep(seq_len(num_time_steps), times = numcep)
  )
  
  extra_names <- c("Time_Start", "Time_End", "Humpback")
  
  all_names <- c(base_names, extra_names)
  
  
  return(all_names)
}

################################################################################


add_observations_linear = function(whaledf, start_time, end_time,
                                   num_time_steps = 40, numcep = 12, 
                                   step_size, jump_size = 0.1) {
  
  whaledf = whaledf %>%
    dplyr::filter(start_time <= Time_Start & Time_Start < end_time) %>%
    dplyr::arrange(Time_Start)
  
  if (nrow(whaledf) == 0) {
    stop("No data in specified range")
  }
  
  if (start_time < min(whaledf$Time_Start) || max(whaledf$Time_End) < end_time) {
    stop("Specified time range is not within data time range")
  }
  
  n = nrow(whaledf)
  window_len = jump_size * num_time_steps
  
  # cumulative whale counts
  whale_flag = as.integer(whaledf$Humpback == 1)
  whale_csum = c(0L, cumsum(whale_flag))
  
  # coefficient matrix once
  coeff_mat = as.matrix(whaledf[paste0("V", 1:numcep)])
  
  rows = vector("list", n)
  iter = 0L
  j = 1L
  
  for (k in seq_len(n)) {
    i = whaledf$Time_Start[k]
    window_end = i + window_len
    
    if (window_end > end_time) {
      break
    }
    
    # advance right pointer only forward
    while (j <= n && whaledf$Time_Start[j] < window_end) {
      j = j + 1L
    }
    
    # rows in current window are k:(j-1)
    window_size = j - k
    
    # need enough rows to form num_time_steps
    if (window_size < num_time_steps) {
      next
    }
    
    iter = iter + 1L
    
    whale = as.integer((whale_csum[j - 1L + 1L] - whale_csum[k]) > 0)
    
    coeffs = coeff_mat[k:(k + num_time_steps - 1L), , drop = FALSE]
    
    rows[[iter]] = c(as.vector(coeffs), i, window_end, whale)
  }
  results = do.call(rbind, rows)
  
  column_names = initializing_names(numcep, num_time_steps)
  
  colnames(results) = column_names
  
  results = results %>% data.frame() %>%
    drop_na() %>% mutate(Humpback = as.factor(Humpback))
  
  #need to create conditionals for end of file
  
  return(results)
}
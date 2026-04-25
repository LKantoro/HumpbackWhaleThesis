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

#function for specifically overlapped time steps (0.5 second increases, 0.5 second overlap)


# dummydf is the empty df created in the above 'create_observations' function
# whaledf is the full mel coefficient data frame
# num_time_steps is the number of time steps 
# numcep is the number of cepstrum coefficients
# start_time (seconds)
# end_time (seconds)
# step size is the number of steps in a context window


add_observations = function(whaledf, start_time, end_time,
                            num_time_steps = 40, numcep = 12, step_size){
  
  rows = vector("list", 80000)
  
  # subsetting on only the observed range
  whaledf = whaledf %>% 
    filter(start_time <= Time_Start & Time_Start < end_time)
  
  
  if (start_time < min(whaledf$Time_Start) || max(whaledf$Time_End) < end_time) {
    stop("Specified time range is not within data time range")
  }
  
  iter = 0
  
  # filtering on initial times
  initials = whaledf %>% pull(Time_Start)
  # for each initial row in which an observation is desired
  for (i in initials){
    
    
    #i represents the start time of an initial observation (context window)
    
    # create observation by looping over the number of time steps
    
    #if statement to avoid including observations at end of the file
    
    if (i + step_size*num_time_steps <= end_time)
    {
      iter = iter + 1
      
      window = whaledf %>% 
        filter(i <= Time_Start & Time_Start< i + step_size*num_time_steps)
      
      # determining whether the window has any whale call
      if (nrow(window %>%
               filter(Humpback == 1)) >= 1){
        
        whale = 1
      } else {
        whale = 0
      }
      
      coeffs = window %>% 
        dplyr::select(V1:paste0("V", numcep)) %>% slice(1:num_time_steps)
      
      rows[[iter]] = c(as.matrix(coeffs), i, i + step_size*num_time_steps, whale)
      }
    else {
      break
    }
    }
    results = do.call(rbind, rows)
    
    column_names = initializing_names(numcep, num_time_steps)
    
    colnames(results) = column_names
    
    results = results %>% data.frame() %>%
      drop_na() %>% mutate(Humpback = as.factor(Humpback))
    
    #need to create conditionals for end of file

    return(results)

}


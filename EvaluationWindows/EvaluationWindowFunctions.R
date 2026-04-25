# predictions is a data frame with time start, time end, prediction, Humpback
# length is the length of each evaluation window
# threshold is the proportion of positives required for the threshold window


testing_eval_windows = function(predictions, length, threshold){
  
  #initials = creating_eval_windows(predictions, length)
  initials = predictions %>% pull(Time_Start)
  
  rows <- vector("list", nrow(predictions))
  
  #init_matrix = matrix(nrow = 0, ncol = 5)
  
  #create data frame with state time, end time, proportion, 
  #and 0/1 based on prop vs threshold, and truth
  
  #results = data.frame(init_matrix)
  
  maxtime = max(predictions %>% pull(Time_End))
  iter = 0
  for (i in initials){
    #print(predictions %>% filter(i <= Time_Start & Time_Start < i + length))
    if (i + length > maxtime){
      break
    } else {
    
    iter = iter + 1
    window = predictions %>% filter(i <= Time_Start & Time_Start < i + length)

      
    rows[[iter]] = data.frame("Time_Start" = i, 
            "Time_End" = i + length, 
            "proportion" = window %>% 
              summarise(proportion = mean(preds == 1)) %>% pull(proportion), 
            "prediction" = NA,
            "Humpback" = as.numeric(nrow(window %>% 
                                           filter(Humpback == 1)) >= 1))
    
    }
    
      
    
    #print(row)
    #results = rbind(results, row)
    }
  
  results = do.call(rbind, rows)
  #print(class(results))
  colnames(results) = c("Time_Start", "Time_End", 
                        "proportion", "prediction", "Humpback")
  results = results %>% 
    mutate(prediction = as.factor(as.numeric(proportion >= threshold)),
                              Humpback = as.factor(Humpback))
  return(results)
  
  
  
  
  # end observation at start + length
  # if number of time steps that predicted a whale is >= proportion
  # make this observation a 1
  # else: make it 0
  # start new observation at next time step
  # repeat until end of data file
  
  
  
  
  
}
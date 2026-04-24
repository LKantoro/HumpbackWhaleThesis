# Creating Evaluation Windows

# predictions is a data frame with time start, time end, prediction, and Humpback
# length is the length of each evaluation window
creating_eval_windows = function(predictions, length){
  
  stepsize = predictions %>% pull(Time_End) %>% first() - 
    predictions %>% pull(Time_Start) %>% first()
  
  initial = predictions %>% pull(Time_Start) %>% first()
  final = predictions %>% pull(Time_Start) %>% last()
  initials = c()
  starttime = initial
  
  while (starttime + length <= final){
    
    newtime = starttime
    initials = c(initials, newtime)
    starttime = starttime + stepsize
    
  }
  return(initials)
  
}

#creating_eval_windows(testingpredictions, 1)




################################################################################


# predictions is a data frame with time start, time end, prediction, Humpback
# length is the length of each evaluation window
# threshold is the proportion of positives required for the threshold window


testing_eval_windows = function(predictions, length, threshold){
  
  initials = creating_eval_windows(predictions, length)
  
  init_matrix = matrix(nrow = 0, ncol = 5)
  
  #create data frame with state time, end time, proportion, 
  #and 0/1 based on prop vs threshold, and truth
  
  results = data.frame(init_matrix)
  
  for (i in initials){
    #print(predictions %>% filter(i <= Time_Start & Time_Start < i + length))
    

      
    row = c("Time_Start" = i, 
            "Time_End" = i + length, 
            "proportion" = predictions %>% 
              filter(i <= Time_Start & Time_Start < i + length) %>% 
              summarise(proportion = sum(preds == 1) /n()) %>% pull(proportion), 
            "prediction" = as.numeric((predictions %>% 
                                         filter(i <= Time_Start & 
                                                  Time_Start < i + length) %>% 
                                         summarise(proportion = 
                                                     sum(preds == 1) / n()) %>% 
                                         pull(proportion) > threshold)),
            "Humpback" = as.numeric(nrow(predictions %>% 
                                           filter(i <= Time_Start & 
                                                    Time_Start < i + length) %>% 
                                           filter(Humpback == 1)) >= 1))
      

    
      
    
    #print(row)
    results = rbind(results, row)
    }
  colnames(results) = c("Time_Start", "Time_End", 
                        "proportion", "prediction", "Humpback")
  results = results%>% mutate(prediction = as.factor(prediction),
                              Humpback = as.factor(Humpback))
  return(results)
  
  
  
  
  # end observation at start + length
  # if number of time steps that predicted a whale is >= proportion
  # make this observation a 1
  # else: make it 0
  # start new observation at next time step
  # repeat until end of data file
  
  
  
  
  
}
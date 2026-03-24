

# input contains: (ldapredictions, manualannotations)
# time_start
# time_end
# prediction (0/1)
# truth (0/1)
# length of rolling evaluation windows




# manual_annotations: the observed manual annotation file
# predictions: a file of predictions on each time segment
# pred_start_time: the time in the annotated file when predictions begin
# pred_end_time: the time in the annotated file when predictions begin
# threshold: the lower limit proportion of encapsulating windows that predict 1 
# for the overall prediction to be 1


custom_denominator = function(manual_annotations, predictions, 
                              pred_start_time, pred_end_time, threshold){
  # pseudocode:
  outputdf = data.frame()
  
  # formatting and filtering annotation dataset for easier coding
  manual_annotations = manual_annotations %>% 
    rename(Begin_Time = 'Begin Time (s)', End_Time = 'End Time (s)') %>%
    filter(Begin_Time >= pred_start_time & End_Time <= pred_end_time)

  # for each annotated range
  for (i in c(1:nrow(manual_annotations))){
    
    # taking the start and end time of each annotated whale
    start_time_annotated = manual_annotations$Begin_Time[i]
    end_time_annotated = manual_annotations$End_Time[i]

    
    # if beginning of 0.1 second window is less than end of annotated window
    # and if end of 0.1 window is greater than beginning of annotated window
    # calculate the proportion of 1s
    proportion = predictions %>% filter(Time_Start < end_time_annotated &
                                start_time_annotated < Time_End) %>% 
      summarise(proportion = sum(preds == 1) / n()) %>% pull(proportion)
    
    # predicting a whale if the proportion is greater than the threshold
    prediction = as.numeric(proportion >= threshold)
    
    row = c("Time_Start" = start_time_annotated, 
            "Time_End" = end_time_annotated, 
            "proportion" = proportion, 
            "prediction" = prediction)
    
    # adding a row with start_time, end_time, and proportion to output
    outputdf = rbind(outputdf, row)
  }
  colnames(outputdf) = c("Time_Start", "Time_End", "proportion", "prediction")
  
  # output 
  return(outputdf)
}


  
  










# Create a custom denominator that counts a success based on the number of whale calls found, instead of the number of windows with a whale call.


#If at any point in the whale call, there is a prediction, count that as a success, if there are no predictions on a whale call, count that as a failure.

#How to judge incorrect predictions using this metric?
  
  
# take all true annotations
# round both ends to nearest 0.05
# determine the percentage of positive predictions out of segments that overlap it
# record that metric and compare it to a threshold



# input contains: (ldapredictions, manualannotations)
# time_start
# time_end
# prediction (0/1)
# truth (0/1)
# length of rolling evaluation windows


manual_annotations = read_excel(here::here("TrainingData", "AW", "AWhb.xlsx"))

manual_annotations = manual_annotations %>% rename(Begin_Time = 'Begin Time (s)',
                                 End_Time = 'End Time (s)')


custom_denominator = function(){
  # pseudocode:
  outputdf = data.frame()
  # for each annotated range
  for (i in c(1:nrow(manual_annotations))){
    #print(i)
    
    start_time_annotated = manual_annotations$Begin_Time[i]
    end_time_annotated = manual_annotations$End_Time[i]
    
    
    proportion = ldapredictions %>% filter(Time_Start < end_time_annotated &
                                start_time_annotated < Time_End) %>% 
      summarise(proportion = sum(preds == 1) / n()) %>% pull(proportion)
    
    prediction = as.numeric(proportion > threshold)
    
    row = c("Time_Start" = i, 
            "Time_End" = i + length, 
            "proportion" = proportion, 
            "prediction" = prediction)
  }
  colnames(outputdf) = c("Time_Start", "Time_End", "proportion", "prediction")
  
}

  # if beginning of 0.1 second window is less than end of annotated window
  # and if end of 0.1 window is greater than beginning of annotated window
    # investigate whether prediction is 0 or 1
  # calculate the proportion of 1s
  # add row with start_time, end_time, and proportion to output
# output 






annotatedAW = read_excel(here::here("TrainingData", "AW", "AWhb.xlsx"))



annotatedAW = annotatedAW %>% dplyr::rename("begin_time" = "Begin Time (s)",
                                            "end_time" = "End Time (s)") %>%
  mutate(begin_time = round_any(begin_time, accuracy = 0.05, f = round),
         end_time = round_any(end_time, accuracy = 0.05, f = round))


annotatedAW




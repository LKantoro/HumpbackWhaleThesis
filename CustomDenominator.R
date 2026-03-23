
# Create a custom denominator that counts a success based on the number of whale calls found, instead of the number of windows with a whale call.


#If at any point in the whale call, there is a prediction, count that as a success, if there are no predictions on a whale call, count that as a failure.

#How to judge incorrect predictions using this metric?
  
  
annotatedAW = read_excel(here::here("TrainingData", "AW", "AWhb.xlsx"))



annotatedAW = annotatedAW %>% dplyr::rename("begin_time" = "Begin Time (s)",
                                            "end_time" = "End Time (s)") %>%
  mutate(begin_time = round_any(begin_time, accuracy = 0.05, f = round),
         end_time = round_any(end_time, accuracy = 0.05, f = round))


annotatedAW



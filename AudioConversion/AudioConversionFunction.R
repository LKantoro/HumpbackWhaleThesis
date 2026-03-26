# function with: 
# wave file, 
# start time (minutes)
# end time (minutes)
# step time (seconds)
# overlap time (seconds) 
# number of mel coefficients (default 12)


melcoeffs = function(wave_file, start_time, end_time, step_time, overlap_time,
                     num_ceps){
  
  # reading in wave file
  audio_data = readWave(wave_file, from = start_time, to = end_time, 
                        units = "minutes")
  
  
  # Extracting Raw Audio Samples
  raw_samples = audio_data@left #mono data has identical left and right values
  
  # Computing Mel Coefficients
  melcoeffs = melfcc(audio_data, wintime = step_time, hoptime = overlap_time, 
                       numcep = num_ceps, bwidth = 1)
  
  # Adding Time Steps to Matrix
  #17999 is for 30 minutes
  timesteps = as.data.frame(cbind(melcoeffs, 
                                    "Time_Start" = seq(from = start_time * 60, to = end_time * 60 - step_time - overlap_time, by = overlap_time),
                                    "Time_End" = seq(from = start_time * 60  + step_time, to = end_time * 60 - overlap_time, by = overlap_time)))
  return(timesteps)
}





# test2 = melcoeffs(wave_file = here::here("TrainingData", "AW", "671658014.180928183606-AW.wav"),
#                  start_time = 0,
#                  end_time = 0.005,
#                  step_time = 0.1,
#                  overlap_time = 0.05,
#                  num_ceps = 12)


# 
# audio_data = readWave(here::here("TrainingData", "AW", 
#                                  "671658014.180928183606-AW.wav"),
#                       from = 0, to = 30, units = "minutes")
# 
# raw_samples = audio_data@left #mono data has identical left and right values
# 
# melcoeffs = melfcc(audio_data, wintime = 0.1, hoptime = 0.05, 
#                    numcep = 12, bwidth = 1)
# nrow(melcoeffs) # 35998 rows
# 
# timesteps = as.data.frame(cbind(melcoeffs, 
#                     "Time_Start" = seq(from = 0, to = 1799.85, by = 0.05),
#                     "Time_End" = seq(from = .05, to = 1799.9, by = 0.05)))
# 
# 
# length(seq(from = 0, to = 1799.9, by = 0.05))



################################################################################


import_annotations = function(annotated_file_path){
  
  # Reading in a Training Data Set
  annotated = read_excel(annotated_file_path)
  
  
  
  # Cleaning Training Data Set
  annotated = annotated %>% rename(Begin_Time = 'Begin Time (s)',
                                       End_Time = 'End Time (s)')
  
  # Extracting Ranges of Calls
  # list format
  whaleranges = apply(cbind(annotated$Begin_Time, annotated$End_Time)
                        , 1, list)
  
  return(whaleranges)
  
}



# ranges = import_annotations(here::here("TrainingData", "AW", "AWhb.xlsx"))
# 
# read_excel(here::here("TrainingData", "AW", "AWhb.xlsx"))




################################################################################



#' Checks if a number falls within any of a list of specified ranges.
#'
#' @param number The numeric value to check.
#' @param ranges A list of numeric vectors, where each vector represents a range
#'   with two elements: `c(start, end)`.
#' @return A logical value: `TRUE` if the number is within any range, `FALSE` otherwise.
#' @examples
#' check_in_ranges(5, list(c(1, 10), c(15, 20))) # Returns TRUE
#' check_in_ranges(12, list(c(1, 10), c(15, 20))) # Returns FALSE
#' check_in_ranges(18, list(c(1, 10), c(15, 20))) # Returns TRUE
#' 
#' 
check_in_ranges <- function(number, ranges, timesteplength = 0.1) {
  # Initialize a flag to track if the number is found within any range
  found <- FALSE
  
  #print(ranges)
  # Iterate through each range in the list
  for (r in 1:length(ranges)) {
    #print(r)
    #print(ranges[[r]][[1]][2])
    # Check if the number is within the current range (inclusive)
    if (number <= ranges[[r]][[1]][2] && 
        number + timesteplength >= ranges[[r]][[1]][1]) {
      found <- TRUE
      break # Exit the loop as soon as a match is found
    }
  }
  
  return(c(number, found))
}



# applying ranges function to timestep
# overlapstart = lapply(timesteps$Time_Start, check_in_ranges, ranges = ranges, 
#                       timesteplength = 0.1)


################################################################################

## Merging Output of Function With Mel Coefficient Datasets



creating_observation_dataframe = function(overlaps, timesteps){
   
   startmatcheddf = as.data.frame(t(as.data.frame(overlaps))) %>%
     rename(Time_Start = V1, Humpback = V2)
   
   
   annotatedfull = full_join(timesteps, startmatcheddf, 
                               by = join_by(Time_Start == Time_Start))
   
   return(annotatedfull)
   
 }


# annotatedfull = creating_observation_dataframe(overlaps = overlapstart,
#                                                timesteps = timesteps)




###############################################################################

# Adding Instance Numbers

add_instances =  function(dataframe){
  
  
  #
  dataframe$instances = rep(0, times = nrow(dataframe))
  dataframe$initial = rep(0, times = nrow(dataframe))
  
  #recording successive positive/negative observations
  dataframe$streak = rep(0, times = nrow(dataframe))
  
  instances = 0
  
  for (i in 1:nrow(dataframe)) {
    # Access the current row using df[i]
    current = dataframe$Humpback[i]
    
    
    #if on the first row, assume the previous was 0
    if (i == 1) {
      previous = 0
    }
    
    #if not on first row, set previous value to use as comparison
    if (i > 1) {
      previous = dataframe$Humpback[i - 1]
      
    }
    
    #if previous has no whale call and current has whale call
    if (previous == 0 & current == 1){
      #create new instance since it is a new whale call
      instances = instances + 1
      dataframe$instances[i] = instances
      
      #indiciate that this is the initilization of a new whale call
      dataframe$initial[i] = 1
      
    }
    
    #if previous has a whale call and current also has a whale call
    if (previous == 1 & current == 1){
      dataframe$instances[i] = instances
    }
  }
  
  return(dataframe)
}

#annotatedfull = add_instances(annotatedfull)



################################################################################


  
  
  
  

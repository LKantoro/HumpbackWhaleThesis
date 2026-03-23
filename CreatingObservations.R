
#numcep is the number of mel coefficients used
#num_time_steps is the number of time steps recorded in an observation

#create_observations takes in the above parameters,
#outputs a data frame with num_time_steps*numcep columns, 
#and 'Time_Start', 'Time_End' and Humpback (0/1) columns

create_observations = function(numcep, num_time_steps){
  #testdf = data.frame(nrow = num_time_steps*numcep)
  #print(testdf)
  colnamelist = list()
  for (i in 1:numcep){
    for (j in 1:num_time_steps){
      #print(i)
      #print(j)
      #testdf = cbind(testdf, paste0("V1_",i))
      colnamelist = append(colnamelist, paste0("V", i, "_", j))
    }
  }
  #adding Time_Start and Time_End variables to end of each row
  colnamelist = append(colnamelist, "Time_Start")
  colnamelist = append(colnamelist, "Time_End")
  colnamelist = append(colnamelist, "Humpback")
  
  
  dummydf = data.frame(matrix(nrow = 1, ncol = num_time_steps*numcep + 3))
  
  colnames(dummydf) = colnamelist
  
  
  
  return(dummydf)
  
}

###############################################################################


#function for specifically overlapped time steps 
#(0.05 second increases, 0.05 second overlap)

# dummydf is the empty df created in the above 'create_observations' function


# whaledf is the full mel coefficient data frame
# num_time_steps is the number of time steps 
# numcep is the number of cepstrum coefficients

add_observations = function(whaledf, num_time_steps, numcep){
  
  dummydf = create_observations(numcep, num_time_steps)
  
  #getting initial times
  #filtering on initial times
  initials = whaledf %>% pull(Time_Start)
  
  # for each initial row in which an observation is desired
  for (i in initials){

    #i represents the start time of an initial observation (context window)
    
    #create observation by looping over the number of time steps
    
    #if statement to avoid including observations at end of the file
    if (nrow(whaledf %>% 
             filter(i <= Time_Start & Time_Start < i + 0.05*num_time_steps)) == 
        num_time_steps)
    {
      # determining whether the window has any whale call
      if (nrow(whaledf %>% 
               filter(i <= Time_Start & Time_Start < i + 0.05*num_time_steps) %>%
               filter(Humpback == 1)) >= 1){
        
        whale = 1
      } else {
        whale = 0
      }
      
      coeffs = whaledf %>% 
        filter(i <= Time_Start & Time_Start < i + 0.05*num_time_steps) %>% 
        dplyr::select(V1:paste0("V", numcep))
      
      
      #c(as.matrix()) creates a vector out of a data.frame, going down columns
      dummydf = rbind(dummydf, 
                      c(as.matrix(coeffs), i, i + 0.05*num_time_steps, whale))
    }
    
    
    #need to create conditionals for end of file
  }
  return(dummydf)
}


################################################################################


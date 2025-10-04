# function for OWSA

f_OWSA <- function(params, n_change) {

# Initialize an empty data frame to store results
result_df <- data.frame(parameter = character(),
                        icer_10 = numeric(),
                        icer_6 = numeric(),
                        icer_4 = numeric(),
                        icer_range = numeric(),
                        stringsAsFactors = FALSE)


# Loop through parameters
for (i in names(params)) {  # Use names(params) instead of params
  # Store original parameter value
  original_value <- params[[i]]
  
  # Modify parameter
  params[[i]] <- params[[i]] *n_change
  
  # Run model
  a <- f_run_models(params)
  
  # Calculate ICER range
  icer_range <- max(a$ICER) - min(a$ICER)
  
  # Add to results data frame
  result_df <- rbind(result_df, data.frame(parameter = i, 
                                           icer_10= a[1,8],
                                           icer_6 = a[2,8],
                                           icer_4 = a[3,8],
                                           icer_range = icer_range))
  
  # Reset parameter to original value 
  params[[i]] <- original_value
}
return(result_df)

}







f_run_models <- function(params){
  
  
  #10 markov model results
  
  results_comp_10 <- f_10_health_state_model(params = params)
  
  results_int_10 <- f_10_health_state_model(params = params,
                                            apply_RR = 1,
                                            c_int = params["c_int"])
  #6 markov mdoel results
  
  results_comp_6 <- f_6_health_state_model(params = params)
  
  
  results_int_6 <- f_6_health_state_model(params = params,
                                          apply_RR = 1,
                                          c_int = params["c_int"])
  
  # 4 markov model results
  
  results_comp_4 <- f_4_health_state_model(params = params)
  
  results_int_4 <- f_4_health_state_model(params = params,
                                          apply_RR = 1,
                                          c_int = params["c_int"])
  
  # incremental costs
  
  # Calculate incremental results for 10-state model
  inc_cost_10 <- results_int_10$results[1] - results_comp_10$results[1]
  inc_qaly_10 <- results_int_10$results[2] - results_comp_10$results[2]
  icer_10 <- inc_cost_10 / inc_qaly_10
  
  # Calculate incremental results for 6-state model
  inc_cost_6 <- results_int_6$results[1] - results_comp_6$results[1]
  inc_qaly_6 <- results_int_6$results[2] - results_comp_6$results[2]
  icer_6 <- inc_cost_6 / inc_qaly_6
  
  # Calculate incremental results for 4-state model
  inc_cost_4 <- results_int_4$results[1] - results_comp_4$results[1]
  inc_qaly_4 <- results_int_4$results[2] - results_comp_4$results[2]
  icer_4 <- inc_cost_4 / inc_qaly_4
  
  # Create a summary table of results
  results_summary <- data.frame(
    Model = c("10-state", "6-state", "4-state"),
    Comp_Cost = c(results_comp_10$results[1], results_comp_6$results[1], results_comp_4$results[1]),
    Int_Cost = c(results_int_10$results[1], results_int_6$results[1], results_int_4$results[1]),
    Inc_Cost = c(inc_cost_10, inc_cost_6, inc_cost_4),
    Comp_QALY = c(results_comp_10$results[2], results_comp_6$results[2], results_comp_4$results[2]),
    Int_QALY = c(results_int_10$results[2], results_int_6$results[2], results_int_4$results[2]),
    Inc_QALY = c(inc_qaly_10, inc_qaly_6, inc_qaly_4),
    ICER = c(icer_10, icer_6, icer_4)
  )
  
  # Print the summary table
  return(results_summary)
  
}

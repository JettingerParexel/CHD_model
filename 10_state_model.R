f_10_health_state_model <- function(
    params,
    apply_RR= 0,
    c_int = 0) {
  
  
  t <- params["t"] # cycle length
  n_cycles <- params["n_cycles"]
  n_cohort <- params["n_cohort"]
  
  # Extract parameters from the params list/vector
  n_py <- params["n_py"]
  n_isch_stroke <- params["n_isch_stroke"]
  n_haem_stroke <- params["n_haem_stroke"]
  n_unhosp_hf <- params["n_unhosp_hf"]
  n_hosp_hf <- params["n_hosp_hf"]
  
  mr_well <- params["mr_well"]
  smr_isch_stroke <- params["smr_isch_stroke"]
  smr_haem_stroke <- params["smr_haem_stroke"]
  smr_unhosp_hf <- params["smr_unhosp_hf"]
  smr_hosp_hf <- params["smr_hosp_hf"]
  
  c_isch_stroke <- params["c_isch_stroke"]
  c_haem_stroke <- params["c_haem_stroke"]
  c_unhosp_hf <- params["c_unhosp_hf"]
  c_hosp_hf <- params["c_hosp_hf"]
  
  c_monitoring <- params["c_monitoring"]
  c_post_isch_stroke <- params["c_post_isch_stroke"]
  c_post_haem_stroke <- params["c_post_haem_stroke"]
  c_post_hosp_hf <- params["c_post_hosp_hf"]
  c_post_unhosp_hf <- params["c_post_unhosp_hf"]
  
  u_well <- params["u_well"]
  u_multi_isch_stroke <- params["u_multi_isch_stroke"]
  u_multi_haem_stroke <- params["u_multi_haem_stroke"]
  u_multi_unhosp_hf <- params["u_multi_unhosp_hf"]
  u_multi_hosp_hf <- params["u_multi_hosp_hf"]
  
  u_multi_post_isch_stroke <- params["u_multi_post_isch_stroke"]
  u_multi_post_haem_stroke <- params["u_multi_post_haem_stroke"]
  u_multi_post_unhosp_hf <- params["u_multi_post_unhosp_hf"]
  u_multi_post_hosp_hf <- params["u_multi_post_hosp_hf"]
  

  Ne.int_isch_stroke <- params["Ne.int_isch_stroke"]
  Ne.int_haem_stroke<-params["Ne.int_haem_stroke"]
  Ne.int_hosp_hf<-params["Ne.int_hosp_hf"] 
  Ne.int_unhosp_hf<-params["Ne.int_unhosp_hf"]
  Ne.comp_isch_stroke<-params["Ne.comp_isch_stroke"]
  Ne.comp_haem_stroke<-params["Ne.comp_haem_stroke"] 
  Ne.comp_hosp_hf<-params["Ne.comp_hosp_hf"]
  Ne.comp_unhosp_hf<-params["Ne.comp_unhosp_hf"]
  n_trial_people<-params["n_trial_people"] 
  
  #treatment effect
  
  v_events <- c("isch_stroke", "haem_stroke", "hosp_hf", "unhosp_hf")
  
  v_trt_effect_columns <- c("Ne.Int", "Ne.Comp", "Risk.Int", "Risk.Comp", "Rr") 

  # Create the dataframe directly
  df_trt_effect_data <- data.frame(
    "Ne.Int" = c(Ne.int_isch_stroke, 
                 Ne.int_haem_stroke,
                 Ne.int_hosp_hf,
                 Ne.int_unhosp_hf),
    "Ne.Comp" = c(Ne.comp_isch_stroke, 
                  Ne.comp_haem_stroke,
                  Ne.comp_hosp_hf,
                  Ne.comp_unhosp_hf),
    "Risk.Int" = c(0, 0, 0, 0),
    "Risk.Comp" = c(0, 0, 0, 0),
    "RR" = c(0, 0, 0, 0),  # Assuming this is the fifth column
    row.names = v_events
  )
  
  
  # calculate relative risks 
  
Risk.Int <- df_trt_effect_data$Ne.Int/n_trial_people

Risk.Comp <- df_trt_effect_data$Ne.Comp/n_trial_people
  
  RR <- if(apply_RR == 1) { Risk.Int / Risk.Comp} else
               {c(1,1,1,1)}
  
    # calculate annualised event rates
  
  r_isch_stroke <- n_isch_stroke/ n_py 
  r_haem_stroke <- n_haem_stroke / n_py  
  r_unhosp_hf <- n_unhosp_hf / n_py   
  r_hosp_hf <- n_hosp_hf / n_py   
  
  
  # Transition probabilities
  p_isch_stroke <- (1 - exp(-r_isch_stroke * t)) *RR[1]
  p_haem_stroke <- (1 - exp(-r_haem_stroke * t)) *RR[2]
  p_hosp_hf <- (1 - exp(-r_hosp_hf * t)) *RR[3]
  p_unhosp_hf <- (1 - exp(-r_unhosp_hf * t)) *RR[4]
  p_death <- 1 - exp(-mr_well * t)
  
  p_well <- 1 - (p_isch_stroke + p_haem_stroke + p_hosp_hf + p_unhosp_hf + p_death)
  


  
  # Define the health states
  list_states_10 <- c("Well", "isch_stroke", "haem_stroke", 
                      "hosp_hf", "unhosp_hf",
                      "post_isch_stroke", "post_haem_stroke",
                      "post_hosp_hf", "post_unhosp_hf",
                      "Dead")
  
  # Create a transition matrix
  transition_matrix_10 <- matrix(c(
    p_well, p_isch_stroke, p_haem_stroke, p_hosp_hf, p_unhosp_hf, 0, 0, 0, 0, p_death, # Well
    0, 0, 0, 0, 0, 1, 0, 0, 0, p_death * smr_isch_stroke, # Ischaemic Stroke (tunnel)
    0, 0, 0, 0, 0, 0, 1, 0, 0, p_death * smr_haem_stroke, # Haemorrhagic Stroke (tunnel)
    0, 0, 0, 0, 0, 0, 0, 1, 0, p_death * smr_hosp_hf, # Hospitalised HF (tunnel)
    0, 0, 0, 0, 0, 0, 0, 0, 1, p_death * smr_unhosp_hf, # Non-Hospitalised HF (tunnel)
    0, p_isch_stroke, p_haem_stroke, p_hosp_hf, p_unhosp_hf, 0, 0, 0, 0, p_death, # Post Ischaemic Stroke
    0, p_isch_stroke, p_haem_stroke, p_hosp_hf, p_unhosp_hf, 0, 0, 0, 0, p_death, # Post Haemorrhagic Stroke
    0, p_isch_stroke, p_haem_stroke, p_hosp_hf, p_unhosp_hf, 0, 0, 0, 0, p_death, # Post Hospitalised HF
    0, p_isch_stroke, p_haem_stroke, p_hosp_hf, p_unhosp_hf, 0, 0, 0, 0, p_death, # Post Non-Hospitalised HF
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1 # Dead (absorbing state)
  ), nrow = length(list_states_10), byrow = TRUE)
  
  rownames(transition_matrix_10) <- list_states_10
  colnames(transition_matrix_10) <- list_states_10
  
  # Prob of going from disease health state to death 
  transition_matrix_10[2,6] <- 1 - transition_matrix_10[2,10]
  transition_matrix_10[3,7] <- 1 - transition_matrix_10[3,10]
  transition_matrix_10[4,8] <- 1 - transition_matrix_10[4,10]
  transition_matrix_10[5,9] <- 1 - transition_matrix_10[5,10]
  
  # Calculate chance of remaining in post health states 
  transition_matrix_10[6,6] <- 1 - sum(transition_matrix_10[6,])
  transition_matrix_10[7,7] <- 1 - sum(transition_matrix_10[7,])
  transition_matrix_10[8,8] <- 1 - sum(transition_matrix_10[8,])
  transition_matrix_10[9,9] <- 1 - sum(transition_matrix_10[9,])
  
  # Payoffs
  m_payoffs_10 <- matrix(c(c_monitoring, c_isch_stroke, c_haem_stroke, c_hosp_hf, c_unhosp_hf,
                           c_post_isch_stroke, c_post_haem_stroke, c_post_hosp_hf, c_post_unhosp_hf, 0,
                           u_well, u_well*u_multi_isch_stroke, u_well*u_multi_haem_stroke,
                           u_well*u_multi_hosp_hf, u_well*u_multi_unhosp_hf,
                           u_well*u_multi_post_isch_stroke, u_well*u_multi_post_haem_stroke,
                           u_well*u_multi_post_hosp_hf, u_well*u_multi_post_unhosp_hf, 0),
                         nrow = length(list_states_10), ncol = 2, byrow = FALSE,
                         dimnames = list(state = list_states_10,
                                         payoff = c("Cost", "QALY")))
  
  # State membership
  state_membership_10 <- array(NA_real_,
                               dim = c(n_cycles, length(list_states_10)),
                               dimnames = list(cycle = 1:n_cycles,
                                               state = list_states_10))
  
  state_membership_10[1,] <- c(n_cohort, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  for (i in 2:n_cycles) {
    state_membership_10[i, ] <- state_membership_10[i - 1, ] %*% transition_matrix_10
  }
  
  # Calculate results
  payoff_trace_10 <- state_membership_10 %*% m_payoffs_10
  
  # apply intervention cost
  payoff_trace_10[1,1] <- payoff_trace_10[1,1] + (c_int * n_cohort)
  
  results <- colSums(payoff_trace_10) / n_cohort
  
  # Return results and other useful objects
  return(list(
    results = results,
    state_membership = state_membership_10,
    payoff_trace = payoff_trace_10,
    transition_matrix = transition_matrix_10,
    payoffs_matrix = m_payoffs_10
  ))
}








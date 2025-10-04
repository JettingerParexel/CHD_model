f_6_health_state_model <- function(params,
                                   apply_RR = 0,
                                   c_int = 0) {
  
  
  # Extract parameters from the params list/vector
  
  t <- params["t"] # cycle length
  n_cycles <- params["n_cycles"]
  n_cohort <- params["n_cohort"]
  
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
  
  # treatment effect 
  
  df_stroke <- df_trt_effect_data[1,] + df_trt_effect_data[2,] 
  
  df_hf <- df_trt_effect_data[3,] + df_trt_effect_data[4,] 
  
  df_trt_effect_6 <- rbind(df_stroke, df_hf)
  
  df_trt_effect_6 <- df_trt_effect_6%>%
    mutate(Risk.Int = df_trt_effect_6$Ne.Int/n_trial_people,
           Risk.Comp = df_trt_effect_6$Ne.Comp/n_trial_people)

  # calculate relative risks 
  
  Risk.Int <- df_trt_effect_6$Ne.Int/n_trial_people
  
  Risk.Comp <- df_trt_effect_6$Ne.Comp/n_trial_people
  
  RR <- if(apply_RR == 1) { Risk.Int / Risk.Comp} else
  {c(1,1)}
  # calculate annualised event rates
  
    r_stroke <-(n_isch_stroke+n_haem_stroke)/ n_py   
  r_hf <-(n_hosp_hf + n_unhosp_hf)/ n_py   
  
  # 6 health state STM weights
  
  percent_isch_stroke <- n_isch_stroke / (n_isch_stroke+n_haem_stroke)
  percent_hosp_hf <- n_hosp_hf / (n_hosp_hf + n_unhosp_hf)
  
  # Transition probabilities
  p_stroke <- (1 - exp(-r_stroke * t)) *RR[1]
  p_hf <- (1 - exp(-r_hf * t)) *RR[2]
  p_death <- 1 - exp(-mr_well * t)
  p_well <- 1 - (p_stroke + p_hf + p_death)
  
  
  # Calculate SMRs

  smr_stroke <- smr_isch_stroke * percent_isch_stroke +
    smr_haem_stroke * (1- percent_isch_stroke)
  smr_hf  <-    smr_hosp_hf * percent_hosp_hf +
    smr_unhosp_hf * (1- percent_hosp_hf)
  
  # Calculate event costs
  
  c_stroke <- c_isch_stroke * percent_isch_stroke + c_haem_stroke * (1- percent_isch_stroke)
  c_hf <- c_hosp_hf * percent_hosp_hf +
    c_unhosp_hf * (1- percent_hosp_hf)
  
  # calculate post-event health state costs
  
  c_post_stroke <-  c_post_isch_stroke * percent_isch_stroke + c_post_haem_stroke * (1- percent_isch_stroke )
  c_post_hf <- c_post_hosp_hf * percent_hosp_hf + c_post_unhosp_hf * (1- percent_hosp_hf )
  
  #calculate event utility multipliers
  
  u_multi_stroke <- u_multi_isch_stroke * percent_isch_stroke + u_multi_haem_stroke * (1- percent_isch_stroke )
  u_multi_hf <- u_multi_hosp_hf * percent_hosp_hf + u_multi_unhosp_hf * (1- percent_hosp_hf )
  
  #calculate post-event utility multipliers
  u_multi_post_stroke <-  u_multi_post_isch_stroke * percent_isch_stroke +
    u_multi_post_haem_stroke * (1- percent_isch_stroke)
  
  u_multi_post_hf <-  u_multi_post_hosp_hf * percent_hosp_hf +
    u_multi_post_unhosp_hf * (1- percent_hosp_hf)
  
    # Define the health states
  list_states_6 <- c("Well", "Stroke", 
                     "hf", "post_stroke",
                     "post_hf", "Dead")
  
  # Create a transition matrix
  transition_matrix_6 <- matrix(c(
    p_well, p_stroke, p_hf, 0, 0, p_death, # Well
    0, 0, 0, 0, 0, p_death * smr_stroke, # Stroke (tunnel)
    0, 0, 0, 0, 0, p_death * smr_hf, # Heart Failure (tunnel)
    0, p_stroke, p_hf, 0, 0, p_death, # Post-Stroke
    0, p_stroke, p_hf, 0, 0, p_death, # Post-Heart Failure
    0, 0, 0, 0, 0, 1 # Dead (absorbing state)
  ), nrow = length(list_states_6), byrow = TRUE)
  
  rownames(transition_matrix_6) <- list_states_6
  colnames(transition_matrix_6) <- list_states_6
  
  # Prob of going from disease health state to death 
  transition_matrix_6[2,4] <- 1 - transition_matrix_6[2,6]
  transition_matrix_6[3,5] <- 1 - transition_matrix_6[3,6]
  
  # Calculate chance of remaining in post health states 
  transition_matrix_6[4,4] <- 1 - sum(transition_matrix_6[4,])
  transition_matrix_6[5,5] <- 1 - sum(transition_matrix_6[5,])
  
  # Payoffs
  m_payoffs_6 <- matrix(c(c_monitoring, c_stroke, c_hf, c_post_stroke, c_post_hf, 0,
                          u_well, u_well*u_multi_stroke, u_well*u_multi_hf, 
                          u_well*u_multi_post_stroke, u_well*u_multi_post_hf, 0),
                        nrow = length(list_states_6), ncol = 2, byrow = FALSE,
                        dimnames = list(state = list_states_6,
                                        payoff = c("Cost", "QALY")))
  
  # State membership
  state_membership_6 <- array(NA_real_,
                              dim = c(n_cycles, length(list_states_6)),
                              dimnames = list(cycle = 1:n_cycles,
                                              state = list_states_6))
  
  state_membership_6[1,] <- c(n_cohort, 0, 0, 0, 0, 0)
  
  for (i in 2:n_cycles) {
    state_membership_6[i, ] <- state_membership_6[i - 1, ] %*% transition_matrix_6
  }
  
  # Calculate results
  payoff_trace_6 <- state_membership_6 %*% m_payoffs_6
  
  # apply intervention cost
  payoff_trace_6[1,1] <- payoff_trace_6[1,1] + (c_int * n_cohort)
  
  results <- colSums(payoff_trace_6) / n_cohort
  
  # Return results and other useful objects
  return(list(
    results = results,
    state_membership = state_membership_6,
    payoff_trace = payoff_trace_6,
    transition_matrix = transition_matrix_6,
    payoffs_matrix = m_payoffs_6
  ))
}


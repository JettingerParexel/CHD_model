f_4_health_state_model <- function(params,
                                   apply_RR = 0,
                                   c_int = 0) {

    # Extract parameters from the params list/vector
  n_py <- params["n_py"]
  n_cycles <- params["n_cycles"]
  n_cohort <- params["n_cohort"]
  
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
  
  
  
  
  df_trt_effect_4 <- df_trt_effect_data[1,] + 
    df_trt_effect_data[2,] + 
    df_trt_effect_data[3,] + 
    df_trt_effect_data[4,] 
  
  df_trt_effect_4 <- df_trt_effect_4%>%
    mutate(Risk.Int = df_trt_effect_4$Ne.Int/n_trial_people,
           Risk.Comp = df_trt_effect_4$Ne.Comp/n_trial_people)
  
  # calculate relative risks 
  
  Risk.Int <- df_trt_effect_4$Ne.Int/n_trial_people
  
  Risk.Comp <- df_trt_effect_4$Ne.Comp/n_trial_people
  
  
  RR <- if(apply_RR == 1) { Risk.Int / Risk.Comp} else
  {c(1)}
  # calculate annualised event rates
  
  r_total_events <- as.numeric((n_isch_stroke + n_haem_stroke + n_unhosp_hf + n_hosp_hf)/ n_py)
  
  # event weights for 4 health state STM

  p_event_isch_stroke <- n_isch_stroke / (n_hosp_hf + n_unhosp_hf + n_isch_stroke+n_haem_stroke)
  p_event_haem_stroke <- n_haem_stroke  / (n_hosp_hf + n_unhosp_hf + n_isch_stroke+n_haem_stroke)
  p_event_unhosp_hf <- n_unhosp_hf / (n_hosp_hf + n_unhosp_hf + n_isch_stroke+n_haem_stroke)
  p_event_hosp_hf <- n_hosp_hf / (n_hosp_hf + n_unhosp_hf + n_isch_stroke+n_haem_stroke)
  
  # Transition probabilities
  p_event <- (1 - exp(-r_total_events * t)) * RR
  p_death <- 1 - exp(-mr_well * t)
  p_well <- 1 - (p_event + p_death)
  
  #Calculate SMRs

  smr_event <-  smr_isch_stroke * p_event_isch_stroke +
    smr_haem_stroke * p_event_haem_stroke +
    smr_hosp_hf * p_event_hosp_hf + 
    smr_unhosp_hf * p_event_unhosp_hf
  
  # Calculate event costs
  
    c_event <- c_isch_stroke * p_event_isch_stroke +
    c_haem_stroke * p_event_haem_stroke +
    c_hosp_hf * p_event_hosp_hf + 
    c_unhosp_hf * p_event_unhosp_hf
  
    # Calculate post-event costs 
    
    c_post_event <- c_post_isch_stroke * p_event_isch_stroke +
      c_post_haem_stroke * p_event_haem_stroke +
      c_post_hosp_hf * p_event_hosp_hf + 
      c_post_unhosp_hf * p_event_unhosp_hf
    
    #calculate event utility multipliers
    
    u_multi_event <-  u_multi_isch_stroke * p_event_isch_stroke +
      u_multi_haem_stroke * p_event_haem_stroke +
      u_multi_hosp_hf * p_event_hosp_hf + 
      u_multi_unhosp_hf * p_event_unhosp_hf
    
  #  calculate post-event utility multipliers
    
    u_multi_post_event <-   u_multi_post_isch_stroke * p_event_isch_stroke +
      u_multi_post_haem_stroke * p_event_haem_stroke +
      u_multi_post_hosp_hf * p_event_hosp_hf + 
      u_multi_post_unhosp_hf * p_event_unhosp_hf
    
  # Define the health states
  list_states_4 <- c("Well", "Event", 
                     "Post-event", "Dead")
  
  # Create a transition matrix
  transition_matrix_4 <- matrix(c(
    p_well, p_event, 0, p_death, # Well
    0, 0, 1, p_death * smr_event, # event (tunnel)
    0, p_event, 0, p_death, # Post-event
    0, 0, 0, 1 # Dead (absorbing state)
  ), nrow = length(list_states_4), byrow = TRUE)
  
  rownames(transition_matrix_4) <- list_states_4
  colnames(transition_matrix_4) <- list_states_4
  
  # Prob of going from disease health state to death 
  transition_matrix_4[2,3] <- 1 - transition_matrix_4[2,4]
  
  # Calculate chance of remaining in post health states 
  transition_matrix_4[3,3] <- 1 - sum(transition_matrix_4[3,])
  
  # Payoffs
  m_payoffs_4 <- matrix(c(c_monitoring, c_event, c_post_event, 0,
                          u_well, u_well*u_multi_event, u_well*u_multi_post_event, 0),
                        nrow = length(list_states_4), ncol = 2, byrow = FALSE,
                        dimnames = list(state = list_states_4,
                                        payoff = c("Cost", "QALY")))
  
  # State membership
  state_membership_4 <- array(NA_real_,
                              dim = c(n_cycles, length(list_states_4)),
                              dimnames = list(cycle = 1:n_cycles,
                                              state = list_states_4))
  
  state_membership_4[1,] <- c(n_cohort, 0, 0, 0)
  
  for (i in 2:n_cycles) {
    state_membership_4[i, ] <- state_membership_4[i - 1, ] %*% transition_matrix_4
  }
  
  # Calculate results
  payoff_trace_4 <- state_membership_4 %*% m_payoffs_4
  
  # apply intervention cost
  payoff_trace_4[1,1] <- payoff_trace_4[1,1] + (c_int * n_cohort)
  
  results <- colSums(payoff_trace_4) / n_cohort
  
  # Return results and other useful objects
  return(list(
    results = results,
    state_membership = state_membership_4,
    payoff_trace = payoff_trace_4,
    transition_matrix = transition_matrix_4,
    payoffs_matrix = m_payoffs_4
  ))
}





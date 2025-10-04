library("readxl")
library("dplyr")
library("Epi")

# model settings

# model inputs sourcing 

df <- read_excel(".../cvd_synthetic_dataset_v0.2.xlsx", sheet = 1)


params <- c(
  
  # model settings
  
  t = 1, # cycle length
  n_cycles = 50,
  n_cohort = 1000,
  
  # calculate total person years and number of events
  n_py = sum(df$time_to_event_or_censoring),
  
  # calculate n of events 
  n_isch_stroke = sum(df$classification_column == 1, na.rm = TRUE),
  n_haem_stroke = sum(df$classification_column == 2, na.rm = TRUE),
  n_unhosp_hf = sum(df$classification_column == 3, na.rm = TRUE),
  n_hosp_hf = sum(df$classification_column == 4, na.rm = TRUE),
  
  # general population mortality and standardised mortality ratios
  mr_well = 0.02, 
  smr_isch_stroke = 1.4,
  smr_haem_stroke = 2.72,
  smr_unhosp_hf = 1,
  smr_hosp_hf = 2.2,
  
  # costs
  c_isch_stroke = 16746,
  c_haem_stroke = 23076,
  c_unhosp_hf = 2719,
  c_hosp_hf = 4641,
  
  # intervention cost
  c_int = 10000,
  c_monitoring = 0,
  c_post_isch_stroke = 587,
  c_post_haem_stroke = 1749,
  c_post_hosp_hf = 706,
  c_post_unhosp_hf = 203,
  
  # QALYs
  u_well = 1,
  
  # utility multipliers
  u_multi_isch_stroke = 0.756,
  u_multi_haem_stroke = 0.628,
  u_multi_unhosp_hf = 0.770,
  u_multi_hosp_hf = 0.683,
  
  u_multi_post_isch_stroke = 0.816,
  u_multi_post_haem_stroke = 0.628,
  u_multi_post_unhosp_hf = u_multi_unhosp_hf * 1.2,
  u_multi_post_hosp_hf = u_multi_hosp_hf * 1.2,
  
  # treatment effects
  Ne.int_isch_stroke = 4,
  Ne.int_haem_stroke = 10,
  Ne.int_hosp_hf = 3, 
  Ne.int_unhosp_hf = 7,
  Ne.comp_isch_stroke = 24,
  Ne.comp_haem_stroke = 56,
  Ne.comp_hosp_hf = 6, 
  Ne.comp_unhosp_hf = 34,
  n_trial_people = 100 
)





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


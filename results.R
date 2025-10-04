
#base case results

df_basecase_results <- f_run_models(params)

# base case difference

n_basecase_diff <- max(df_basecase_results["ICER"]) - min(df_basecase_results["ICER"])

#one-way senstivity analysis

upper_owsa <- f_OWSA(params, n_change = 1.2)

lower_owsa <- f_OWSA(params, n_change = 0.8)

df_owsa <- data.frame(
  param = upper_owsa[,1],
  lower = lower_owsa[,5],
  upper = upper_owsa[,5]
)


max(df_owsa[-1,2])
max(df_owsa[-1,3])

min(df_owsa[-1,2])
min(df_owsa[-1,3])

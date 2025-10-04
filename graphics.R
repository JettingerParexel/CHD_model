library(ggplot2)
library(tidyr)




### look at utility values and costs
ggplot(df, aes(x = utility)) + geom_histogram(binwidth = 0.1) + facet_grid(~classification_column)
ggplot(df, aes(x = costs)) + geom_histogram(binwidth = 0.1) + facet_grid(~classification_column)

# Assuming your dataframe is called 'df'
# We need to reshape the data from wide to long format

# Now create the line graph 10

# Convert matrix to dataframe and reshape to long format
df_long_ten <- results_int_10$state_membership %>%
  as.data.frame() %>%
  pivot_longer(cols = -Time, names_to = "State", values_to = "Value")

# Define a color palette
colors <- c("Well" = "#E41A1C", "isch_stroke" = "#377EB8", "haem_stroke" = "#4DAF4A",
            "hosp_hf" = "#984EA3", "unhosp_hf" = "#FF7F00", "post_isch_stroke" = "#00FFFF",
            "post_haem_stroke" = "#A65628", "post_hosp_hf" = "#F781BF", 
            "post_unhosp_hf" = "#999999", "Dead" = "#000000")

# Create the plot
ggplot(df_long_ten, aes(x = Time, y = Value, color = State)) +
  geom_line() +
  scale_color_manual(values = colors) +
  theme_minimal() +
  labs(title = "State Membership Over Time",
       x = "Time",
       y = "Membership Value") +
  theme(legend.position = "right",
        legend.title = element_blank())


# dsix 
# Convert matrix to dataframe and reshape to long format
df_long_six <- state_membership_6 %>%
  as.data.frame() %>%
  mutate(Time = 1:50) %>%
  pivot_longer(cols = -Time, names_to = "State", values_to = "Value")

# Define a color palette
colors <- c("Well" = "#E41A1C", "Stroke" = "#377EB8", "hf" = "#984EA3","Dead" = "#000000", 
            "post_stroke" = "#A65628", "post_hf" = "#F781BF")

# Create the plot
ggplot(df_long_six, aes(x = Time, y = Value, color = State)) +
  geom_line() +
  scale_color_manual(values = colors) +
  theme_minimal() +
  labs(title = "State Membership Over Time",
       x = "Time",
       y = "Membership Value") +
  theme(legend.position = "right",
        legend.title = element_blank())


# Convert matrix to dataframe and reshape to long format
df_long_four <- state_membership_4 %>%
  as.data.frame() %>%
  mutate(Time = 1:50) %>%
  pivot_longer(cols = -Time, names_to = "State", values_to = "Value")

# Define a color palette
colors <- c("Well" = "#E41A1C", "Event" = "#377EB8", "Post-event" = "#984EA3","Dead" = "#000000")

#four
# Create the plot
ggplot(df_long_four, aes(x = Time, y = Value, color = State)) +
  geom_line() +
  scale_color_manual(values = colors) +
  theme_minimal() +
  labs(title = "State Membership Over Time",
       x = "Time",
       y = "Membership Value") +
  theme(legend.position = "right",
        legend.title = element_blank())










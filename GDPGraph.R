# Load required libraries
library(readxl)
library(tidyverse)

# Read the Excel file (use your correct file path if different)
gdp_data <- read_excel("C:/Users/Anurag/Downloads/GDP.xlsx")

# Rename first column to state_code for clarity
names(gdp_data)[1] <- "state_code"

# Convert from wide to long format
gdp_long <- gdp_data %>%
  pivot_longer(
    cols = -state_code,
    names_to = "period",
    values_to = "cagr"
  )

# Separate India from state data
india_data <- gdp_long %>% filter(state_code == "India")
states_data <- gdp_long %>% filter(state_code != "India")

# Define the states you want to plot
selected_states <- c("TN", "GJ","CG","BR","MP","UP","AS")

# Filter the long data
filtered_states_data <- gdp_long %>% filter(state_code %in% selected_states)
filtered_india_data <- gdp_long %>% filter(state_code == "India")

contrasting_colors <- c(
  "MH" = "#e85724",  # light blue
  "TN" = "#f39673",  # light green
  "GJ" = "#8cb73f",  # light orange
  "CG" = "#ccdf84",  # light turquoise
  "BR" = "#029bd6",  # light yellow
  "MP" = "#86bfde",
  "UP" = "#7f7f7f",
  "AS" = "#f5f0e7"
)

# Plot
ggplot() +
  geom_line(data = filtered_states_data,
            aes(x = period, y = cagr, color = state_code, group = state_code),
            linewidth = 1.2) +
  geom_line(data = filtered_india_data,
            aes(x = period, y = cagr, group = 1),
            color = "black", linetype = "dotted", linewidth = 1.2) +
  scale_color_manual(values = contrasting_colors) +
  labs(
    title = "State-wise GDP",
    x = "Period",
    y = "CAGR (%)",
    color = "State"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )
















# Plot
ggplot() +
  geom_line(data = filtered_states_data, aes(x = period, y = cagr, color = state_code, group = state_code), linewidth = 1) +
  geom_line(data = filtered_india_data, aes(x = period, y = cagr, group = 1), color = "black", linetype = "dotted", linewidth = 1.2) +
  labs(
    title = "GDP CAGR States",
    x = "Period",
    y = "CAGR (%)",
    color = "State"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


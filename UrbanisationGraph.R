# Load libraries
library(readxl)
library(tidyverse)

# Step 1: Read the data
gdp_data <- read_excel("data/Urbanisation.xlsx")  # <-- change if needed

# Step 2: Rename first column for consistency
names(gdp_data)[1] <- "state_code"

# Step 3: Convert from wide to long format
gdp_long <- gdp_data %>%
  pivot_longer(
    cols = -state_code,
    names_to = "year",
    values_to = "percent"
  )

# Step 4: Clean % symbols and convert to numeric
gdp_long <- gdp_long %>%
  mutate(
    percent = as.numeric(str_remove(percent, "%")),
    year = as.integer(year)
  )

# Step 5: Filter
selected_states <- c("AS", "BR", "CG", "GJ", "MP", "TN", "UP")
states_data <- gdp_long %>% filter(state_code %in% selected_states)
india_data <- gdp_long %>% filter(state_code == "India")

# Step 6: Define lighter pastel colors
state_colors <- c(
  "MH" = "#e85724",  # light blue
  "TN" = "#f39673",  # light green
  "GJ" = "#8cb73f",  # light orange
  "CG" = "#ccdf84",  # light turquoise
  "BR" = "#029bd6",  # light yellow
  "MP" = "#86bfde",
  "UP" = "#7f7f7f",
  "AS" = "#f5f0e7"
)

# Step 7: Plot
ggplot() +
  geom_line(data = states_data, aes(x = year, y = percent, color = state_code, group = state_code), size = 1.2) +
  geom_line(data = india_data, aes(x = year, y = percent, group = 1), color = "black", linetype = "dotted", size = 1.2) +
  scale_color_manual(values = state_colors) +
  labs(
    title = "Urbanisation",
    x = "Year",
    y = "Urbanisation (%)",
    color = "State"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

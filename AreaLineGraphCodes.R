# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Load data
data <- read_excel("data/adjusted_generation_forecast_BAU.xlsx")

# Filter years from 2020 to 2070 with a 5-year gap
data_filtered <- data %>%
  filter(year %% 5 == 0)

# ---- USER INPUT: Select subsectors and states here ----
selected_subsectors <- c("coal","gas","nuclear","hydro","solar","wind","biomass")  # you can modify this
selected_states <- c("JH")  # modify as needed

# Filter based on user selection
plot_data <- data_filtered %>%
  filter(subsector %in% selected_subsectors, state %in% selected_states)
plot_data %>% filter(subsector == "coal", year == 2070)



# Set a fixed y-axis limit for consistent scale across plots (optional)
y_max <- max(plot_data$adjusted_value, na.rm = TRUE) * 1.1  # Add 10% buffer

plot_data_filtered <- plot_data %>%
  filter(adjusted_value > 0.001)

ggplot(plot_data_filtered, aes(x = year, y = adjusted_value, fill = subsector)) +
  geom_area(position = "stack", alpha = 0.8) +
  scale_fill_manual(values = c("coal" = "#e85724",
                      "gas" = "#f39673",
                      "biomass" = "#8cb73f",
                      "nuclear" = "#ccdf84",
                      "hydro" = "#029bd6",
                      "solar" = "#86bfde",
                      "wind" = "#7f7f7f")) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(
    title = "Electricity Generation(BAU)",
    x = "Year",
    y = "Electricity Generation (EJ)",
    fill = "Subsector"
  ) +
  theme_minimal(base_size = 10)

ggplot(plot_data, aes(x = year, y = adjusted_value, color = state)) +
  geom_line(size = 1) +  # Use lines instead of area
  facet_wrap(~subsector, scales = "free_y") +  # One graph per subsector
  scale_color_manual(values = c("#e85724", "#f39673", "#8cb73f", "#ccdf84", "#029bd6", "#86bfde", "#7f7f7f")) +
  scale_y_continuous(limits = c(0, 1.25), labels = comma) +
  labs(
    title = "Electricity Generation (NZ)",
    x = "Year",
    y = "Electricity Generation (EJ)",
    color = "State"
  ) +
  theme_minimal(base_size = 10)

# Additional
geom_area(position = "stack", alpha = 0.7)

scale_y_continuous(
  limits = c(0, y_max),
  breaks = seq(0, y_max, by = 1),
  labels = comma
)
breaks = seq(0,y_max, by =2),

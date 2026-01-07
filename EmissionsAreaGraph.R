library(readxl)
library(dplyr)
library(ggplot2)

data <- read_excel("C:/Users/Anurag/Downloads/AllSectorEmissions_NZ.xlsx")

selected_types <- c("Power", "Industry", "Building", "Transport")
selected_states <- c("GJ")

data_filtered <- data %>%
  filter(
    year %% 5 == 0,
    year >= 2020,
    year <= 2070,
    type %in% selected_types,
    region %in% selected_states,
    !is.na(adjusted_value)
  ) %>%
  mutate(adjusted_value = adjusted_value * 3.66)

# Calculate total emissions by type to decide stacking order
emission_totals <- data_filtered %>%
  group_by(type) %>%
  summarise(total = sum(adjusted_value, na.rm = TRUE)) %>%
  arrange(total)

# Reorder factor levels so smallest sector is at bottom
data_filtered$type <- factor(data_filtered$type, levels = emission_totals$type)

# Plot
ggplot(data_filtered, aes(x = year, y = adjusted_value, fill = type)) +
  geom_area(position = "stack", color = "black", size = 0.1) +
  scale_fill_manual(values = c(
    "Power" = "#e85724",
    "Industry" = "#ccdf84",
    "Building" = "#029bd6",
    "Transport" = "#7f7f7f"
  )) +
  scale_y_continuous(limits = c(0, 3500)) +
  labs(
    title = paste("Sector-wise Emissions NZ", paste(selected_states, collapse = ", ")),
    x = "Year",
    y = "Emissions (MtCO2)",
    fill = "Subsector"
  ) +
  theme_minimal(base_size = 11)


emission_totals <- data_filtered %>%
  group_by(type) %>%
  summarise(total = sum(adjusted_value, na.rm = TRUE)) %>%
  arrange(total)

emission_totals



# Load required libraries
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)

# Step 1: Read your Excel file
file_path <- "data/adjusted_industry_BAU.xlsx"
df <- read_excel(file_path)

# Step 2: Filter for 2070 and aggregate across fuels
df_2070 <- df %>%
  filter(year == 2020) %>%
  group_by(region) %>%
  summarise(total_consumption = sum(adjusted_value, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Read state codes and shapefile
statecodes <- read_excel("data/Statecodes.xlsx")
sf <- st_read("data/India_State_Boundary.shp")

# Step 4: Merge spatial data with state codes
sf <- sf %>%
  rename(State = State_Name) %>%
  left_join(statecodes, by = "State") %>%
  rename(state = `State Code`)

# Step 5: Merge with energy data
india_map <- sf %>%
  left_join(df_2070, by = c("State" = "region"))

ggplot(india_map) +
  geom_sf(aes(fill = total_consumption), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "#FFE5B4",   # light orange
    high = "#FF4500",  # dark orange
    na.value = "grey90",
    name = "Consumption (EJ)"
  ) +
  labs(
    title = "State-wise Energy Consumption (2020)",
    caption = "Source: GCAM/CEEW Analysis"
  ) +
  theme_minimal(base_size = 10)
  )


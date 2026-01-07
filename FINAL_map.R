# Load required libraries
library(sf)       # For handling spatial data (shapefiles)
library(ggplot2)  # For visualization
library(dplyr)    # For data wrangling
library(readxl)
library(stringr)   # For data manipulation
library(ggthemes)  # For better visualization themes
library(openxlsx)
library(grid)
library(patchwork)
library(gridExtra)
library(png)
# -----------------------------------------------
# STEP 1: Read all Excel files
# -----------------------------------------------
adjusted_forecast_ind_NZ <- read_excel("C:/Users/Anurag/Downloads/adjusted_industryenergy_NZ.xlsx")
adjusted_forecast_ind_BAU <- read_excel("C:/Users/Anurag/Downloads/adjusted_industry_BAU.xlsx")  # Industry sector
adjusted_forecast_ind_NZ <- adjusted_forecast_ind_NZ %>%
  mutate(scenario = "NZ")
adjusted_forecast_ind_BAU <- adjusted_forecast_ind_BAU %>%
  mutate(scenario = "BAU")

adjusted_forecast_trn_NZ <- read_excel("C:/Users/Anurag/Downloads/adjusted_energy_trn_NZ.xlsx")
adjusted_forecast_trn_BAU <- read_excel("C:/Users/Anurag/Downloads/adjusted_forecast_trn_BAU.xlsx")  # Transport sector
adjusted_forecast_trn_NZ <- adjusted_forecast_trn_NZ %>%
  mutate(scenario = "NZ")
adjusted_forecast_trn_BAU <- adjusted_forecast_trn_BAU %>%
  mutate(scenario = "BAU")

adjusted_forecast_blg_NZ <- read_excel("C:/Users/Anurag/Downloads/Final_Energy_Building_NZ.xlsx")  # Building sector
adjusted_forecast_blg_BAU <- read_excel("C:/Users/Anurag/Downloads/BuildingEnergy_BAU.xlsx")
adjusted_forecast_blg_NZ <- adjusted_forecast_blg_NZ %>%
  mutate(scenario = "NZ")
adjusted_forecast_blg_BAU <- adjusted_forecast_blg_BAU %>%
  mutate(scenario = "BAU")

combined_data_ind <- bind_rows(adjusted_forecast_ind_NZ, adjusted_forecast_ind_BAU)
write.xlsx(combined_data_ind, "C:/Users/Anurag/Downloads/adjusted_forecast_ind.xlsx", sheetName = "Combined", overwrite = TRUE)

combined_data_trn <- bind_rows(adjusted_forecast_trn_NZ, adjusted_forecast_trn_BAU)
write.xlsx(combined_data_trn, "C:/Users/Anurag/Downloads/adjusted_forecast_trn.xlsx", sheetName = "Combined", overwrite = TRUE)

combined_data_blg <- bind_rows(adjusted_forecast_blg_NZ, adjusted_forecast_blg_BAU)
write.xlsx(combined_data_blg, "C:/Users/Anurag/Downloads/adjusted_forecast_blg.xlsx", sheetName = "Combined", overwrite = TRUE)
# -----------------------------------------------
# STEP 2: Combine all datasets into one master file
# -----------------------------------------------
combined_data_trn <- combined_data_trn %>%
  rename(region = state)
master_data <- bind_rows(combined_data_ind, combined_data_trn, combined_data_blg)
# -----------------------------------------------
# STEP 3: Reclassify 'input' column based on keywords
# -----------------------------------------------
master_data <- master_data %>%
  mutate(
    input_group = case_when(
      str_detect(input, "biomass") ~ "biomass",
      str_detect(input, "coal") ~ "coal",
      str_detect(input, "gas") ~ "gas",
      str_detect(input, "H2 enduse") ~ "H2 enduse",
      str_detect(input, "refined liquids") ~ "refined liquids",
      str_starts(input, "elect") ~ "electricity",
      TRUE ~ input # Keep original name if it doesn’t match any category
    )
  )

# -----------------------------------------------
# STEP 4: Summarize data by 'state', 'year', and 'input_group'
# -----------------------------------------------
master_data <- master_data %>%
  group_by(region, scenario, year, input_group) %>%
  summarize(total_adjusted_value = sum(adjusted_value, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------
# STEP 5: Calculate total energy generation per state and year
# -----------------------------------------------
total_energy_by_state <- master_data %>%
  group_by(region, scenario, year) %>%
  summarize(total_adjusted_value = sum(total_adjusted_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(input_group = "total")  # Label this row as "total"


# -----------------------------------------------
# STEP 6: Merge with master summary to include total energy for each state
# -----------------------------------------------
master_data <- bind_rows(master_data, total_energy_by_state)

# Step 1: Aggregate DD and DN into DD_DN by input_group and year
dd_dn_aggregated <- master_data %>%
  filter(region %in% c("DD", "DN")) %>%  # Select rows where state is DD or DN
  group_by(input_group, year) %>%  # Group by input_group and year
  summarize(total_adjusted_value = sum(total_adjusted_value, na.rm = TRUE), .groups = "drop") %>%  # Sum values
  mutate(region = "DD_DN")  # Create a new state called "DD_DN"

# Step 2: Remove DD and DN from the master dataset
master_data <- master_data %>%
  filter(!region %in% c("DD", "DN"))  # Remove DD and DN rows

# Step 3: Append the new aggregated DD_DN row
master_data <- bind_rows(master_data, dd_dn_aggregated)

# -----------------------------------------------
# STEP 5: Save the master file
# -----------------------------------------------
write.csv(master_data, "Master_Forecast.csv", row.names = FALSE)

# Step 1: Aggregate total energy to ensure uniqueness
#total_energy <- master_data %>%
# filter(input_group == "total") %>%
#group_by(region, year) %>%  # Ensure one value per region-year
#summarize(total_value = sum(total_adjusted_value, na.rm = TRUE), .groups = "drop")

# Step 2: Compute share percentages
#share_data <- master_data %>%
# filter(input_group %in% c("electricity")) %>%
#left_join(total_energy, by = c("region", "year")) %>%  # Now a one-to-many join
#mutate(share_percentage = (total_adjusted_value / total_value) * 100) %>%  # Compute percentage
#select(region, year, input_group, share_percentage)  # Keep necessary columns

# Step 3: Save the new dataset (optional)
#write.csv(share_data, "energy_share_data.csv", row.names = FALSE)



#Creating Maps


names(sf)

#Read state codes data (mapping state names to state codes)
statecodes <- read_excel("C:/Users/Anurag/Downloads/StateIntegrationGraphs/Statecodes.xlsx")

# Read shapefile containing spatial data for states
sf <- st_read("C:/Users/Anurag/Downloads/StateIntegrationGraphs/shapefiles_CEEW/shapefiles_CEEW/India_State_Boundary.shp")

# =========================== #
# STEP 2: Preprocess Spatial Data (Shapefile)
# =========================== #

master_data <- master_data %>%
  rename(state = 'region')
master_2070data_map <- master_data %>%
  filter(year == 2070, scenario == "NZ", input_group == "total")

sf <- st_make_valid(sf)

masterline_map <- master_data %>%
  filter(input_group == "total")

sf <- sf %>%
  rename(state = State_Name)

merged_2070data <- merge(sf, master_2070data_map, by = "state", all.x = TRUE)

# Normalize year and energy values for graph scaling
merged_data_sf <- merge(sf, masterline_map, by = "state", all.x = TRUE) %>%
  group_by(state) %>%
  mutate(scaled_x = (year - min(year)) / (max(year) - min(year)) * 2,  # Scale for positioning
         scaled_y = total_adjusted_value) %>%
  ungroup()

# Step 2: Create Map with Superimposed Graphs
library(gridExtra)

# List of states to exclude from graph generation
# Remove NA states
merged_data_sf <- merged_data_sf %>% filter(!is.na(state))

# List of states to exclude from graph generation
excluded_states <- c("DD_DN", "IN", "PC", "LD", "LA", "AS", "TR", "ML", "MZ", "NL", "DL", "MN", "PD", "CH", "AN", "AR", "DN", "JK", "SK")

# Ensure only valid states are considered
graph_states <- setdiff(unique(merged_data_sf$state), excluded_states)

# Extract max value for normalizing
max_value <- max(merged_data_sf$total_adjusted_value, na.rm = TRUE)

# Extract gradient data for background (NZ 2070 scenario)
gradient_data <- merged_data_sf %>%
  filter(year == 2070, scenario == "NZ") %>%
  select(state, total_adjusted_value)

# Ensure output directory exists
if (!dir.exists("graphs")) dir.create("graphs")

graph_files <- c()

library(ggplot2)
library(grid)
library(png)

# Normalize state values for color mapping
max_energy <- max(merged_data_sf$total_adjusted_value, na.rm = TRUE)

# Get gradient colors based on total energy demand in 2070
gradient_data <- merged_data_sf %>%
  filter(year == 2070, scenario == "NZ") %>%
  mutate(normalized_value = total_adjusted_value / max_energy) %>%
  select(state, normalized_value)

# Define the color scale
get_gradient_color <- function(value) {
  return(col2rgb(colorRampPalette(c("white", "red"))(100)[floor(value * 99) + 1]) / 255)
}

graph_files <- c()

for (state_name in graph_states) {
  state_data <- merged_data_sf %>% filter(state == state_name)

  # Get background color for the state
  state_energy <- gradient_data %>% filter(state == state_name) %>% pull(normalized_value)
  state_color <- get_gradient_color(state_energy)

  # Convert RGB to hex
  state_hex <- rgb(state_color[1], state_color[2], state_color[3], alpha = 1)

  # Create plot with dynamic background
  p <- ggplot(state_data, aes(x = year, y = total_adjusted_value)) +
    # Background fill using annotation
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = state_hex) +

    # Line graph
    geom_line(aes(color = factor(scenario)), linewidth = 1.9) +

    # Customize colors
    scale_color_manual(values = c("black", "green")) +

    # Axes settings
    scale_x_continuous(breaks = c(2020, 2050, 2070)) +
    scale_y_continuous(limits = c(0, max_energy)) +

    # Title with state name
    ggtitle(state_name) +

    # Formatting
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text = element_text(face = "bold", size = 18),
      axis.title = element_blank(),
      plot.title = element_text(size= 20, hjust = 0.5, face = "bold")
    )

  # Save plot as image
  file_path <- paste0("graphs/", state_name, ".png")
  ggsave(filename = file_path, plot = p, width = 3, height = 3, dpi = 300)
  graph_files <- c(graph_files, file_path)
}

india_map <- ggplot() +
  geom_sf(data = merged_2070data, aes(fill = total_adjusted_value), color = "black") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_void() +
  labs(title = "State-wise Final Energy Consumption (BAU/NZ)", fill = "Energy (EJ)")

# -------------------
# Arrange Graphs
# -------------------
graph_images <- lapply(graph_files, function(file) rasterGrob(readPNG(file), interpolate = TRUE))
graph_grid <- arrangeGrob(grobs = graph_images, ncol = 5)
title <- textGrob("State-Level Energy Consumption Over Time", gp = gpar(fontsize = 14, fontface = "bold"))

# Arrange the grid with the title
grid.arrange(title, graph_grid, ncol = 1, heights = c(0.1, 1))
# -------------------
# Save Final Image
# -------------------
final_plot <- wrap_elements(india_map) +
    wrap_elements(graph_grid) +
    plot_layout(widths = c(1, 1))  # Make grid larger than map

 # Save the final image
 ggsave("India_Energy_Trajectories.png", plot = final_plot, width = 20, height = 12, dpi = 300)

print(final_plot)
print(max(merged_data_sf$total_adjusted_value))


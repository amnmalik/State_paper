# Load required libraries
library(sf)       # For handling spatial data (shapefiles)
library(ggplot2)  # For visualization
library(dplyr)    # For data wrangling
library(readxl)
library(stringr)

# -----------------------------------------------
# STEP 1: Read all Excel files
# -----------------------------------------------
adjusted_forecast_ind <- read_excel(file.choose())  # Industry sector
adjusted_forecast_trn <- read_excel(file.choose())  # Transport sector
adjusted_forecast_blg <- read_excel(file.choose())  # Building sector

# -----------------------------------------------
# STEP 2: Combine all datasets into one master file
# -----------------------------------------------
adjusted_forecast_trn <- adjusted_forecast_trn %>%
  rename(region = state)
master_data <- bind_rows(adjusted_forecast_ind, adjusted_forecast_trn, adjusted_forecast_blg)

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
  group_by(region, year, input_group) %>%
  summarize(total_adjusted_value = sum(adjusted_value, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------
# STEP 5: Calculate total energy generation per state and year
# -----------------------------------------------
total_energy_by_state <- master_data %>%
  group_by(region, year) %>%
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
write.csv(master_data, "FinalEnergyConsumption_NZ.csv", row.names = FALSE)

# Step 1: Aggregate total energy to ensure uniqueness
total_energy <- master_data %>%
  filter(input_group == "total") %>%
  group_by(region, year) %>%  # Ensure one value per region-year
  summarize(total_value = sum(total_adjusted_value, na.rm = TRUE), .groups = "drop")

# Step 2: Compute share percentages
share_data <- master_data %>%
  filter(input_group %in% c("electricity")) %>%
  left_join(total_energy, by = c("region", "year")) %>%  # Now a one-to-many join
  mutate(share_percentage = (total_adjusted_value / total_value) * 100) %>%  # Compute percentage
  select(region, year, input_group, share_percentage)  # Keep necessary columns

# Step 3: Save the new dataset (optional)
write.csv(share_data, "energy_share_data.csv", row.names = FALSE)



#Creating Maps




#Read state codes data (mapping state names to state codes)
statecodes <- read_excel(file.choose())

# Read shapefile containing spatial data for states
sf <- st_read(file.choose())

# =========================== #
# STEP 2: Preprocess Spatial Data (Shapefile)
# =========================== #

# Rename the column in spatial data for consistency
sf <- sf %>%
  rename(State = State_Name)  # Ensure 'State' matches the naming in statecodes

# Merge spatial data with state codes
sf <- sf %>%
  left_join(statecodes, by = "State")  # Add state codes from statecodes dataset

# Rename the column for consistency with other datasets
sf <- sf %>%
  rename(state = 'State Code')  # Ensure 'state' column is correctly formatted

# Filter, group, and summarize data for total power generation
share_data <- share_data %>%
  rename(state = 'region')
sharedata_map <- share_data %>%
  filter(year == 2070)     # Include only data for the year 2070
# =========================== #
# STEP 4: Merge Spatial Data with Power Generation Data
# =========================== #

# Perform a left join to retain all spatial data while adding power generation data
merged_sharedata <- merge(sf, sharedata_map, by = "state", all.x = TRUE)


# =========================== #
# STEP 6: Create the Gradient Map
# =========================== #

map1 <- ggplot() +
  geom_sf(data = merged_sharedata, aes(fill = share_percentage)) +  # Plot states with fill based on total power
  scale_fill_gradientn(
    colors = c("#575756", "#9D9D9C", "#D5D7DC", "#C3E5F5", "#71C9EB", "#009CDB"))+  # Gradient color scheme
  theme_minimal() +  # Apply minimal theme for a clean look
  labs(title = "Share of Electricity consumed in the total energy consumption (2070)", fill = "in %") +  # Set title and legend label
  theme(legend.position = "left")  # Position the legend to the left

# =========================== #
# STEP 7: Print the Final Map
# =========================== #

print(map1)









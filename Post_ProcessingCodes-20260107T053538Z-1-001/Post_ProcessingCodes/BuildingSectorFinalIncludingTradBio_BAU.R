# Load required libraries
library(dplyr)
library(readxl)
library(openxlsx)

# Define the states for which the interpolation will be done
states <- c("AP", "AR", "AS", "HP", "KA", "KL", "MH", "ML", "NL", "TN", "TR")

# Define the years for interpolation (2020 to 2070 with a 5-year gap)
years <- seq(2020, 2070, by = 5)

# Step 1: Load the input data
# Load the real data for interpolation
real_data <- read_excel("data/BuildingResidential.xlsx")

# Load the master sheet data
master_sheet <- read_excel("data/Building_Energy_Adjusted_all.xlsx")

# Step 2: Filter real data for the selected states and 'traditional biomass' in 2020
filtered_data <- real_data %>%
  filter(state %in% states) %>%
  select(state, `traditional biomass`) %>%
  rename(value_2020 = `traditional biomass`)

# Step 3: Create an interpolation function
interpolate_trajectory <- function(value_2020) {
  # Define the trajectory
  trajectory <- data.frame(
    year = years,
    value = ifelse(years <= 2020, value_2020, 0)  # Set up placeholders for linear interpolation
  )

  # Perform linear interpolation between 2020 and 2050
  trajectory$value <- ifelse(
    trajectory$year >= 2020 & trajectory$year <= 2050,
    approx(x = c(2020, 2050), y = c(value_2020, 0), xout = trajectory$year)$y,
    trajectory$value
  )

  return(trajectory)
}

# Step 4: Apply interpolation for each state
interpolated_data <- filtered_data %>%
  rowwise() %>%
  mutate(trajectory = list(interpolate_trajectory(value_2020))) %>%
  unnest(trajectory)

# Step 5: Prepare interpolated data for merging with the master sheet
interpolated_data <- interpolated_data %>%
  rename(region = state, year = year, adjusted_value = value) %>%
  mutate(type = "resid", input = "traditional biomass") %>%
  select(region, type, input, year, adjusted_value) %>%
  crossing(scenario = c("BAU_Final", "NZ_Final"))

# Step 6: Update the master sheet with interpolated data
updated_master_sheet <- master_sheet %>%
  # Remove existing data for the specified states, 'resid' type, and 'traditional biomass' input
  filter(!(region %in% states & type == "resid" & input == "traditional biomass")) %>%
  # Add the interpolated data for these states
  bind_rows(interpolated_data) %>%
  arrange(region, type, input, year)

# Step 7: Write the updated master sheet to an Excel file
output_file <- "data/BuildingEnergy_Final_all.xlsx"
write.xlsx(updated_master_sheet, output_file)


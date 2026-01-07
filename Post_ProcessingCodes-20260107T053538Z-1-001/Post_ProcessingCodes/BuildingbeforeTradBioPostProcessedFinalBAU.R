# Load required libraries
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(rgcam)

# Step 0: Load the database and set up the project
path <- 'D:/Gujarat_State_Modelling/GCAM_State'
conn <- localDBConn(path, 'output')
queryFileInput <- 'data/sp_queries.xml'
prj <- addScenario(conn, proj = 'StateIntegration_BAU_NZ.dat', scenario = c("BAU_Final","NZ_Final"), queryFile = queryFileInput)
prj <- loadProject(proj = "StateIntegration_BAU_NZ.dat")

# Step 1: Read the real data for 2020 (residential and commercial)
real_data_resid <- read_excel("data/BuildingResidential.xlsx")
real_data_comm <- read_excel("data/BuildingCommercial.xlsx")

# Reshape real data for easier processing
real_data_resid_long <- real_data_resid %>%
  pivot_longer(cols = -state, names_to = "input", values_to = "real_value") %>%
  mutate(type = "resid")  # Add type for residential

real_data_comm_long <- real_data_comm %>%
  pivot_longer(cols = -state, names_to = "input", values_to = "real_value") %>%
  mutate(type = "comm")  # Add type for commercial

# Combine both datasets
real_data_long <- bind_rows(real_data_resid_long, real_data_comm_long)

# Step 2: Fetch the database data for all years (do not filter for states here)
database_data <- getQuery(prj, query = 'building final energy by service and fuel',scenario = c("BAU_Final","NZ_Final")) %>%
  select(region, sector, input, year, value,scenario)

# Step 3: Filter database data to include only the regions (states) present in real_data
database_data_filtered <- database_data %>%
  filter(region %in% real_data_long$state)  # Ensure filtering for the states in real_data

# Step 4: Map sectors to types (comm/resid)
sector_to_type <- c(
  "comm cooking" = "comm", "comm hvac" = "comm", "comm lighting" = "comm", "comm others" = "comm", "comm ref" = "comm",
  "resid rural apploth" = "resid", "resid rural cooking" = "resid", "resid rural coolers" = "resid",
  "resid rural cooling" = "resid", "resid rural lighting" = "resid", "resid rural space heating" = "resid",
  "resid urban apploth" = "resid", "resid urban cooking" = "resid", "resid urban coolers" = "resid",
  "resid urban cooling" = "resid", "resid urban lighting" = "resid", "resid urban space heating" = "resid"
)

database_data_filtered <- database_data_filtered %>%
  mutate(type = sector_to_type[sector])  # Add type column

# Step 5: Aggregate database data by region, type, input, and year
database_grouped <- database_data_filtered %>%
  group_by(region, type, input, year,scenario) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")

database_grouped <- database_grouped %>%
  complete(region, type, input, scenario, year = 2020, fill = list(total_value = 0))

# Step 6: Calculate the difference between database and real data for 2020
database_2020 <- database_grouped %>%
  filter(year == 2020)

database_2020 <- database_2020 %>%
  complete(region, type, input, scenario, year = 2020, fill = list(total_value = 0))

# Add missing fuels or regions to real data with default `real_value = NA`
all_inputs <- expand.grid(
  region = unique(database_2020$region),
  type = unique(database_2020$type),
  input = unique(database_2020$input)
)

real_data_long <- all_inputs %>%
  left_join(real_data_long, by = c("region" = "state", "input", "type"))

# Fill missing values in `real_data_long` with `real_value = 0`
real_data_long <- real_data_long %>%
  mutate(real_value = ifelse(is.na(real_value), 0, real_value))

# Calculate differences for existing fuels and regions
difference_data <- database_2020 %>%
  left_join(real_data_long, by = c("region", "type", "input")) %>%
  mutate(
    difference = real_value - total_value,  # Calculate difference
    difference = ifelse(is.na(difference), 0, difference)  # Replace NA differences with 0
  ) %>%
  select(region, type, input, difference,scenario)

# Step 7: Adjust values for the entire trajectory (2020 to 2070, 5-year intervals)
adjusted_data <- database_grouped %>%
  filter(year >= 2020 & year <= 2070 & year %% 5 == 0) %>%
  left_join(difference_data, by = c("region", "type", "input","scenario")) %>%
  mutate(
    adjusted_value = total_value + coalesce(difference, 0),  # Add difference to database values
    adjusted_value = ifelse(adjusted_value < 0, total_value, adjusted_value)  # Default to database value if negative
  ) %>%
  select(region, type, input, year, adjusted_value,scenario)

# Step 8: Ensure data for all years (2020-2070) is present
all_years <- expand.grid(
  region = unique(adjusted_data$region),
  type = unique(adjusted_data$type),
  input = unique(adjusted_data$input),
  year = seq(2020, 2070, by = 5)
)

adjusted_data <- all_years %>%
  left_join(adjusted_data, by = c("region", "type", "input", "year")) %>%
  mutate(
    adjusted_value = ifelse(is.na(adjusted_value), 0, adjusted_value)  # Replace missing years with 0
  ) %>%
  arrange(region, type, input, year)

# Step 9: Remove 'delivered biomass' and 'delivered coal' from the commercial sector
adjusted_data <- adjusted_data %>%
  filter(!(input %in% c("delivered biomass", "delivered coal", "traditional biomass") & type == "comm"))

# Step 10: Write the adjusted data to an Excel file
output_path <- "data/Building_Energy_Adjusted_all.xlsx"
write.xlsx(adjusted_data, output_path)



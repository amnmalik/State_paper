# Load required libraries
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(rgcam)

# Step 0: Load the database and set up the project
path <- 'C:/Users/Anurag/Desktop/GCAM_India_State_FinalVersion/output'
conn <- localDBConn(path, 'database_basexdb')
queryFileInput <- 'sp_queries.xml'
prj <- addScenario(conn, proj = 'StateIntegration_BAU_NZ.dat', scenario = c("BAU_Final"), queryFile = queryFileInput, warn.empty = TRUE, clobber = TRUE)
queries <- listQueries(prj)  # To check all queries
prj <- loadProject(proj = "StateIntegration_BAU_NZ.dat")

# Step 1: Read the real data for industrial fuel consumption in 2020 from Excel
real_data <- read_excel("C:/Users/Anurag/Downloads/StateIntegrationGraphs/CO2emissionsreal.xlsx", sheet = 'Sheet1')
real_data_long <- real_data %>%
  pivot_longer(cols = -States, names_to = "sector", values_to = "real_value")
real_data_long <- real_data_long %>%
  mutate(real_value = real_value / 10^6)

# Step 2: Fetch the database data for all years (do not filter for states here)
database_data <- getQuery(prj, query = 'CO2 emissions by sector', scenarios = "BAU_Final") %>%
  select(region, sector, year, value)

#database_data <- read_excel("D:/CEEW/CO2 crosscheck/emissions_asmi.xlsx", sheet = 'NZ') %>%
# pivot_longer(
#  cols = starts_with("19") | starts_with("20"),  # Pivot year columns
# names_to = "year",
#values_to = "value"
#  ) %>%
# mutate(year = as.integer(year)) %>%  # Ensure year is numeric
#filter(year >= 2020) %>%  # Filter for years >= 2020
#select(region, sector, year, value)  # Select desired columns


# Step 3: Filter database data to include only the regions (states) present in real_data
database_data_filtered <- database_data %>%
  filter(region %in% real_data_long$States)  # Ensure filtering for the states in real_data

# Step 4: Map sectors to types (comm/resid)
sector_to_type <- c(
  "Agriculture_enuse" = "Industry", "Chem_Fert_enuse" = "Industry",
  "Fisheries_enuse" = "Industry", "Food Processing_enuse" = "Industry",
  "Iron and Steel_enuse" = "Industry", "Non-Ferrous Metals_enuse" = "Industry",
  "Other Industries_enuse" = "Industry", "Paper and Pulp_enuse" = "Industry",
  "cement" = "Industry", "process heat cement" = "Industry",
  "trn_aviation_intl" = "Transport", "trn_freight_road" = "Transport", "trn_freight" = "Transport", "trn_shipping_intl" = "Transport",
  "trn_pass" = "Transport", "trn_pass_road" = "Transport", "trn_pass_road_LDV" = "Transport",
  "trn_pass_road_LDV_2W" = "Transport", "trn_pass_road_LDV_4W" = "Transport",
  "comm cooking" = "Building", "comm others" = "Building", "resid rural apploth" = "Building",
  "resid rural cooking" = "Building", "resid rural space heating" = "Building", "resid urban apploth" = "Building",
  "resid urban cooking" = "Building", "electricity" = "Power"
)

database_data_filtered <- database_data_filtered %>%
  mutate(type = sector_to_type[sector])  # Add type column

# Step 5: Aggregate database data by region, type, input, and year
database_grouped <- database_data_filtered %>%
  group_by(region, type, year) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")
database_grouped <- database_grouped %>%
  mutate(total_value = total_value * 3.67)

database_grouped <- database_grouped %>%
  filter(!is.na(type))

database_grouped_ex <- expand.grid(
  region = unique(database_grouped$region),
  type = unique(database_grouped$type),
  year = unique(database_grouped$year)
)

database_grouped <- database_grouped_ex %>%
  left_join(database_grouped, by = c("region", "type", "year"))

# Fill missing values in `real_data_long` with `real_value = 0`
database_grouped <- database_grouped %>%
  mutate(total_value = ifelse(is.na(total_value), 0, total_value)) %>%
  arrange(region, type, year)

# Step 6: Calculate the difference between database and real data for 2020
database_2020 <- database_grouped %>%
  filter(year == 2020)

# Add missing fuels or regions to real data with default `real_value = NA`
all_inputs <- expand.grid(
  region = unique(database_2020$region),
  type = unique(database_2020$type),
  year = unique(database_2020$year)
)

real_data_long <- all_inputs %>%
  left_join(real_data_long, by = c("region" = "States", "type" = "sector"))

# Fill missing values in `real_data_long` with `real_value = 0`
real_data_long <- real_data_long %>%
  mutate(real_value = ifelse(is.na(real_value), 0, real_value))

# Calculate differences for existing fuels and regions
difference_data <- database_2020 %>%
  left_join(real_data_long, by = c("region", "type")) %>%
  mutate(
    difference = real_value - total_value,  # Calculate difference
    difference = ifelse(is.na(difference), 0, difference)  # Replace NA differences with 0
  ) %>%
  select(region, type, difference)

# Step 6: Adjust database values for years starting from 2020 to 2070
adjusted_data <- database_grouped %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 2020 & year <= 2070 & year %% 5 == 0) %>%
  full_join(difference_data, by = c("region", "type")) %>%  # Join without "year"
  mutate(
    adjusted_value = case_when(
      type == "Industry" & year <= 2050 ~ total_value + coalesce(difference, 0),
      type == "Power" & year <= 2040 ~ total_value + coalesce(difference, 0),
      type == "Building" & year <= 2045 ~ total_value + coalesce(difference, 0),
      type == "Transport" & year <= 2040 ~ total_value + coalesce(difference, 0),
      TRUE ~ total_value  # Beyond the threshold years, just use total_value
    ),
    adjusted_value = if_else(adjusted_value <= 0, 0, adjusted_value)  # Prevent negatives
  ) %>%
  select(region, type, year, adjusted_value) %>%
  arrange(region, type, year)


# Arrange data
# Debugging: Check adjusted_data
cat("Adjusted Data:\n")
print(head(adjusted_data))

# Step 7: Write the adjusted data to an Excel file
output_path <- "C:/Users/Anurag/Downloads/allsector_emissions_BAU.xlsx"
write.xlsx(adjusted_data, output_path)


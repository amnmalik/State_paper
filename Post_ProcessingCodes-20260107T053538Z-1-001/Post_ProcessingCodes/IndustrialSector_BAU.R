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
real_data <- read_excel("C:/Users/Anurag/Downloads/StateIntegrationGraphs/IndustryReal.xlsx")

# Debugging: Check real_data structure
cat("Real Data:\n")
print(head(real_data))

# Reshape real data to long format for easier comparison
real_data_long <- real_data %>%
  pivot_longer(
    cols = -c(State, state_name), # Exclude `State` and `state_name` from pivoting
    names_to = "input",
    values_to = "real_value"
  )

# Debugging: Check real_data_long structure
cat("Real Data (Long Format):\n")
print(head(real_data_long))

# Step 2: Fetch data for all years from the database for industrial sectors
database_data <- getQuery(prj, query = 'final energy consumption by sector and fuel',scenario = "BAU_Final") %>%
  filter(sector %in% c("Agriculture_enuse", "Chem_Fert_enuse", "Fisheries_enuse", "Food Processing_enuse","Paper and Pulp_enuse",
                       "Iron and Steel_enuse", "Non-Ferrous Metals_enuse", "Other Industries_enuse", "cement",
                       "process heat cement")) %>%
  select(region, input, year, value)

# Debugging: Check database_data structure
cat("Database Data:\n")
print(head(database_data))

# Step 3: Filter database data to include only the regions (states) in real_data
database_data_filtered <- database_data %>%
  filter(region %in% real_data$State)  # Match with the `State` column from real_data

# Debugging: Check filtered database data
cat("Filtered Database Data:\n")
print(head(database_data_filtered))

# Step 4: Aggregate data across industries for each region, input, and year
database_aggregated <- database_data_filtered %>%
  group_by(region, input, year) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")

# Debugging: Check aggregated database data
cat("Aggregated Database Data:\n")
print(head(database_aggregated))

# Step 5: Filter data for 2020 and calculate the difference with real data
database_2020 <- database_aggregated %>%
  filter(year == 2020)

# Debugging: Check database_2020
cat("Database Data for 2020:\n")
print(head(database_2020))

difference_data <- database_2020 %>%
  left_join(real_data_long, by = c("region" = "State", "input" = "input")) %>%
  mutate(
    difference = real_value - total_value,  # Calculate the difference
    difference = ifelse(is.na(difference), 0, difference)  # Replace NA differences with 0
  ) %>%
  select(region, input, difference)

# Step 6: Adjust database values for years starting from 2020 to 2070
adjusted_data <- database_aggregated %>%
  filter(year >= 2020 & year <= 2070 & year %% 5 == 0) %>%  # Filter only for 2020-2070 at 5-year intervals
  left_join(difference_data, by = c("region", "input")) %>%
  mutate(
    adjusted_value = total_value + coalesce(difference, 0),  # Add the difference to database values
    adjusted_value = if_else(adjusted_value < 0, total_value, adjusted_value)
    ) %>%
  select(region, input, year, adjusted_value)  # Select only relevant columns

# Debugging: Check adjusted_data
cat("Adjusted Data:\n")
print(head(adjusted_data))

# Step 7: Write the adjusted data to an Excel file
output_path <- "C:/Users/Anurag/Downloads/adjusted_industry_BAU.xlsx"
write.xlsx(adjusted_data, output_path)


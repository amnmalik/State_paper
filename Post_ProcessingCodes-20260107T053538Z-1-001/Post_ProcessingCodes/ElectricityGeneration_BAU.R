# Load required libraries
library(rgcam)
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(ggplot2)

# Step 0: Load the database and set up project
path <- 'C:/Users/Anurag/Desktop/GCAM_India_State_FinalVersion/output'
conn <- localDBConn(path, 'database_basexdb')
queryFileInput <- 'sp_queries.xml'
prj <- addScenario(conn, proj = 'StateIntegration_BAU_NZ.dat', scenario = c("BAU_Final"), queryFile = queryFileInput, warn.empty = TRUE, clobber = TRUE)
queries <- listQueries(prj)
prj <- loadProject(proj = "StateIntegration_BAU_NZ.dat")

# Step 1: Read the real generation numbers for 2020 from Excel
real_data <- read_excel("C:/Users/Anurag/Downloads/StateIntegrationGraphs/elecgenbystate.xlsx")

# Reshape real data to long format
real_data_long <- real_data %>%
  pivot_longer(cols = -state, names_to = "subsector", values_to = "real_value")

# Step 2: Fetch data from GCAM project
database_data <- getQuery(prj, query = 'elec gen by subsector', scenario = "BAU_Final") %>%
  filter(year >= 2020) %>%
  select(region, subsector, year, value)

# Filter for relevant regions (states)
database_data_filtered <- database_data %>%
  filter(region %in% real_data$state)

# ⚠️ Deduplicate GCAM data: keep only one row per (region, subsector, year)
database_data_filtered <- database_data_filtered %>%
  group_by(region, subsector, year) %>%
  summarise(value = sum(value), .groups = "drop")

# Step 3: Calculate 2020 correction (difference between real and modelled values)
database_2020 <- database_data_filtered %>%
  filter(year == 2020) %>%
  left_join(real_data_long, by = c("region" = "state", "subsector")) %>%
  mutate(
    real_value = ifelse(is.na(real_value), value, real_value),
    difference = real_value - value
  ) %>%
  select(region, subsector, difference)

# ⚠️ Deduplicate 2020 correction table
database_2020_dedup <- database_2020 %>%
  distinct(region, subsector, .keep_all = TRUE)

# Step 4: Create full year-state-subsector matrix
years <- seq(2020, 2070, by = 5)
states <- unique(real_data$state)
subsectors <- unique(database_data_filtered$subsector)
complete_years <- expand.grid(state = states, subsector = subsectors, year = years)

# Step 5: Merge and adjust generation values
adjusted_data <- complete_years %>%
  left_join(database_data_filtered, by = c("state" = "region", "subsector", "year")) %>%
  left_join(database_2020_dedup, by = c("state" = "region", "subsector")) %>%
  mutate(
    value = coalesce(value, 0),
    adjusted_value = value + coalesce(difference, 0),
    adjusted_value = if_else(adjusted_value < 0, value, adjusted_value)
  ) %>%
  filter(subsector != "rooftop_pv") %>%
  select(state, subsector, year, adjusted_value) %>%
  arrange(state, subsector, year)

# Step 6: Export adjusted data
write.xlsx(adjusted_data, "C:/Users/Anurag/Downloads/adjusted_generation_forecast_BAU.xlsx")

# Step 7: Filter selected states and subsectors
selected_states <- c("KA", "TN")
selected_techs <- c("solar", "wind")

filtered_data <- adjusted_data %>%
  filter(state %in% selected_states, subsector %in% selected_techs)

# Step 8: Export filtered data
wb <- createWorkbook()
addWorksheet(wb, "finaloutput")
writeData(wb, "finaloutput", filtered_data)
saveWorkbook(wb, "C:/Users/Anurag/Downloads/final_output.xlsx", overwrite = TRUE)

# Step 9: Area Plots

# Karnataka
ggplot(filtered_data %>% filter(state == "KA"), aes(x = year, y = adjusted_value, fill = subsector)) +
  geom_area(alpha = 0.6) +
  labs(title = "Adjusted Generation for Karnataka (KA)", x = "Year", y = "Adjusted Value") +
  scale_fill_manual(values = c("solar" = "yellow", "wind" = "blue")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Tamil Nadu
ggplot(filtered_data %>% filter(state == "TN"), aes(x = year, y = adjusted_value, fill = subsector)) +
  geom_area(alpha = 0.6) +
  labs(title = "Adjusted Generation for Tamil Nadu (TN)", x = "Year", y = "Adjusted Value") +
  scale_fill_manual(values = c("solar" = "yellow", "wind" = "blue")) +
  theme_minimal() +
  theme(legend.title = element_blank())

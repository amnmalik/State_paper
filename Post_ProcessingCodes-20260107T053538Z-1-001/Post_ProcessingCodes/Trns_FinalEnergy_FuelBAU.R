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

# Step 1: Read the real generation numbers for 2020 from Excel
real_data_trn <- read_excel("C:/Users/Anurag/Downloads/StateIntegrationGraphs/realdatatransportsector.xlsx")

# Reshape real data to long format for easier comparison
real_data_long_trn <- real_data_trn %>%
  pivot_longer(cols = -c(state, state_name), names_to = "input", values_to = "real_value") %>%
  distinct(state, input, .keep_all = TRUE)  # Remove any duplicates

# Step 2: Fetch GCAM data for transport final energy
database_data_trn <- getQuery(prj, query = 'transport final energy by fuel',scenario = "BAU_Final") %>%
  filter(year >= 2020) %>%
  select(region, input, year, value)

# Filter GCAM data to include only the regions present in real data
database_data_filtered_trn <- database_data_trn %>%
  filter(region %in% real_data_trn$state) %>%
  distinct(region, input, year, .keep_all = TRUE)  # Ensure no duplicate records

# Step 3: Compute 2020 adjustment factors
database_2020_trn <- database_data_filtered_trn %>%
  filter(year == 2020) %>%
  distinct(region, input, .keep_all = TRUE) %>%
  left_join(real_data_long_trn, by = c("region" = "state", "input")) %>%
  mutate(
    real_value = ifelse(is.na(real_value), value, real_value),
    difference = real_value - value
  ) %>%
  select(region, input, difference)

# Step 4: Generate all combinations for full year-state-input coverage
years <- seq(2020, 2070, by = 5)
states <- unique(real_data_trn$state)
inputs <- unique(database_data_filtered_trn$input)

complete_years_trn <- expand.grid(state = states, input = inputs, year = years)

# Step 5: Apply adjustment across all years
adjusted_data_trn <- complete_years_trn %>%
  left_join(database_data_filtered_trn, by = c("state" = "region", "input", "year")) %>%
  left_join(database_2020_trn, by = c("state" = "region", "input")) %>%
  mutate(
    value = coalesce(value, 0),
    adjusted_value = value + coalesce(difference, 0),
    adjusted_value = if_else(adjusted_value < 0, value, adjusted_value)
  ) %>%
  select(state, input, year, adjusted_value) %>%
  arrange(state, input, year)

# Step 6: Export complete adjusted data
write.xlsx(adjusted_data_trn, "C:/Users/Anurag/Downloads/adjusted_forecast_trn_BAU.xlsx")

# Step 7: Filter for plotting and selected export
selected_states <- c("KA", "TN")
selected_fuels <- c("refined liquids enduse")

filtered_data_trn <- adjusted_data_trn %>%
  filter(state %in% selected_states, input %in% selected_fuels)

wb <- createWorkbook()
addWorksheet(wb, "finaloutput_trn")
writeData(wb, "finaloutput_trn", filtered_data_trn)
saveWorkbook(wb, "C:/Users/Anurag/Downloads/final_output_trn.xlsx", overwrite = TRUE)

# Step 8: Plots for KA and TN
ggplot(filtered_data_trn %>% filter(state == "KA"), aes(x = year, y = adjusted_value, fill = input)) +
  geom_area(alpha = 0.6) +
  labs(title = "Adjusted Generation for Karnataka (KA)", x = "Year", y = "Adjusted Value") +
  scale_fill_manual(values = c("refined liquids enduse" = "yellow")) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot(filtered_data_trn %>% filter(state == "TN"), aes(x = year, y = adjusted_value, fill = input)) +
  geom_area(alpha = 0.6) +
  labs(title = "Adjusted Generation for Tamil Nadu (TN)", x = "Year", y = "Adjusted Value") +
  scale_fill_manual(values = c("refined liquids enduse" = "yellow")) +
  theme_minimal() +
  theme(legend.title = element_blank())

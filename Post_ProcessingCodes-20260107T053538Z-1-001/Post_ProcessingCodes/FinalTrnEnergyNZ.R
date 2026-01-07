library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(ggplot2)

# Step 0: Load the database and set up project
path <- 'C:/Users/Anurag/Desktop/GCAM_India_State_FinalVersion/output'
conn <- localDBConn(path, 'database_basexdb')
queryFileInput <- 'sp_queries.xml'
prj <- addScenario(conn, proj = 'StateIntegration_BAU_NZ.dat', scenario = c("NZ_Final"), queryFile = queryFileInput, warn.empty = TRUE, clobber = TRUE)

# Step 1: Read the real generation numbers for 2020 from Excel
real_data_trn <- read_excel("C:/Users/Anurag/Downloads/StateIntegrationGraphs/realdatatransportsector.xlsx")

# Ensure uniqueness to avoid many-to-many join issues
real_data_long_trn <- real_data_trn %>%
  pivot_longer(cols = -c(state, state_name), names_to = "input", values_to = "real_value") %>%
  filter(!is.na(real_value)) %>%
  distinct(state, input, .keep_all = TRUE)

# Step 2: Fetch GCAM data for 2020 onwards
database_data_trn <- getQuery(prj, query = 'transport final energy by fuel',scenario = "NZ_Final") %>%
  filter(year >= 2020) %>%
  select(region, input, year, value)

# Step 3: Filter model data for only states present in real data
database_data_filtered_trn <- database_data_trn %>%
  filter(region %in% real_data_trn$state)

# Step 4: Calculate adjustment factor for 2020
database_2020_trn <- database_data_filtered_trn %>%
  filter(year == 2020)

# Join only on unique rows to avoid row explosion
diff_data_trn <- database_2020_trn %>%
  inner_join(real_data_long_trn, by = c("region" = "state", "input")) %>%
  mutate(difference = real_value - value) %>%
  group_by(region, input) %>%
  summarise(difference = mean(difference, na.rm = TRUE), .groups = "drop")  # FIX APPLIED

# Step 5: Apply adjustments to all years
years <- seq(2020, 2070, by = 5)
states <- unique(real_data_trn$state)
inputs <- unique(database_data_filtered_trn$input)

complete_years_trn <- expand.grid(state = states, input = inputs, year = years)

adjusted_data_trn <- complete_years_trn %>%
  left_join(database_data_filtered_trn, by = c("state" = "region", "input", "year")) %>%
  left_join(diff_data_trn, by = c("state" = "region", "input")) %>%
  mutate(
    value = coalesce(value, 0),
    difference = coalesce(difference, 0),
    adjusted_value = value + difference,
    adjusted_value = if_else(adjusted_value < 0, value, adjusted_value)
  ) %>%
  select(state, input, year, adjusted_value) %>%
  arrange(state, input, year)

# Step 6: Save full adjusted data
write.xlsx(adjusted_data_trn, "C:/Users/Anurag/Downloads/adjustedenergy_trn_NZ.xlsx", overwrite = TRUE)

# Step 7: Save filtered subset for plotting
selected_states <- c("KA", "TN")
selected_fuels <- c("refined liquids enduse")

filtered_data_trn <- adjusted_data_trn %>%
  filter(state %in% selected_states, input %in% selected_fuels)

wb <- createWorkbook()
addWorksheet(wb, "finaloutput_trn")
writeData(wb, "finaloutput_trn", filtered_data_trn)
saveWorkbook(wb, "C:/Users/Anurag/Downloads/adjusted_energy_trn_NZ.xlsx", overwrite = TRUE)

# Step 8: Plot
ggplot(filtered_data_trn %>% filter(state == "KA"), aes(x = year, y = adjusted_value, fill = input)) +
  geom_area(alpha = 0.6) +
  labs(title = "Adjusted Energy")

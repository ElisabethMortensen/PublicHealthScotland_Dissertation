# Data Preprocessing 

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Inputs required from working directory (edit paths if different):
# monthly_attendance_demographics.csv (PHS Monthly A&E attendance by demographics)
# datazone_pop_estimates.csv (NHS, Data Zone (2011) population estimates)
# SIMD2020_popweighted.csv (PHS SIMD 2020v2, population-weighted)

# load and clean monthly attendance demographic data
demographics <- read_csv("monthly_attendance_demographics.csv", show_col_types = FALSE)

# drop rows with unknown age, sex, or deprivation
demographics_known <- filter(demographics, !is.na(Age), !is.na(Sex), !is.na(Deprivation))
demographics_known$Deprivation <- as.integer(demographics_known$Deprivation)
demographics_known$Year <- as.integer(str_sub(as.character(demographics_known$Month), 1, 4))
demographics_known$MonthNum <- as.integer(str_sub(as.character(demographics_known$Month), 5, 6))

demographics_grouped <- group_by(demographics_known, Year, MonthNum, DepartmentType, HBT, Age, Sex, Deprivation)
demographics_clean <- summarise(demographics_grouped, NumberOfAttendances = sum(NumberOfAttendances), .groups = "drop")

# load data zone population estimates (2001 to 2024)
datazone_pop <- read_csv("datazone_pop_estimates.csv", show_col_types = FALSE)

# match age groups to those in monthly_attendance_demographics.csv
age_cols <- function(from, to) paste0("Age", from:to)
datazone_filtered <- filter(datazone_pop, Sex %in% c("Male", "Female"))
datazone_filtered <- filter(datazone_filtered, str_starts(DataZone, "S01"))
datazone_filtered$`Under 18` <- rowSums(datazone_filtered[, age_cols(0, 17)])
datazone_filtered$`18-24` <- rowSums(datazone_filtered[, age_cols(18, 24)])
datazone_filtered$`25-39` <- rowSums(datazone_filtered[, age_cols(25, 39)])
datazone_filtered$`40-64` <- rowSums(datazone_filtered[, age_cols(40, 64)])
datazone_filtered$`65-74` <- rowSums(datazone_filtered[, age_cols(65, 74)])
datazone_filtered$`75 plus` <- rowSums(datazone_filtered[, age_cols(75, 89)]) + datazone_filtered$Age90plus

datazone_bands_wide <- datazone_filtered[, c("Year", "DataZone", "Sex", "Under 18", "18-24", "25-39", "40-64", "65-74", "75 plus")]
datazone_bands <- pivot_longer(datazone_bands_wide, 
                               cols = c(`Under 18`, `18-24`, `25-39`, `40-64`, `65-74`, `75 plus`),
                               names_to = "Age",
                               values_to = "Population")

# load SIMD lookup data and attach corresponding Health Board and deprivation Quintile to each data zone
simd <- read_csv("SIMD2020_popweighted.csv", show_col_types = FALSE)
simd_lookup <- data.frame(DataZone = simd$DataZone, HB = simd$HB, Deprivation = simd$SIMD2020V2CountryQuintile)
population_joined <- left_join(datazone_bands, simd_lookup, by = "DataZone")

# aggregate population to Year, HBT, Age, Sex, and Deprivation
population_grouped <- group_by(population_joined, Year, HB, Age, Sex, Deprivation)
population_agg <- summarise(population_grouped, Population = sum(Population), .groups = "drop")


# data zone population estimates currently only run to 2024 but attendance data runs later to 2026
# Carry population estimates forward for remaining months in 2025-2026
max_pop_year <- max(population_agg$Year)
max_attendance_year <- max(demographics_clean$Year)
if (max_attendance_year > max_pop_year) {
  extension_years <- (max_pop_year + 1):max_attendance_year
  latest_pop <- filter(population_agg, Year == max_pop_year)
  carried_forward_list <- list()
  for (i in seq_along(extension_years)) {
    this_year_pop <- latest_pop
    this_year_pop$Year <- extension_years[i]
    carried_forward_list[[i]] <- this_year_pop
  }
  carried_forward <- bind_rows(carried_forward_list)
  population_agg <- bind_rows(population_agg, carried_forward)
}


# merge attendance counts with population denominators
merged <- left_join(demographics_clean, population_agg, by = c("Year" = "Year", "HBT" = "HB", "Age" = "Age", "Sex" = "Sex", "Deprivation" = "Deprivation"))
structural_zero_pop <- is.na(merged$Population)
merged$Population[is.na(merged$Population)] <- 0


# compute attendance rate (attendances per 1,000 population) and prepare offset for model
model_data <- filter(merged, !structural_zero_pop)
model_data$Rate <- model_data$NumberOfAttendances / model_data$Population * 1000 
model_data$log_offset <- log(model_data$Population)

#save data set 
# write_csv(model_data, "ae_attendance_rates_merged.csv")

#other data set saved with department types merged
model_data_grouped_notype <- group_by(model_data, Year, MonthNum, HBT, Age, Sex, Deprivation)
model_data_notype <- summarise(model_data_grouped_notype,
                               NumberOfAttendances = sum(NumberOfAttendances),
                               Population = first(Population),
                               .groups = "drop")
model_data_notype$Rate <- model_data_notype$NumberOfAttendances / model_data_notype$Population * 1000
model_data_notype$log_offset <- log(model_data_notype$Population)
write_csv(model_data_notype, "ae_attendance_rates_merged_notype.csv")


# focus on post COVID-19 pandemic months
model_data_notype$Date <- as.Date(paste(model_data_notype$Year, model_data_notype$MonthNum, "01", sep = "-"))
cutoff_date <- as.Date("2021-06-01")
model_data_notype_post <- model_data_notype[model_data_notype$Date >= cutoff_date, ]
write_csv(model_data_notype_post, "ae_attendance_rates_postcovid.csv")


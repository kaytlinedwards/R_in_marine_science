# ============================================================
# Workshop 2 - Keystone Exercise: Estuary Fish Survey
# Data Rescue Mission
# ============================================================

# Phase 1: Ingestion and Decontamination

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)

# Import the estuary metadata
estuary_metadata <- read_csv("data/workshop2/estuary_metadata.csv")

# Import the water quality sensor data
estuary_sonde <- read_csv("data/workshop2/estuary_sonde_data.csv")

# Import the species dictionary
species_dictionary <- read_csv("data/workshop2/species_dictionary.csv")

# Check the sheet names in the fish catch Excel file
excel_sheets("data/workshop2/estuary_catch_log.xlsx")

# Get the names of all sheets in the catch log
catch_sheets <- excel_sheets("data/workshop2/estuary_catch_log.xlsx")

# Read every sheet and combine them into one dataset
estuary_catch <- catch_sheets |>
  map_dfr(
    ~ read_excel(
      "data/workshop2/estuary_catch_log.xlsx",
      sheet = .x
    ),
    .id = "sheet"
  )

# View the combined catch dataset
print(estuary_catch)

# Inspect the structure of all four datasets
glimpse(estuary_catch)
glimpse(estuary_metadata)
glimpse(estuary_sonde)
glimpse(species_dictionary)

# Standardise site and species names in the catch data
estuary_catch <- estuary_catch |>
  mutate(
    site = str_to_lower(site),
    site = str_replace_all(site, " ", "_"),
    species = str_to_lower(species),
    species = str_replace_all(species, " ", "_")
  )

# Standardise site names in the metadata
estuary_sonde <- estuary_sonde |>
  mutate(
    site = str_to_lower(site),
    site = str_replace_all(site, " ", "_")
  )

# Standardise site names in the sonde data
estuary_sonde <- estuary_sonde |>
  mutate(
    site = str_to_lower(site),
    site = str_replace_all(site, " ", "_")
  )

head(estuary_sonde$timestamp, 20)

# Clean the water quality sensor data
estuary_sonde <- estuary_sonde |>
  mutate(
    # Convert timestamp text into a proper date-time
    timestamp = dmy_hm(timestamp),
    
    # Replace the sensor error value -999 with NA
    turbidity = na_if(turbidity, -999)
  )

# Check the cleaned sensor data
glimpse(estuary_sonde)

# Check how many failed turbidity readings became NA
sum(is.na(estuary_sonde$turbidity))

# ============================================================
# Phase 2: The Relational Architecture
# ============================================================
# Summarise the 15-minute sensor data into daily values
daily_sonde <- estuary_sonde |>
  mutate(
    # Extract the calendar day from each timestamp
    date = floor_date(timestamp, unit = "day")
  ) |>
  group_by(site, date) |>
  summarise(
    mean_temperature = mean(temperature, na.rm = TRUE),
    mean_salinity = mean(salinity, na.rm = TRUE),
    .groups = "drop"
  )

# View the daily sensor summary
print(daily_sonde)

# Add scientific species names to the catch data
catch_translated <- estuary_catch |>
  left_join(
    species_dictionary,
    by = c("species" = "common_name")
  )

# Check the translated catch data
glimpse(catch_translated)

# View the common and scientific names together
distinct(catch_translated, species, scientific_name)

# Join the catch data with daily water quality data
master_data <- catch_translated |>
  left_join(
    daily_sonde,
    by = c("site", "date")
  )

# Add the spatial metadata and estuary zones
master_data <- master_data |>
  left_join(
    estuary_metadata,
    by = c("site" = "site_name")
  )

# Inspect the completed master dataset
glimpse(master_data)

# Check whether important joined information is missing
sum(is.na(master_data$mean_temperature))
sum(is.na(master_data$mean_salinity))
sum(is.na(master_data$zone))
sum(is.na(master_data$scientific_name))

# ============================================================
# Phase 3: The Zero-Catch Framework
# ============================================================


# Create all species x site x sampling date combinations
master_complete <- master_data |>
  complete(
    scientific_name,
    site,
    date
  )

# Convert missing catch counts in the completed framework to zero
master_complete <- master_complete |>
  mutate(
    count = coalesce(count, 0)
  )

# Check the completed dataset
glimpse(master_complete)

# Confirm there are no missing catch counts
sum(is.na(master_complete$count))

# Restore environmental and spatial information
# for the new zero-catch rows

master_complete <- master_complete |>
  # Remove columns that contain NAs in the newly created rows
  select(
    -mean_temperature,
    -mean_salinity,
    -lat,
    -lon,
    -zone
  ) |>
  
  # Reattach daily water quality using site + date
  left_join(
    daily_sonde,
    by = c("site", "date")
  ) |>
  
  # Reattach spatial information using site
  left_join(
    estuary_metadata,
    by = c("site" = "site_name")
  )

# Check that important variables are now complete
sum(is.na(master_complete$count))
sum(is.na(master_complete$mean_temperature))
sum(is.na(master_complete$mean_salinity))
sum(is.na(master_complete$lat))
sum(is.na(master_complete$lon))
sum(is.na(master_complete$zone))

# Restore common species names for zero-catch rows
master_complete <- master_complete |>
  select(-species) |>
  left_join(
    species_dictionary,
    by = "scientific_name"
  ) |>
  rename(species = common_name)

# Remove the temporary Excel sheet tracking column
master_complete <- master_complete |>
  select(-sheet)

glimpse(master_complete)

sum(is.na(master_complete$species))
sum(is.na(master_complete$count))
sum(is.na(master_complete$mean_temperature))
sum(is.na(master_complete$mean_salinity))
sum(is.na(master_complete$zone))

# ============================================================
# Phase 4: Statistical Extraction
# ============================================================

# Check how many salinity observations are available in each group
master_complete |>
  group_by(scientific_name, zone) |>
  summarise(
    n_rows = n(),
    n_salinity = sum(!is.na(mean_salinity)),
    unique_salinity = n_distinct(mean_salinity, na.rm = TRUE),
    .groups = "drop"
  )

# Check the data type of mean_salinity
class(master_complete$mean_salinity)

# Look at the daily salinity values directly
master_complete |>
  select(scientific_name, site, date, zone, mean_salinity) |>
  distinct() |>
  print(n = 30)

# Calculate summary statistics for each species and estuary zone
estuary_summary <- master_complete |>
  group_by(scientific_name, zone) |>
  summarise(
    mean_catch = mean(count, na.rm = TRUE),
    sd_catch = sd(count, na.rm = TRUE),
    zone_mean_salinity = mean(mean_salinity, na.rm = TRUE),
    zone_sd_salinity = sd(mean_salinity, na.rm = TRUE),
    .groups = "drop"
  )

# View the statistical summary table
print(estuary_summary)

# ============================================================
# Phase 5: Visual Communication
# ============================================================

# Plot fish abundance along the salinity gradient
estuary_plot <- master_complete |>
  ggplot(aes(x = mean_salinity, y = count)) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  facet_wrap(~ scientific_name) +
  labs(
    title = "Fish Abundance Along the Estuary Salinity Gradient",
    x = "Mean Daily Salinity",
    y = "Fish Abundance (count)"
  ) +
  theme_minimal()

# Display the plot
estuary_plot

# Order estuary zones from upstream to downstream
master_complete <- master_complete |>
  mutate(
    zone = factor(
      zone,
      levels = c("Upstream", "Middle", "Downstream", "Marine")
    )
  )

# Create publication-ready salinity gradient plot
estuary_plot <- master_complete |>
  ggplot(
    aes(
      x = mean_salinity,
      y = count,
      colour = zone
    )
  ) +
  geom_point(
    alpha = 0.7,
    size = 2
  ) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = TRUE,
    colour = "black"
  ) +
  facet_wrap(~ scientific_name) +
  labs(
    title = "Fish Abundance Along the Estuary Salinity Gradient",
    x = "Mean Daily Salinity",
    y = "Fish Abundance (count)",
    colour = "Estuary Zone"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "italic"),
    legend.position = "right"
  )

# Display the final plot
estuary_plot

# ============================================================
# Final Quality-Control Checks
# ============================================================

# Confirm no missing catch counts remain
sum(is.na(master_complete$count))

# Confirm no legacy -999 sensor values remain
sum(estuary_sonde$turbidity == -999, na.rm = TRUE)

# Confirm every row has a scientific species name
sum(is.na(master_complete$scientific_name))

# Confirm environmental information is complete
sum(is.na(master_complete$mean_temperature))

# ============================================================
# Save Final Outputs
# ============================================================

# Save the clean master dataset as a CSV file
write_csv(
  master_complete,
  "outputs/estuary_fish_master_dataset.csv"
)
          
          
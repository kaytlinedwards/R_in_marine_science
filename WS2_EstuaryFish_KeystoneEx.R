# Workshop 2 - Keystone Exercise: Estuary Fish Survey
# Data Rescue Mission
# Kaytlin Edwards
  
# ============================================================
# Phase 1: Data Ingestion and Decontamination
# ============================================================

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)

# Load Main Dataset - Estuary Catch Log
# The catch data is split across 4 tabs in one excel file Load indivudally
# Load the four Excel sheets containing fish catch records
catch1 <- read_excel(here::here("data/workshop2/estuary_catch_log.xlsx"), sheet = "Lower_ross")
catch2 <- read_excel(here::here("data/workshop2/estuary_catch_log.xlsx"), sheet = 2)
catch3 <- read_excel(here::here("data/workshop2/estuary_catch_log.xlsx"), sheet = 3)
catch4 <- read_excel(here::here("data/workshop2/estuary_catch_log.xlsx"), sheet = 4)

# Inspect individual tabs
# Here we are looking for anomalies between workbooks and any variable naming issues
catch1
catch2
catch3
catch4 # Clear differences in naming, but will fix later once we combine them into one dataset

# Bind rows together into one dataset
catch_data <- bind_rows(catch1, catch2, catch3, catch4)
rm(catch1, catch2, catch3, catch4) # Clean up environment pane/ remove catch 1-4 data

# Review and check catch_data now
glimpse(catch_data)
head(catch_data)
str(catch_data)
print(catch_data)

# Check how many levels of site and species we have 
# First as distinct values
catch_data |> 
  distinct(site)

catch_data |> 
  distinct(species)
### looking at distrinct values, we can wee that there are four species and four sites total, which look to be located at different points on the Ross River. We also have some naming issues that will need to be standardized later on.


# Load metadata
metadata <- read_csv("data/workshop2/estuary_metadata.csv")

# Review and check metadata
head(metadata)
glimpse(metadata)
print(metadata)

# Fix site names in data and metadata and convert character to factor variables
catch_data <- catch_data |>
  mutate(
    site = str_to_lower(str_trim(site)),
    site = str_replace_all(site, " ", "_")) |> 
  # Convert all chr to fct
  mutate(across(where(is.character), as_factor)
  )

metadata <- metadata |>
  mutate(
    site_name = str_to_lower(str_trim(site_name)),
    site_name = str_replace_all(site_name, " ", "_")) |> 
  # Convert all chr to fct
  mutate(across(where(is.character), as_factor)
  )

# Recheck site names
catch_data |> 
  str()
# catch_data |> 
#   distinct(site)
str(metadata) 
#### Site names have been fixed but we see that there are still some issues with the species names. We will need to fix those after joining our species dictionary with the common names in the dataset.

# Load Taxonomic Dictionary
species_dict <- read_csv("data/workshop2/species_dictionary.csv")

head(species_dict) # Check
#### We'll need to check the common names in the catch log match those in the taxonomic dictionary before we do the join.

# Check that the common names in the catch log match those in the taxonomic dictionary befor we join
catch_data |> 
  distinct(species) # Can see these are different to the dictionary

species_dict |>
  distinct(common_name)

###  Standardise species (common name) variable in the catch log to match dictionary
catch_data <- catch_data |>
  mutate(
    species = str_to_lower(str_trim(species)),
    species = str_replace_all(species, " ", "_")
  )

## check
catch_data |> 
  distinct(species) # Now matches the dictionary!

## Create new variable ("scientific_name") in catch log by joining with taxonomic dictionary 
catch_data <- catch_data |>
  mutate(common_name = species) |>
  left_join(species_dict, by = join_by(common_name))

## Check catch log now
glimpse(catch_data) # names are now fixed!

# Load the sonde data/ sensor data
source(here::here("code/diw_sensor.R"))

# Join catch data with metadata and sensor data
catch_data_final <- catch_data |>
  left_join(metadata, by = join_by(site == site_name)) |>
  left_join(sensor_daily, by = join_by(site, date))

## Review and Check
glimpse(catch_data_final)

# Wrangle final dataset
catch_data_final <- 
  catch_data_final |> 
  complete(
    nesting(site, date, 
            # lat, lon, 
            zone, mean_temp, mean_salinity, mean_turbidity),
    scientific_name
  ) |>
  mutate(
    # Convert the resulting NA counts to 0
    count = coalesce(count, 0)
  )

# ======== MAIN DATA WRANGLE DONE!! ========

### First check that there is an actual difference in env vars among zones

#### Salinity
# Let's make a table
sal_sum <- 
  catch_data_final |>
  group_by(zone) |>
  summarise(
    # sem_salinity = sd(mean_salinity) / sqrt(n()),
    sd_salinity = sd(mean_salinity, na.rm = TRUE),
    median_salinity = median(mean_salinity, na.rm = TRUE),
    .groups = "drop"
  )

# And from this a plot
sal_sum |> 
  ggplot(aes(x = zone, y = median_salinity)) +
  geom_point() +
  geom_errorbar(aes(ymin = median_salinity - sd_salinity, ymax = median_salinity + sd_salinity), width = 0.2) +
  theme_minimal() +
  labs(#title = "Median Salinity by Zone with Standard Deviation Error Bars",
    x = "Zone",
    y = "Median Salinity (ppt)")

### We see clear differences in salinity as we would expect between upstream, middle stream, downstream, and marine zones
### Turbidity

turb_sum <- 
  catch_data_final |>
  group_by(zone) |>
  summarise(
    sd_turbidity = sd(mean_turbidity, na.rm = TRUE),
    median_turbidity = median(mean_turbidity, na.rm = TRUE),
    .groups = "drop"
  )

turb_sum |>
  ggplot(aes(x = zone, y = median_turbidity)) +
  geom_point() +
  geom_errorbar(aes(ymin = median_turbidity - sd_turbidity, ymax = median_turbidity + sd_turbidity), width = 0.2) +
  theme_minimal() +
  labs(#title = "Median Turbidity by Zone with Standard Deviation Error Bars",
    x = "Zone",
    y = "Median Turbidity (NTU)")

#### median turbidity levels constant across zones

### And temperature
temp_sum <- 
  catch_data_final |>
  group_by(zone) |>
  summarise(
    sd_temperature = sd(mean_temp, na.rm = TRUE),
    median_temperature = median(mean_temp, na.rm = TRUE),
    .groups = "drop"
  )

temp_sum |>
  ggplot(aes(x = zone, y = median_temperature)) +
  geom_point() +
  geom_errorbar(aes(ymin = median_temperature - sd_temperature, ymax = median_temperature + sd_temperature), width = 0.2) +
  theme_minimal() +
  labs(#title = "Median Temperature by Zone with Standard Deviation Error Bars",
    x = "Zone",
    y = "Median Temperature (°C)")

#### Constant temperatures, small differences only 

####OK!! so we'll continue with salinity as our main env var of interest, but we know that there are also differences in turbidity and temperature across the zones, which could also be influencing fish abundance patterns. We can discuss this in the interpretation of our results and consider potential confounding effects of these other environmental variables in our discussion.



#### Remake salinity plot nicely for publication
#| fig-cap: "Salinity Gradient across zones. Ross River Estuary System, Townsville, NQ, Australia May 2026."

sal_sum |> 
  ggplot(aes(x = zone, y = median_salinity)) +
  geom_point() +
  geom_errorbar(aes(ymin = median_salinity - sd_salinity, ymax = median_salinity + sd_salinity), width = 0.2) +
  theme_minimal() +
  labs(#title = "Median Salinity by Zone with Standard Deviation Error Bars",
    x = "Zone",
    y = "Median Salinity (ppt)")

# Now we can turn to patterns in fish abundance
# Calculate summary stats 
fish_stats <- catch_data_final |>
  group_by(zone, scientific_name) |>
  summarise(
    mean_catch = mean(count),
    sem_catch = sd(count) / sqrt(n()),
    .groups = "drop"
  )

# And plot
fish_stats |> 
  ggplot(aes(x = zone, y = mean_catch, fill = scientific_name)) +
  geom_line(group = 1) +
  # add in points from master data
  geom_point(data = catch_data_final, aes(x = zone, y = count),
             position = position_jitter(width = 0.2), alpha = 0.2, size = 1, inherit.aes = FALSE) + 
  geom_point(position = "dodge", size = 4, pch = 23) +
  # SE pretty narrow, potentially not worth plotting but let's viewer see that included
  geom_errorbar(aes(ymin = mean_catch - sem_catch, ymax = mean_catch + sem_catch), 
                position = position_dodge(0.9), width = 0.2) + 
  facet_wrap(~ scientific_name, scales = "fixed") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "italic", size = 10, colour = "grey20"), # spp names in italic
    # legend.position = "bottom"
    legend.position = "none" # legend unnecessary
  ) +
  labs(
    #title = "Predatory Fish Abundance across Estuarine Salinity Gradients", # default ggplot title appears on top
    #subtitle = "Ross River Estuary System, May 2026",
    x = "Spatial Zone",
    # scientific label for y axis of n day -1 use regex to get superscript
    y = expression("Abundance (n day"^-1*")"),
    #  y = "Daily abundance (n day^-1)"
    y = "Abundance (per day)"
  )

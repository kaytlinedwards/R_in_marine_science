# Script to wrangle sensor data

library(tidyverse)

#  Load sensor data, 
sensor_raw <- read_csv(here::here("data/workshop2/estuary_sonde_data.csv"))

# Check sensor data
head(sensor_raw)
str(sensor_raw)
glimpse(sensor_raw)
summary(sensor_raw)


### Clean up sensor data
sensor_data <- 
  sensor_raw |>
  mutate(
    # Parse the messy character string (Day/Month/Year Hour:Minute)
    datetime = dmy_hm(timestamp),
    # Convert the -999.0 hardware error to a true NA
    turbidity = na_if(turbidity, -999),
    # Convert negative salinity values to na
    salinity = ifelse(salinity < 0, NA, salinity),
  )

# Summarise 15-minute sonde data into daily means
sensor_daily <- 
  sensor_data |>
  mutate(date = as_date(floor_date(datetime, "day"))) |>
  group_by(site, date) |>
  summarise(
    mean_temp = mean(temperature, na.rm = TRUE),
    mean_salinity = mean(salinity, na.rm = TRUE),
    mean_turbidity = mean(turbidity, na.rm = TRUE),
    .groups = "drop")






# Load the primary data science framework and Excel import library
library(tidyverse)
library(readxl)

# Practice Import A: Loading a standard comma-separated plain text file
benthic_cover <- read_csv(here::here("data/workshop1/reef_cover_log.csv"))

# Practice Import B: Parsing a tab-separated telemetry instrument array string
acoustic_stream <- read_tsv(
  here::here("data/workshop1/acoustic_telemetry_stream.txt")
)

# Practice Import C: Targeting a specific sheet in a multi-tab Excel spreadsheet
fisheries_annual <- read_excel(
  here::here("data/workshop1/fish_catch_data.xlsx"),
  sheet = "Commercial_2026"
)

# Read in mangrove_data
# Use args within read_csv to skip headers and declare missing flags
mangrove_data <- read_csv(
  here::here("data/workshop1/mangrove_survey_raw.csv"),
  skip = 5,
  na = c(".", "NA", "9999", "ND", "blank")
)

# 1.5 Data frame architectures: Tibbles versus legacy tables

# Force the modern tibble into a legacy base R data frame
benthic_cover_df <- as.data.frame(benthic_cover)

# Print the old-style data frame
print(benthic_cover_df)

# Compare with the modern tibble
print(benthic_cover)


# 1.6 Wrangling out ecological signals using Palmer Penguins

# Load the Palmer Penguins package
library(palmerpenguins)

# Load the penguins dataset into R
data("penguins")

# Look at the structure of the dataset
glimpse(penguins)

# Compare with the base (old) R version
str(penguins)

# Generate a summary of the dataset
summary(penguins)

# 1.7 Foundational grammar
# 1.7.1 select() = choose COLUMNS
# Select only the penguin measurements we care about
morphology_metrics <- select(
  penguins,
  species,
  bill_length_mm,
  bill_depth_mm,
  body_mass_g
)

# Look at the result
glimpse(morphology_metrics)

# Retain a continuous block of attributes using the colon operator
spatial_block <- select(penguins, species:island) # ":" means "from this column THROUGH this column."So instead of individually typing every column between species and island, R grabs that whole continuous block.

# Keep everything EXCEPT the year column using the minus sign
clean_scientific_fields <- select(penguins, -year)
summary(clean_scientific_fields)


# 1.7.2 filter() = choose ROWS

# Keep ONLY Adelie penguins
adelie_cohort <- filter(penguins, species == "Adelie")

# Keep ONLY penguins heavier than 4500 g
heavy_penguins <- filter(penguins, body_mass_g > 4500)

# Keep ONLY Gentoo penguins that were sampled on Biscoe Island
biscoe_gentoo <- filter(
  penguins,
  species == "Gentoo" & island == "Biscoe"
)

# Keep penguins from EITHER Dream OR Torgersen Island
sub_islands <- filter(
  penguins,
  island %in% c("Dream", "Torgersen")
)

# 1.7.3 arrange() = sort rows

# Sort penguins by ascending body mass (Default setting: Smallest mass first)
lightest_to_heaviest <-arrange(penguins, body_mass_g)

# Sort penguins by descending body mass using desc (Largest mass first) 
heaviest_to_lightest <- arrange(penguins, desc(body_mass_g))

# Execute nested sorting criteria: Group by species, then sort by descending bill length
# Sort by species first, then within each species sort bill length largest → smallest
stratified_morphology <- arrange(penguins, species, desc(bill_length_mm))

# 1.7.4 The pipe |>
# |> basically means "THEN" - it passes the result of one step into the next

penguins_final <- penguins |>
  
  # FIRST: create a new column called bill_ratio
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  
  # THEN: keep only Adelie penguins
  filter(species == "Adelie")

# Look at the finished dataset
View(penguins_final)

# 1.7.5 mutate() = CREATE or CHANGE columns

# Start with penguins, THEN create two new columns
penguin_ratios <- penguins |>
  mutate(
    body_mass_kg = body_mass_g / 1000,              # Convert grams → kilograms
    bill_ratio = bill_length_mm / bill_depth_mm     # Calculate bill length/depth ratio
  )

# Check that the new columns were created
glimpse(penguin_ratios)


# 1.8 Data aggregation and ecological summarisation

# Group penguins by species
grouped_penguins <- group_by(penguins, species)

# View the grouped data
print(grouped_penguins)

# Calculate the average body mass for each species
species_mass_summary <- summarise(
  grouped_penguins,
  mean_mass_g = mean(body_mass_g)
)

print(species_mass_summary)

# Fix missing values and summarise by species AND sex
# Overcoming the missing value trap using na.rm = TRUE

biological_signal <- penguins %>%                   # Take the penguins dataset
  group_by(species, sex) %>%                        # AND THEN (%>%) group by species AND sex
  summarise(                                        # AND THEN (%>%) summarize info for each group, such as...
    sample_size = n(),                              # number of rows/observations in each group,
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),  # average body mass mass, ignore NAs,
    sd_mass_g = sd(body_mass_g, na.rm = TRUE)       # and standard deviation (variation) in mass, ignore NAs
  )

# View the final summary
print(biological_signal)

















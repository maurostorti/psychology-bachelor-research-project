# ==============================================================================
# MEDIA DATA PREPARATION
# ==============================================================================

# Load dependencies
source("00_config.R")
source("01_helpers.R")

library(readxl)
library(dplyr)
library(janitor)
library(tidyr)

# Load Dataset -----------------------------------------------------------------
eurotopics_raw <- read_excel(PATH_EUROTOPICS)

# Clean and Filter -------------------------------------------------------------
cleaned_eurotopics <- eurotopics_raw %>%
  # Remove unnecessary columns and clean headers
  select(-any_of("Circulation")) %>%
  clean_names() %>%
  
  # Filter by country and media type defined in config
  filter(country %in% EURO_COUNTRIES) %>%
  filter(type %in% TYPE_FILTER)

# Calculate Orientation Scores -------------------------------------------------
euro_with_scores <- cleaned_eurotopics %>%
  mutate(
    orientation_score = assign_orientation_score(
      media_outlet,
      orientation, 
      country, 
      MEDIA_BASE_SCORES
    )
  ) %>%
  # Remove outlets that couldn't be scored
  drop_na(orientation_score)

# Final validation -------------------------------------------------------------
cat("Media preparation complete.\n")
cat("Total unique outlets processed:", nrow(euro_with_scores), "\n")

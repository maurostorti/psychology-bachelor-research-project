# ==============================================================================
# SURVEY DATA PREPARATION
# ==============================================================================

# Load dependencies
source("00_config.R")
source("01_helpers.R")

library(readxl)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)

# Load and Initial Clean -------------------------------------------------------
response_data <- read_excel(PATH_RESPONSES)

cleaned_data <- response_data %>%
  mutate(participant_id = as.character(row_number() -1)) %>%
  clean_names() %>%
  slice(-1) %>%
  # Remove unwanted columns
  select(-recorded_date, -gender_identity_4_text, -(37:118), -161, 85) %>%
  # Rename specific columns from COLUMN_MAP
  rename(any_of(COLUMN_MAP)) %>%
  # Rename the scale items
  rename_with(~ str_replace(., "life_satisfaction_", "swls_"), starts_with("life_satisfaction_")) %>%
  rename_with(~ str_replace(., "pq_8_", "pq_"), starts_with("pq_8_")) %>%
  rename_with(~ str_replace(., "pa_scale_", "pa_"), starts_with("pa_scale_")) %>%
  # Fix data types
  mutate(across(any_of(c("duration_s", "progress", "age")), as.numeric))

# Recode Values and Scales --------------------------------------------------
processed_data <- cleaned_data %>%
  mutate(
    consent_check = apply_scale(consent_check, SCALES$boolean),
    residence_3yr = apply_scale(residence_3yr, SCALES$boolean),
    edu_level     = apply_scale(edu_level,     SCALES$edu),
    pol_ideology  = apply_scale(pol_ideology,  SCALES$pol),
    
    across(starts_with("swls_"), ~ apply_scale(.x, SCALES$swls)),
    across(starts_with("pq_"),   ~ apply_scale(.x, SCALES$pq8)),
    across(starts_with("pa_"),   ~ apply_scale(.x, SCALES$pa)),
    
    across(
      any_of(c("freq_tv", "freq_radio", "freq_newspaper", "freq_posts", 
               "freq_podcast", "freq_videos", "social_passive", 
               "social_active", "news_avoid", "news_confirm")),
      ~ clean_media_strings(.x) %>% apply_scale(SCALES$freq)
    )
  )

# Filtering --------------------------------------------------------------------

# Filter 1: Informed Consent
accepted_data <- processed_data %>% 
  filter(consent_check == TRUE)

# Filter 2: Demographics & Age (18+)
adults_data <- accepted_data %>%
  drop_na(age, gender, edu_level, birthplace, residence_3yr) %>%
  mutate(age = if_else(age > 1900, 2026 - age, age)) %>%
  filter(age >= 18)

# Filter 3: European Residency/Birthplace
europeans_data <- adults_data %>%
  filter(
    birthplace %in% EURO_COUNTRIES | 
      (country_of_residence %in% EURO_COUNTRIES & residence_3yr == TRUE)
  )

# Filter 4: Complete Dependent Variables
dependent_data <- europeans_data %>%
  drop_na(pol_ideology,
          any_of(SWLS_VARS), 
          any_of(PA_VARS), 
          any_of(PQ_VARS))

# Filter 5: Complete Independent Variables
independent_data <- dependent_data %>%
  drop_na(starts_with("freq_"), 
          starts_with("social_"), 
          news_avoid, news_confirm, news_country)

# Filter 6: Attention Check
survey_final <- independent_data %>%
  filter(attention_check == "green") %>%
  select(-attention_check)

# Reporting --------------------------------------------------------------------
cat("--------- Survey Funnel Summary ---------\n")
cat("Initial Responses:                   ", nrow(cleaned_data), "\n")
cat("Consented:                           ", nrow(accepted_data), "\n")
cat("Adults (18+):                        ", nrow(adults_data), "\n")
cat("Europeans:                           ", nrow(independent_data), "\n")
cat("Completed Media Diet Questionnaire:  ", nrow(europeans_data), "\n")
cat("Passed Attention Check N:            ", nrow(survey_final), "\n")
cat("-----------------------------------------\n")

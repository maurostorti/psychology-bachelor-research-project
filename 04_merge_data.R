# ==============================================================================
# DATA MERGING AND FEATURE ENGINEERING
# ==============================================================================

source("00_config.R")
source("01_helpers.R")
source("02_media_prep.R")   
source("03_survey_prep.R")   

library(BayesFactor)
library(bayestestR)
library(correlation)
library(dplyr)
library(psych)
library(stringr)
library(tidyr)

# Join Survey Data with Media Scores -------------------------------------------
survey_with_media <- survey_final %>%
  pivot_longer(
    cols = any_of(SURVEY_COUNTRY_COLS),
    names_to = "survey_country_col",
    values_to = "selected_outlets"
  ) %>%
  filter(!is.na(selected_outlets)) %>%
  separate_rows(selected_outlets, sep = ",") %>%
  mutate(
    selected_outlets = str_trim(selected_outlets),
    country_clean = str_to_title(str_replace_all(survey_country_col, "_", " "))
  ) %>%
  left_join(
    euro_with_scores, 
    by = c("selected_outlets" = "media_outlet", "country_clean" = "country")
  )

# Aggregate Media Scores per Participant ---------------------------------------
participant_media_stats <- survey_with_media %>%
  group_by(participant_id) %>% 
  summarize(
    outlet_mean     = coalesce(mean(orientation_score, na.rm = TRUE), 4),
    outlet_sd       = coalesce(sd(orientation_score, na.rm = TRUE), 0),
    n_outlets       = n(),
    .groups = "drop"
  )

# Psychometric Analysis (Reliability) ------------------------------------------
swls_alpha <- psych::alpha(survey_final %>% select(any_of(SWLS_VARS)))
pa_alpha   <- psych::alpha(survey_final %>% select(any_of(PA_VARS)))

# Final Data Assembly ----------------------------------------------------------
final_analysis_df <- survey_final %>%
  left_join(participant_media_stats, by = "participant_id") %>%
  mutate(
    swls_mean = rowMeans(across(all_of(SWLS_VARS)), na.rm = TRUE),
    pa_mean   = rowMeans(across(all_of(PA_VARS)), na.rm = TRUE),
    pq_mean   = rowMeans(across(all_of(PQ_VARS)), na.rm = TRUE)
  ) %>%
  calculate_swb_composite() %>%
  mutate(
    pol_ideology = as.numeric(pol_ideology),
    age          = as.numeric(age),
    gender       = as.factor(gender),
    edu_level    = as.factor(edu_level)
  ) %>% 
  select(
    -any_of(SWLS_VARS), 
    -any_of(PA_VARS),
    -any_of(PQ_VARS),
    -any_of(SURVEY_COUNTRY_COLS),
    -news_country
  )


# Summary for APA Reporting ----------------------------------------------------

# Reliability Coefficients
cat("Cronbach's Alpha for SWLS:", round(swls_alpha$total$raw_alpha, 2), "\n")
cat("Cronbach's Alpha for PA:", round(pa_alpha$total$raw_alpha, 2), "\n")

# Means and SD
apa_descriptives <- final_analysis_df %>%
  select(swls_mean, pa_mean, swb) %>%
  describe() %>%
  as.data.frame() %>%
  select(mean, sd, min, max)

print(round(apa_descriptives, 2))

# Correlation Analysis
stats_results <- correlation::correlation(
  final_analysis_df %>% select(swls_mean, pa_mean),
  bayesian = TRUE,
  include_factors = FALSE
)

summary_report <- stats_results %>%
  mutate(
    p = pd_to_p(pd),
    df = n_Obs - 2,
    R2 = rho^2,
    BF10 = BF 
  ) %>%
  select(Parameter1, Parameter2, rho, df, p, R2, BF10)

print(summary_report)

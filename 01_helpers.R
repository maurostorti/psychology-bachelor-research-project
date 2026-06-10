# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

library(dplyr)
library(stringr)

#' Recode values using a named vector/list
#' @param x The vector to recode
#' @param scale_mapping A named vector from SCALES in 00_config.R
apply_scale <- function(x, scale_mapping) {
  # Clean the input to ensure string matching works
  x_clean <- str_trim(as.character(x))
  
  # Return the mapped value
  unname(scale_mapping[x_clean])
}


#' Clean and Standardize Frequency Strings
#' Handles the inconsistencies between Qualtrics dashes and text
#' @param x Vector of frequency strings
clean_media_strings <- function(x) {
  x %>%
    str_replace_all("–", "-") %>%
    str_replace_all("times", "days") %>%
    str_trim()
}


#' Assign Orientation Scores
#' @param orientation The orientation string from Eurotopics
#' @param country The country string
assign_orientation_score <- function(media_outlet, orientation, country, base_scores) {
  case_when(
    
    # Override for Pro-government
    orientation == "Pro-government"  ~ 6,
    
    # Override for Critical of the government
    orientation == "Critical of the government" ~ 2,
    
    # General mapping from config
    TRUE ~ base_scores[orientation]
  )
}


#' Calculate Composite Z-Score for Well-Being
#' @param df Dataframe containing swls_mean and pa_mean
calculate_swb_composite <- function(df) {
  df %>%
    mutate(
      swls_z = as.numeric(scale(swls_mean)),
      pa_z   = as.numeric(scale(pa_mean)),
      pq_z   = as.numeric(scale(pq_mean)),
      swb    = (swls_z + pa_z) / 2
    )
}
# ==============================================================================
# CONFIGURATION AND CONSTANTS
# ==============================================================================

# FILE PATHS
PATH_EUROTOPICS <- # Insert path to eurotopics data here!!!
PATH_RESPONSES  <- # Insert path to dataset here!!!


# GEOGRAPHIC FILTERS -----------------------------------------------------------
EURO_COUNTRIES <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine",
  "United Kingdom", "United Kingdom of Great Britain and Northern Ireland"
)


# MEDIA TYPE FILTERS -----------------------------------------------------------
TYPE_FILTER <- c(
  "Daily newspaper", "Website", "Magazine", "Weekly newspaper", "Monthly magazine"
)


# SCALE MAPPINGS ---------------------------------------------------------------

SCALES <- list(
  
  # Consent and Residence
  boolean = c(
    "Yes, I give consent for my research data to be collected, stored, processed, and published as described in the information letter." = TRUE,
    "No, I do not give consent for my research data to be collected." = FALSE,
    "Yes" = TRUE,
    "No" = FALSE
  ),
  
  # Education Level
  edu = c(
    "Prefer not to say" = 0,
    "Less than Primary" = 1,
    "Primary" = 2,
    "Some Secondary" = 3,
    "Secondary" = 4,
    "Vocational or Similar" = 5,
    "Some University but no degree" = 6,
    "University - Bachelors Degree" = 7,
    "Graduate or professional degree (MA, MS, MBA, PhD, Law Degree, Medical Degree etc)" = 8
  ),
  
  # Political Ideology (1 = Liberal, 7 = Conservative)
  pol = c(
    "Strongly liberal" = 1,
    "Liberal" = 2,
    "Somewhat liberal" = 3,
    "Neither liberal nor conservative" = 4,
    "Somewhat conservative" = 5,
    "Conservative" = 6,
    "Strongly Conservative" = 7
  ),
  
  # Satisfaction with Life (SWLS)
  swls = c(
    "Strongly disagree" = 1,
    "Disagree" = 2,
    "Somewhat disagree" = 3,
    "Neither disagree or agree" = 4,
    "Neither agree or disagree" = 4,
    "Somewhat agree" = 5,
    "Agree" = 6,
    "Strongly agree" = 7
  ),
  
  # PQ-8 Scale
  pq8 = c(
    "Not at all" = 1,
    "Several days" = 2,
    "More than half the days" = 3,
    "Nearly every day" = 4
  ),
  
  # Positive Affect (PA)
  pa = c(
    "Very slightly or not at all" = 1,
    "A little" = 2,
    "Moderately" = 3,
    "Quite a bit" = 4,
    "Extremely" = 5
  ),
  
  # Frequency of Media Use
  freq = c(
    "Never" = 1,
    "More rarely" = 2,
    "1-2 days a week" = 3,
    "3-4 days a week" = 4,
    "5-6 days a week" = 5,
    "Daily" = 6 
  )
)


# MEDIA ORIENTATION MAPPING ----------------------------------------------------
MEDIA_BASE_SCORES <- c(
  "Far left"              = 1,
  "Left-wing"             = 2,
  "Centre-left"           = 3,
  "Liberal"               = 4,
  "Liberal-conservative"  = 5,
  "Conservative"          = 6,
  "Christian"             = 6,
  "Catholic"              = 6,
  "Right-wing"            = 7
)


# SURVEY COLUMN RE-NAMING ------------------------------------------------------

COLUMN_MAP <- c(
  # Basic info
  duration_s     = "duration_in_seconds",
  consent_check  = "q92",
  
  # Demographics
  gender         = "gender_identity",
  edu_level      = "education",
  residence_3yr  = "validation_of_co_r",
  
  # Dependent variable
  pol_ideology   = "q28_1",
  
  # Media diet
  freq_tv        = "frequency_1",
  freq_radio     = "frequency_2",
  freq_newspaper = "frequency_3",
  freq_posts     = "frequency_4",
  freq_podcast   = "frequency_5",
  freq_videos    = "frequency_6",
  
  social_passive = "social_media_1",
  social_active  = "social_media_2",
  
  news_avoid     = "q48_1",    
  news_confirm   = "q48_2",    
  news_country   = "country_media_select"


)

# COUNTRY LIST FOR PIVOTING ----------------------------------------------------
SURVEY_COUNTRY_COLS <- c(
  "austria", "belgium", "bulgaria", "croatia", "cyprus", "czech_republic", 
  "denmark", "estonia", "finland", "france", "germany", "greece", "hungary", 
  "ireland", "italy", "latvia", "lithuania", "luxembourg", "malta", 
  "netherlands", "poland", "portugal", "romania", "slovakia", "slovenia", 
  "spain", "sweden", "switzerland", "turkey", "ukraine", "united_kingdom"
)

# VARIABLE GROUPS --------------------------------------------------------------
FREQ_VARS <- c(
  "freq_tv", "freq_radio", "freq_newspaper", 
  "freq_posts", "freq_podcast", "freq_videos"
)

SWLS_VARS <- c(
  "swls_1", "swls_2", "swls_3","swls_4", "swls_5"
)

PA_VARS <- c(
  "pa_1", "pa_2", "pa_3","pa_4", "pa_5",
  "pa_6", "pa_7", "pa_8","pa_9", "pa_10"
)

PQ_VARS <- c(
  "pq_1", "pq_2", "pq_3","pq_4", "pq_5",
  "pq_6", "pq_7", "pq_8"
)

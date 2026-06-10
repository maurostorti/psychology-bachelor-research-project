# ==============================================================================
# LATENT PROFILE ANALYSIS (LPA)
# ==============================================================================

# Dependencies -----------------------------------------------------------------
library(BayesFactor) 
library(bridgesampling)
library(brms)
library(broom)
library(dabestr)
library(dplyr)
library(fs)
library(ggplot2)
library(janitor)
library(nnet) 
library(tidyLPA)
library(tidyr)
     
if(!exists("final_analysis_df")) source("04_merge_data.R")

# Create Output Directory
output_dir <- "LPA_Plots_APA"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Variables for LPA ------------------------------------------------------------

lpa_indicator_names <- c( 
  "social_active", "social_passive",
  "news_avoid", "news_confirm",
  "outlet_mean","outlet_sd",
  FREQ_VARS
  )

# Generate summary table
summary_table <- final_analysis_df %>%
  select(all_of(lpa_indicator_names)) %>%
  # Pivot to long format to calculate stats for all variables at once
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  # Group by variable name and calculate Mean and SD
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# View the result
print(summary_table)


# Prepare Data -----------------------------------------------------------------

lpa_data_prepared <- final_analysis_df %>%
  select(all_of(lpa_indicator_names)) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.vector(scale(.x))))

# Estimate LPA Models ----------------------------------------------------------

model_comparison <- lpa_data_prepared %>%
  estimate_profiles(
    1:6,
    variances = "equal",
    covariances = "zero"
  )

fit_indices <- get_fit(model_comparison)

# Generate Scree Plot for SABIC ------------------------------------------------

# Extract numbers of classes and SABIC values from tidyLPA fit indices data frame
scree_data <- fit_indices %>% 
  select(Classes, SABIC)

p_scree <- ggplot(scree_data, aes(x = Classes, y = SABIC)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 2.5) +
  scale_x_continuous(breaks = scree_data$Classes) +
  theme_classic(base_size = 12, base_family = "sans") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "plain"),
    panel.grid.major.y = element_line(color = "gray95")
  ) +
  labs(
    x = "Number of Profiles (Classes)",
    y = "SABIC Value"
  )


# Select Best Model ------------------------------------------------------------

print(fit_indices)

best_model_index <- 4
final_model <- model_comparison[[best_model_index]]

# Extract class assignments
final_assignments <- get_data(final_model) %>%
  clean_names()

# Merge class assignments back to original data
analysis_data <- final_analysis_df %>%
  drop_na(all_of(lpa_indicator_names)) %>%
  bind_cols(final_assignments %>%
              select(class, starts_with("cprob")
                   )) %>%
  mutate(class = factor(class)) 

# Summary of Class Means and Average Posterior Probabilities
lpa_summary <- analysis_data %>%
  group_by(class) %>%
  summarise(
    n = n(),
    across(starts_with("cprob"), \(x) mean(x, na.rm = TRUE)), 
    across(all_of(lpa_indicator_names), \(x) mean(x, na.rm = TRUE))
  )

print(lpa_summary)

# Estimation Plotting Function -------------------------------------------------
plot_estimation_lpa <- function(data, var_name) {
  
  # Format the variable name
  pretty_label <- stringr::str_to_title(gsub("_", " ", var_name))
  
  # Create dabestr object
  res <- dabestr::load(
    data = data,
    x = class,
    y = !!sym(var_name),
    idx = levels(data$class)
  )
  
  # Compute mean differences
  diffs <- dabestr::mean_diff(res)
  
  # Create plot
  p <- dabestr::dabest_plot(
    diffs,
    swarm_label           = pretty_label,                 
    effsize_ylabel        = "Difference from Class 1\n(95% CI)",
    rawplot_color_palette = "Set2",
    raw_marker_alpha      = 0.4,
    raw_marker_size       = 1.0,
    raw_marker_spread     = 0.3,
    asymmetric_side       = "right",
    swarm_bars            = TRUE,
    show_legend           = FALSE
  )
  

  p <- p +
    theme_classic(base_size = 12, base_family = "sans") +
    theme(
      axis.title.y     = element_text(face = "bold"),
      legend.position  = "none",
      panel.spacing    = unit(1.5, "lines"),
      strip.background = element_blank()
    )
  
  return(p)
}

# Generate and Save Plots ------------------------------------------------------

message("Generating and saving APA formatted plots...")

ggsave(
  filename = file.path(output_dir, "LPA_Scree_SABIC.png"),
  plot     = p_scree,
  device   = "png",
  width    = 16.5,    
  height   = 9.5,     
  units    = "cm",    
  dpi      = 300
)

lapply(lpa_indicator_names, function(v) {
  
  p_temp <- plot_estimation_lpa(analysis_data, v)
  
  # Sanitize filename
  file_name <- paste0("LPA_", v, ".png")
  file_path <- file.path(output_dir, file_name)
  
  ggsave(
    filename = file_path,
    plot     = p_temp,
    device   = "png",
    width    = 16.5,
    height   = 9.5,    
    units    = "cm",   
    dpi      = 300
  )
  
  return(NULL)
})

message(paste("Success! All plots saved in folder:", output_dir))


# MULTINOMIAL LOGISTIC REGRESSION ----------------------------------------------
message("Running Multinomial Logistic Regression...")

# Prepare and CLEAN demographic data
demo_data <- analysis_data %>%
  mutate(
    age = as.numeric(age),
    education = as.numeric(edu_level), 
    gender = factor(gender),
    class = factor(class)
  ) %>%
  filter(gender != "Non binary") %>%
  filter(education != 0) %>%
  droplevels() 

# Set Reference Group
demo_data$class <- relevel(demo_data$class, ref = "1")

# Fit the Model
multinom_model <- multinom(class ~ age + education + gender, data = demo_data, trace = FALSE)

# Extract Clean Results
mlr_results <- tidy(multinom_model, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(across(where(is.numeric), round, 3))

print(mlr_results)


# BAYESIAN ANALYSIS ------------------------------------------------------------

message("Calculating Bayes Factors via Bridge Sampling...")

# Fit the Full Model
bf_fit_full <- brm(
  formula = class ~ age + education + gender,
  data = demo_data,
  family = categorical(link = "logit"),
  save_pars = save_pars(all = TRUE), 
  iter = 4000,          
  cores = 4,
  silent = 2,
  backend = "rstan" # Ensures compatibility with bridge sampling
)

# Fit the Null Model (Intercept only)
bf_fit_null <- brm(
  formula = class ~ 1,
  data = demo_data,
  family = categorical(link = "logit"),
  save_pars = save_pars(all = TRUE),
  iter = 4000,
  cores = 4,
  silent = 2,
  backend = "rstan"
)

# Calculate the Bayes Factor (Evidence for Predictors vs. Null)
bf_comparison <- bayes_factor(bf_fit_full, bf_fit_null)

print("--- Bayes Factor: Predictors vs. Null ---")
print(bf_comparison)

# View Bayesian Odds Ratios
summary(bf_fit_full)


# ESTIMATION PLOTTING ----------------------------------------------------------

plot_estimation_age <- function(data, var_name) {
  
  # Labeling
  pretty_label <- stringr::str_to_title(gsub("_", " ", var_name))
  
  # Data Prep
  res <- dabestr::load(data = data, x = class, y = !!sym(var_name), 
                       idx = levels(as.factor(data$class)))
  diffs <- dabestr::mean_diff(res)
  
  p <- dabestr::dabest_plot(
    diffs,
    swarm_label           = pretty_label,                      
    effsize_ylabel        = "Difference from Class 1\n(95% CI)",
    rawplot_color_palette = "Set2",     
    raw_marker_alpha      = 0.4,        
    raw_marker_size       = 1.0,        
    raw_marker_spread     = 0.3,        
    asymmetric_side       = "right",    
    swarm_bars            = TRUE,
    show_legend           = FALSE
  ) +
    theme_classic(base_size = 12) +
    theme(
      axis.title.y    = element_text(face = "bold"),
      panel.spacing   = unit(1.5, "lines"),
      strip.background = element_blank()
    ) 
  
  return(p)
}

# Generate and Save Age Plot
p_age <- plot_estimation_age(demo_data, "age")
ggsave(
  filename = file.path(output_dir, "Age_Comparison_Plot.png"), 
  plot     = p_age, 
  device   = "png",   
  width    = 16.5,     
  height   = 9.5,      
  units    = "cm",     
  dpi      = 300
)

message("Analysis complete. Multinomial model and plots generated.")

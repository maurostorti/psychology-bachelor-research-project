# ==============================================================================
# Analysis
# ==============================================================================

if(!exists("analysis_data")) source("05_latent_profile_analysis.R")

library(BayesFactor)
library(car)
library(dplyr)
library(emmeans)
library(parameters)
library(psych)
library(sjPlot)
library(ggplot2)

# Create Output Directory First
analysis_output_dir <- "Analysis_Plots_APA"
if (!dir.exists(analysis_output_dir)) dir.create(analysis_output_dir)

# Plots theme
theme_style <- function() {
  theme_classic(base_size = 12, base_family = "sans") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey30"),
      axis.title = element_text(size = 13),      
      axis.text = element_text(color = "black", size = 12),     # Forces axis tick labels to 12
      legend.title = element_text(size = 12),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

plot_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")

# Distribution Plot ------------------------------------------------------------
p1 <- ggplot(analysis_data, aes(x = swb)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 15, fill = "white", color = "black") +
  geom_density(size = 0.8, color = "#66C2A5") + # Set2 Green for consistency
  labs(x = "Subjective Well-Being", y = "Density") +
  theme_style()


# Descriptive Statistics for Political Ideology --------------------------------
cat("\n--- Descriptive Statistics: Political Ideology ---\n")
ideology_stats <- analysis_data %>%
  summarise(
    Mean = mean(pol_ideology, na.rm = TRUE),
    SD = sd(pol_ideology, na.rm = TRUE),
    N = n()
  )
print(ideology_stats)

# Ideology Distribution Plot ---------------------------------------------------
p2 <- ggplot(analysis_data, aes(x = pol_ideology)) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), 
           fill = "white", color = "black", width = 0.7) +
  geom_density(aes(y = after_stat(density) * 1), size = 0.8, color = "#FC8D62") +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "Political Ideology (1 = Strongly Liberal, 7 = Strongly Conservative)", 
       y = "Proportion / Density") +
  theme_style()


# Prediction Model -------------------------------------------------------------
model <- lm(swb ~ pol_ideology, data = analysis_data)

# Assumption Checks for Linear Regression --------------------------------------
cat("\n--- Regression Assumption Checks ---\n")

# Normality of Residuals (Q-Q Plot)
# ------------------------------------------------------------------
png(filename = file.path(analysis_output_dir, "Diagnostics_BaseModel_QQ.png"), 
    width = 16.5, height = 9.5, units = "cm", res = 300)
par(mar = c(4.5, 4.5, 1.5, 1.5) + 0.1) 
plot(model, which = 2, col = "#FC8D62", pch = 16, 
     main = "", sub = "", ann = FALSE) 
title(xlab = "Theoretical Quantiles", ylab = "Standardized residuals") 
dev.off()

# Linearity and Homoscedasticity (Residual vs. Predicted Plot)
# ------------------------------------------------------------------
png(filename = file.path(analysis_output_dir, "Diagnostics_BaseModel_Residuals.png"), 
    width = 16.5, height = 9.5, units = "cm", res = 300)
par(mar = c(4.5, 4.5, 1.5, 1.5) + 0.1)
plot(model, which = 1, col = "#66C2A5", pch = 16, 
     main = "", sub = "", ann = FALSE)
title(xlab = "Fitted values", ylab = "Residuals")
dev.off()

# Formal test for Homoscedasticity: Non-constant Variance Score Test
print(ncvTest(model))

# Outlier Diagnostics ----------------------------------------------
k <- 1 # Number of predictors (pol_ideology)
n <- nrow(analysis_data) # Sample size

analysis_data <- analysis_data %>%
  mutate(
    stdres = rstandard(model),   # Standardized residuals
    leverage = hatvalues(model), # Leverage values
    cd = cooks.distance(model)   # Cook's distance
  )

cat("\n--- Outlier Summary ---\n")
print(summary(analysis_data$stdres)) 

cat("Leverage threshold:", 3 * (k + 1) / n, "\n")
print(summary(analysis_data$leverage))

print(summary(analysis_data$cd))


# Linear Regression Results ----------------------------------------------------
summary(model)
tab_model(model, show.std = TRUE, digits = 3)
model_parameters(model, standardize = "refit", digits = 4)

# Bayes Factor for simple regression
bf_result <- regressionBF(swb ~ pol_ideology, data = analysis_data)
print(bf_result)

# Regression Plot
p3 <- ggplot(analysis_data, aes(x = pol_ideology, y = swb)) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.0) + 
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  scale_x_continuous(breaks = 1:7) + 
  labs(x = "Political Ideology (1 = Strongly Liberal, 7 = Strongly Conservative)",
       y = "Subjective Well-Being (Raw)") +
  theme_style()


# Weighted Interaction Analysis (BCH Logic) ------------------------------------

# Prepare Weights
analysis_data <- analysis_data %>%
  rowwise() %>%
  mutate(assignment_prob = get(paste0("cprob", class))) %>%
  ungroup()

# Set Contrasts for Type III SS
options(contrasts = c("contr.sum", "contr.poly"))

# The Weighted Model
model_weighted <- lm(swb ~ pol_ideology * class, 
                     data = analysis_data, 
                     weights = assignment_prob)

# Assumption Checks for Linear Regression --------------------------------------
cat("\n--- Regression Assumption Checks: Weighted Interaction Model ---\n")


# Normality of Residuals (Q-Q Plot)
# ------------------------------------------------------------------
png(filename = file.path(analysis_output_dir, "Diagnostics_WeightedModel_QQ.png"), 
    width = 16.5, height = 9.5, units = "cm", res = 300)
par(mar = c(4.5, 4.5, 1.8, 2) + 0.1) # Changed top from 1 to 1.8
plot(model_weighted, which = 2, col = "#FC8D62", pch = 16, 
     main = "", sub = "", ann = FALSE) 
title(xlab = "Theoretical Quantiles", ylab = "Standardized residuals") 
dev.off()


# Linearity and Homoscedasticity (Residual vs. Predicted Plot)
# ------------------------------------------------------------------
png(filename = file.path(analysis_output_dir, "Diagnostics_WeightedModel_Residuals.png"), 
    width = 16.5, height = 9.5, units = "cm", res = 300)
par(mar = c(4.5, 4.5, 1.8, 2) + 0.1) # Changed top from 1 to 1.8
plot(model_weighted, which = 1, col = "#66C2A5", pch = 16, 
     main = "", sub = "", ann = FALSE) 
title(xlab = "Fitted values", ylab = "Residuals")
dev.off()

# Formal Test
print(ncvTest(model_weighted)) 

# Outlier Diagnostics (Adjusted for Interaction)
k_weighted <- length(coef(model_weighted)) - 1 
n <- nrow(analysis_data)

analysis_data <- analysis_data %>%
  mutate(
    stdres_w = rstandard(model_weighted),   
    leverage_w = hatvalues(model_weighted), 
    cd_w = cooks.distance(model_weighted)   
  )

cat("\n--- Outlier Summary (Weighted Model) ---\n")
print(summary(analysis_data$stdres_w)) 

lev_threshold <- 3 * (k_weighted + 1) / n
cat("Leverage threshold (k =", k_weighted, "):", lev_threshold, "\n")
print(summary(analysis_data$leverage_w))

print(summary(analysis_data$cd_w))

cat("\n--- Weighted Interaction Analysis (Type III Anova) ---\n")
print(Anova(model_weighted, type = 3))

# Post-Hoc: Simple Slopes ------------------------------------------------------
cat("\n--- Weighted Simple Slopes ---\n")
slopes <- emtrends(model_weighted, ~ class, var = "pol_ideology")
print(slopes)
print(pairs(slopes))

# Final Visualization ----------------------------------------------------------
p4 <- ggplot(analysis_data, aes(x = pol_ideology, y = swb, color = class, fill = class, weight = assignment_prob)) +
  geom_jitter(aes(alpha = assignment_prob), width = 0.15, height = 0.1, size = 1.2) + 
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_color_manual(values = plot_colors) +
  scale_fill_manual(values = plot_colors) +
  scale_alpha_continuous(range = c(0.2, 0.7), guide = "none") + 
  scale_x_continuous(breaks = 1:7) +
  labs(
    x = "Political Ideology",
    y = "Subjective Well-Being (Raw)",
    color = "Media Class",
    fill = "Media Class"
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),
    fill = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_style()

# Bayesian Model Comparison ----------------------------------------------------
bf_base <- lmBF(swb ~ pol_ideology, data = analysis_data)
bf_additive <- lmBF(swb ~ pol_ideology + class, data = analysis_data)
bf_interaction <- lmBF(swb ~ pol_ideology * class, data = analysis_data)

cat("\n--- Bayes Factor Comparisons ---\n")
print(bf_additive / bf_base)        
print(bf_interaction / bf_base)     

# Reset contrasts --------------------------------------------------------------
options(contrasts = c("contr.treatment", "contr.poly"))


# Save Analysis Plots ----------------------------------------------------------

message("Saving final analysis plots...")

plot_list <- list(
  "Distribution_SWB" = p1, 
  "Distribution_Ideology" = p2,
  "Ideology_Regression" = p3, 
  "Weighted_Interaction" = p4  
)

lapply(names(plot_list), function(name) {
  file_path <- file.path(analysis_output_dir, paste0(name, ".png"))
  ggsave(
    filename = file_path,
    plot = plot_list[[name]],
    device = "png",
    width = 16.5,
    height = 9.5,
    units = "cm",
    dpi = 300
  )
})

message(paste("Success! All analysis plots (including diagnostic plots) saved in:", analysis_output_dir))
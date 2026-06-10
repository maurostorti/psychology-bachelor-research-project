library(pwr)

# H1: Power analysis for a simple linear regression (1 predictor)
pwr.r.test(
  r = 0.12,
  sig.level = 0.05, 
  power = 0.80, 
  alternative = "two.sided"
)

# ----

# H2

# R-squared values
r2_basic <- 0.12 ^ 2          # Variance explained by ideology + class without the interaction
r2_full  <- 0.12 ^ 2 + 0.03   # Total variance explained by the whole interaction model

# Calculate f2 specifically for the interaction effect
f2_interaction <- (r2_full - r2_basic) / (1 - r2_full)
f2_interaction

# 3. Power analysis
pwr.f2.test(
  u = 3,               # Degrees of freedom for the interaction: (4 classes - 1) * 1 predictor
  v = NULL, 
  f2 = f2_interaction,
  sig.level = 0.05, 
  power = 0.80
)

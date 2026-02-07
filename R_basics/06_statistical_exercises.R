# ============================================
# Statistical Analysis Practice Exercises
# Aquaculture, Marine Ecology & Fisheries
# ============================================

library(ggplot2)

# ============================================
# DATASETS FOR EXERCISES
# ============================================

# Dataset 1: Shrimp feeding trial
shrimp_trial <- data.frame(
  treatment = factor(rep(c("Control", "Probiotic", "Prebiotic", "Synbiotic"), each = 8)),
  tank_id = paste0("T", 1:32),
  final_weight = c(
    # Control
    18.2, 17.5, 19.1, 16.8, 18.5, 17.2, 18.8, 17.9,
    # Probiotic
    21.3, 22.1, 20.5, 21.8, 22.5, 20.9, 21.2, 22.0,
    # Prebiotic
    20.1, 19.5, 20.8, 19.2, 20.3, 19.8, 20.5, 19.6,
    # Synbiotic
    23.5, 24.2, 22.8, 23.9, 24.5, 23.1, 24.0, 23.6
  ),
  survival_pct = c(
    # Control
    82, 78, 85, 80, 83, 79, 84, 81,
    # Probiotic
    92, 95, 90, 93, 94, 91, 93, 92,
    # Prebiotic
    88, 85, 90, 86, 89, 87, 88, 86,
    # Synbiotic
    95, 97, 93, 96, 98, 94, 96, 95
  )
)

# Dataset 2: Fish catch data from different fishing grounds
fishing_data <- data.frame(
  fishing_ground = factor(rep(c("North", "Central", "South"), each = 12)),
  season = factor(rep(rep(c("Dry", "Wet"), each = 6), 3)),
  catch_kg = c(
    # North
    245, 280, 260, 255, 270, 250,  # Dry
    180, 165, 195, 175, 188, 170,  # Wet
    # Central
    320, 345, 310, 335, 328, 340,  # Dry
    285, 270, 295, 280, 290, 275,  # Wet
    # South
    195, 210, 185, 200, 205, 190,  # Dry
    220, 240, 225, 235, 230, 218   # Wet
  )
)

# Dataset 3: Coral reef survey
reef_survey <- data.frame(
  site = c("Reef_A", "Reef_B", "Reef_C", "Reef_D", "Reef_E",
           "Reef_F", "Reef_G", "Reef_H", "Reef_I", "Reef_J"),
  coral_cover = c(45, 62, 38, 55, 48, 70, 42, 58, 35, 52),
  fish_abundance = c(120, 185, 95, 165, 135, 210, 105, 175, 80, 155),
  water_clarity = c(8.5, 12.0, 6.5, 10.5, 9.0, 14.0, 7.0, 11.0, 5.5, 9.5),
  distance_to_shore = c(2.5, 5.0, 1.5, 4.0, 3.0, 6.5, 2.0, 4.5, 1.0, 3.5)
)

# Dataset 4: Oyster growth in different salinities
oyster_growth <- data.frame(
  salinity = factor(rep(c("Low_15ppt", "Medium_25ppt", "High_35ppt"), each = 10)),
  shell_length = c(
    # Low salinity
    32.5, 30.8, 33.2, 29.5, 31.8, 32.0, 30.2, 33.5, 31.0, 32.8,
    # Medium salinity
    42.3, 44.5, 41.8, 45.2, 43.0, 44.8, 42.5, 45.0, 43.5, 44.2,
    # High salinity
    38.2, 36.5, 39.0, 35.8, 37.5, 38.8, 36.0, 39.5, 37.2, 38.5
  )
)

# Dataset 5: Species occurrence in different habitats
species_habitat <- matrix(c(
  52, 18, 30,   # Species 1
  25, 45, 30,   # Species 2
  15, 35, 50,   # Species 3
  38, 32, 30    # Species 4
), nrow = 4, byrow = TRUE)
rownames(species_habitat) <- c("Snapper", "Grouper", "Rabbitfish", "Parrotfish")
colnames(species_habitat) <- c("Coral_Reef", "Seagrass", "Mangrove")


# ============================================
# EXERCISES
# ============================================

# -----------------------------------------
# EXERCISE 1: Descriptive Statistics
# -----------------------------------------
# Using the shrimp_trial dataset:
# a) Calculate the mean, SD, and SE for final_weight in each treatment
# b) Calculate the coefficient of variation (CV%) for each treatment
# c) Which treatment shows the most consistent growth (lowest CV)?

# Your code here:



# -----------------------------------------
# EXERCISE 2: Two-Sample T-Test
# -----------------------------------------
# Compare the final_weight between Control and Synbiotic treatments
# a) Perform a t-test
# b) Calculate Cohen's d effect size
# c) Is there a significant difference? Interpret the results

# Your code here:



# -----------------------------------------
# EXERCISE 3: One-Way ANOVA
# -----------------------------------------
# Using the oyster_growth dataset:
# a) Test if salinity affects shell_length
# b) Check assumptions (normality, homogeneity of variance)
# c) Perform Tukey HSD post-hoc test
# d) Create a boxplot with significance letters

# Your code here:



# -----------------------------------------
# EXERCISE 4: Two-Way ANOVA
# -----------------------------------------
# Using the fishing_data dataset:
# a) Test the effects of fishing_ground and season on catch_kg
# b) Is there an interaction effect?
# c) Create an interaction plot
# d) Interpret the results

# Your code here:



# -----------------------------------------
# EXERCISE 5: Correlation Analysis
# -----------------------------------------
# Using the reef_survey dataset:
# a) Calculate correlation between coral_cover and fish_abundance
# b) Test if the correlation is significant
# c) Create a correlation matrix for all numeric variables
# d) Visualize with a scatter plot and trend line

# Your code here:



# -----------------------------------------
# EXERCISE 6: Linear Regression
# -----------------------------------------
# Using the reef_survey dataset:
# a) Build a regression model predicting fish_abundance from coral_cover
# b) What is the R-squared value?
# c) Interpret the slope coefficient
# d) Predict fish abundance when coral cover is 50%

# Your code here:



# -----------------------------------------
# EXERCISE 7: Multiple Regression
# -----------------------------------------
# Using the reef_survey dataset:
# a) Build a model predicting fish_abundance from coral_cover,
#    water_clarity, and distance_to_shore
# b) Which predictors are significant?
# c) Compare R-squared with the simple regression model

# Your code here:



# -----------------------------------------
# EXERCISE 8: Chi-Square Test
# -----------------------------------------
# Using the species_habitat matrix:
# a) Test if species distribution is independent of habitat type
# b) Examine the expected frequencies
# c) Which species-habitat combinations differ most from expected?

# Your code here:



# -----------------------------------------
# EXERCISE 9: Non-Parametric Test
# -----------------------------------------
# Using the shrimp_trial dataset:
# a) Perform Kruskal-Wallis test on survival_pct by treatment
# b) Perform post-hoc pairwise comparisons
# c) When would you use this instead of ANOVA?

# Your code here:



# -----------------------------------------
# EXERCISE 10: Complete Analysis
# -----------------------------------------
# Design and analyze a hypothetical experiment:
# You want to test if water temperature (25°C vs 30°C) and
# feeding frequency (2x vs 3x daily) affect fish growth.
#
# a) Create a simulated dataset with 6 replicates per treatment
# b) Perform appropriate statistical analysis
# c) Create visualization
# d) Write a brief interpretation

# Your code here:



# ============================================
# SOLUTIONS
# ============================================
# Scroll down...
#
#
#
#
#
#
#
#
#
#
#

# -----------------------------------------
# SOLUTION 1: Descriptive Statistics
# -----------------------------------------
# Calculate stats by treatment
stats_by_treatment <- aggregate(final_weight ~ treatment,
                                 data = shrimp_trial,
                                 FUN = function(x) c(
                                   mean = mean(x),
                                   sd = sd(x),
                                   se = sd(x)/sqrt(length(x)),
                                   cv = (sd(x)/mean(x))*100
                                 ))
print(stats_by_treatment)

# Alternative using tapply
tapply(shrimp_trial$final_weight, shrimp_trial$treatment, mean)
tapply(shrimp_trial$final_weight, shrimp_trial$treatment, sd)

# CV calculation
cv_by_treatment <- tapply(shrimp_trial$final_weight, shrimp_trial$treatment,
                           function(x) (sd(x)/mean(x))*100)
print(cv_by_treatment)
# Synbiotic has lowest CV = most consistent

# -----------------------------------------
# SOLUTION 2: Two-Sample T-Test
# -----------------------------------------
control <- shrimp_trial$final_weight[shrimp_trial$treatment == "Control"]
synbiotic <- shrimp_trial$final_weight[shrimp_trial$treatment == "Synbiotic"]

t_result <- t.test(control, synbiotic)
print(t_result)

# Cohen's d
cohens_d <- (mean(synbiotic) - mean(control)) /
            sqrt((var(control) + var(synbiotic)) / 2)
print(paste("Cohen's d:", round(cohens_d, 3)))
# Large effect size (d > 0.8)

# -----------------------------------------
# SOLUTION 3: One-Way ANOVA
# -----------------------------------------
# ANOVA
oyster_anova <- aov(shell_length ~ salinity, data = oyster_growth)
summary(oyster_anova)

# Normality check
shapiro.test(residuals(oyster_anova))

# Tukey HSD
TukeyHSD(oyster_anova)

# Visualization
ggplot(oyster_growth, aes(x = salinity, y = shell_length, fill = salinity)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1) +
  labs(
    title = "Oyster Shell Length by Salinity",
    x = "Salinity Level",
    y = "Shell Length (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# -----------------------------------------
# SOLUTION 4: Two-Way ANOVA
# -----------------------------------------
two_way <- aov(catch_kg ~ fishing_ground * season, data = fishing_data)
summary(two_way)

# Interaction plot
ggplot(fishing_data, aes(x = fishing_ground, y = catch_kg,
                          color = season, group = season)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  labs(
    title = "Catch by Fishing Ground and Season",
    x = "Fishing Ground",
    y = "Catch (kg)"
  ) +
  theme_minimal()

# Interaction is significant - effect of season depends on location

# -----------------------------------------
# SOLUTION 5: Correlation Analysis
# -----------------------------------------
# Correlation test
cor_result <- cor.test(reef_survey$coral_cover, reef_survey$fish_abundance)
print(cor_result)

# Correlation matrix
cor_matrix <- cor(reef_survey[, c("coral_cover", "fish_abundance",
                                   "water_clarity", "distance_to_shore")])
print(round(cor_matrix, 3))

# Scatter plot
ggplot(reef_survey, aes(x = coral_cover, y = fish_abundance)) +
  geom_point(size = 3, color = "coral") +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Coral Cover vs Fish Abundance",
    subtitle = paste("r =", round(cor_result$estimate, 3)),
    x = "Coral Cover (%)",
    y = "Fish Abundance"
  ) +
  theme_minimal()

# -----------------------------------------
# SOLUTION 6: Linear Regression
# -----------------------------------------
lm_model <- lm(fish_abundance ~ coral_cover, data = reef_survey)
summary(lm_model)

# R-squared
print(paste("R-squared:", round(summary(lm_model)$r.squared, 3)))

# Prediction
predict(lm_model, data.frame(coral_cover = 50))

# -----------------------------------------
# SOLUTION 7: Multiple Regression
# -----------------------------------------
multi_model <- lm(fish_abundance ~ coral_cover + water_clarity + distance_to_shore,
                   data = reef_survey)
summary(multi_model)

# Compare R-squared
print(paste("Simple R²:", round(summary(lm_model)$r.squared, 3)))
print(paste("Multiple R²:", round(summary(multi_model)$r.squared, 3)))

# -----------------------------------------
# SOLUTION 8: Chi-Square Test
# -----------------------------------------
chi_result <- chisq.test(species_habitat)
print(chi_result)

# Expected frequencies
print("Expected:")
print(round(chi_result$expected, 1))

# Residuals (how much observed differs from expected)
print("Residuals:")
print(round(chi_result$residuals, 2))

# -----------------------------------------
# SOLUTION 9: Non-Parametric Test
# -----------------------------------------
kruskal.test(survival_pct ~ treatment, data = shrimp_trial)

# Post-hoc
pairwise.wilcox.test(shrimp_trial$survival_pct, shrimp_trial$treatment,
                     p.adjust.method = "bonferroni")

# Use when: data is not normally distributed, ordinal data,
# small sample sizes, or outliers present

# -----------------------------------------
# SOLUTION 10: Complete Analysis
# -----------------------------------------
set.seed(123)
fish_experiment <- data.frame(
  temperature = factor(rep(c("25C", "30C"), each = 12)),
  feeding_freq = factor(rep(rep(c("2x_daily", "3x_daily"), each = 6), 2)),
  weight_gain = c(
    # 25°C, 2x daily
    rnorm(6, mean = 45, sd = 5),
    # 25°C, 3x daily
    rnorm(6, mean = 52, sd = 4),
    # 30°C, 2x daily
    rnorm(6, mean = 55, sd = 6),
    # 30°C, 3x daily
    rnorm(6, mean = 65, sd = 5)
  )
)

# Two-way ANOVA
exp_anova <- aov(weight_gain ~ temperature * feeding_freq, data = fish_experiment)
summary(exp_anova)

# Visualization
ggplot(fish_experiment, aes(x = temperature, y = weight_gain,
                             fill = feeding_freq)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Effect of Temperature and Feeding Frequency on Fish Growth",
    x = "Temperature",
    y = "Weight Gain (g)",
    fill = "Feeding Frequency"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Interpretation: Both temperature and feeding frequency significantly
# affect fish growth. Higher temperature and more frequent feeding
# result in greater weight gain.

# ============================================
# End of Statistical Exercises
# ============================================

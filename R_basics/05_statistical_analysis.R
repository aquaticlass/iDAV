# ============================================
# Statistical Data Analysis for Experimental Design
# Applications in Aquaculture, Marine Ecology & Fisheries
# ============================================

# -----------------------------------------
# REQUIRED PACKAGES
# -----------------------------------------

# Install packages (run once)
# install.packages(c("car", "agricolae", "multcomp", "MASS", "vegan"))

# Load packages
library(ggplot2)

# -----------------------------------------
# PART 1: DESCRIPTIVE STATISTICS
# -----------------------------------------

# Example: Shrimp weight measurements from a farm
shrimp_weights <- c(12.3, 14.5, 11.8, 15.2, 13.7, 14.1, 12.9,
                    15.8, 13.2, 14.6, 11.5, 16.1, 13.8, 14.3, 12.7)

# Central tendency
mean(shrimp_weights)       # Mean weight
median(shrimp_weights)     # Median weight

# Dispersion
sd(shrimp_weights)         # Standard deviation
var(shrimp_weights)        # Variance
range(shrimp_weights)      # Min and max
IQR(shrimp_weights)        # Interquartile range

# Standard error of the mean
se <- sd(shrimp_weights) / sqrt(length(shrimp_weights))
print(paste("Standard Error:", round(se, 3)))

# Summary statistics
summary(shrimp_weights)

# Coefficient of variation (CV%)
cv <- (sd(shrimp_weights) / mean(shrimp_weights)) * 100
print(paste("CV:", round(cv, 2), "%"))

# -----------------------------------------
# PART 2: EXPERIMENTAL DESIGN CONCEPTS
# -----------------------------------------

# Key principles:
# 1. REPLICATION - Multiple experimental units per treatment
# 2. RANDOMIZATION - Random assignment of treatments
# 3. CONTROL - Baseline for comparison
# 4. BLOCKING - Group similar units to reduce variability

# Example: Setting up a feeding trial
set.seed(123)  # For reproducibility

# Create experimental design: 4 treatments, 5 replicates
treatments <- rep(c("Control", "Diet_A", "Diet_B", "Diet_C"), each = 5)
tank_id <- paste0("Tank_", 1:20)

# Randomize treatment assignment
experiment <- data.frame(
  tank = tank_id,
  treatment = sample(treatments)  # Random assignment
)
print(experiment)

# -----------------------------------------
# PART 3: T-TESTS (Comparing Two Groups)
# -----------------------------------------

# Example: Comparing fish growth between two feeding regimes

# Simulated data: Weight gain (g) after 8 weeks
control_diet <- c(45.2, 48.1, 42.3, 50.5, 44.8, 47.2, 43.9, 46.5)
enriched_diet <- c(52.3, 55.8, 49.7, 58.2, 53.1, 56.4, 51.2, 54.8)

# Two-sample t-test (independent samples)
t_result <- t.test(control_diet, enriched_diet)
print(t_result)

# Interpretation:
# p-value < 0.05 = significant difference between diets

# Effect size (Cohen's d)
cohens_d <- (mean(enriched_diet) - mean(control_diet)) /
            sqrt((var(control_diet) + var(enriched_diet)) / 2)
print(paste("Cohen's d:", round(cohens_d, 3)))

# Paired t-test example: Before/after treatment
# Fish weight before and after antibiotic treatment
before_treatment <- c(120, 135, 128, 142, 138, 125, 130, 145)
after_treatment <- c(118, 132, 125, 140, 134, 122, 127, 141)

paired_result <- t.test(before_treatment, after_treatment, paired = TRUE)
print(paired_result)

# Visualize the comparison
diet_data <- data.frame(
  weight_gain = c(control_diet, enriched_diet),
  diet = rep(c("Control", "Enriched"), each = 8)
)

ggplot(diet_data, aes(x = diet, y = weight_gain, fill = diet)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, size = 2) +
  labs(
    title = "Fish Weight Gain by Diet Type",
    subtitle = paste("t-test p-value:", round(t_result$p.value, 4)),
    x = "Diet Type",
    y = "Weight Gain (g)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# -----------------------------------------
# PART 4: ONE-WAY ANOVA (Comparing 3+ Groups)
# -----------------------------------------

# Example: Effect of stocking density on tilapia growth
# Three density treatments: Low, Medium, High

set.seed(456)
fish_growth <- data.frame(
  density = factor(rep(c("Low", "Medium", "High"), each = 10)),
  weight_gain = c(
    rnorm(10, mean = 85, sd = 8),   # Low density
    rnorm(10, mean = 72, sd = 10),  # Medium density
    rnorm(10, mean = 58, sd = 12)   # High density
  )
)

# Order factor levels
fish_growth$density <- factor(fish_growth$density,
                               levels = c("Low", "Medium", "High"))

# Perform one-way ANOVA
anova_model <- aov(weight_gain ~ density, data = fish_growth)
summary(anova_model)

# Check assumptions
# 1. Normality of residuals
shapiro.test(residuals(anova_model))

# 2. Homogeneity of variances (Levene's test)
# library(car)
# leveneTest(weight_gain ~ density, data = fish_growth)

# Post-hoc test: Tukey HSD
tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

# Visualize results
ggplot(fish_growth, aes(x = density, y = weight_gain, fill = density)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    title = "Effect of Stocking Density on Tilapia Growth",
    x = "Stocking Density",
    y = "Weight Gain (g)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

# -----------------------------------------
# PART 5: TWO-WAY ANOVA (Factorial Design)
# -----------------------------------------

# Example: Effect of temperature AND salinity on shrimp survival
# Factor A: Temperature (25°C, 30°C)
# Factor B: Salinity (15 ppt, 25 ppt, 35 ppt)

set.seed(789)
shrimp_experiment <- data.frame(
  temperature = factor(rep(c("25C", "30C"), each = 18)),
  salinity = factor(rep(rep(c("15ppt", "25ppt", "35ppt"), each = 6), 2)),
  survival_rate = c(
    # 25°C
    rnorm(6, 75, 5),   # 15 ppt
    rnorm(6, 88, 4),   # 25 ppt
    rnorm(6, 82, 5),   # 35 ppt
    # 30°C
    rnorm(6, 70, 6),   # 15 ppt
    rnorm(6, 85, 5),   # 25 ppt
    rnorm(6, 72, 7)    # 35 ppt
  )
)

# Two-way ANOVA
two_way_anova <- aov(survival_rate ~ temperature * salinity,
                      data = shrimp_experiment)
summary(two_way_anova)

# Interpretation:
# - Main effect of temperature
# - Main effect of salinity
# - Interaction effect (temperature:salinity)

# Interaction plot
ggplot(shrimp_experiment, aes(x = salinity, y = survival_rate,
                               color = temperature, group = temperature)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  labs(
    title = "Interaction: Temperature x Salinity on Shrimp Survival",
    x = "Salinity",
    y = "Survival Rate (%)",
    color = "Temperature"
  ) +
  theme_minimal()

# -----------------------------------------
# PART 6: CORRELATION ANALYSIS
# -----------------------------------------

# Example: Relationship between water quality and fish growth

water_quality <- data.frame(
  dissolved_oxygen = c(6.2, 7.1, 5.8, 7.5, 6.8, 7.3, 5.5, 6.9, 7.0, 6.5,
                       7.2, 5.9, 6.7, 7.4, 6.3),
  temperature = c(26, 28, 25, 29, 27, 28, 24, 27, 28, 26,
                  29, 25, 27, 29, 26),
  fish_weight = c(245, 280, 220, 295, 265, 285, 200, 270, 275, 250,
                  290, 225, 260, 300, 240)
)

# Pearson correlation
cor(water_quality$dissolved_oxygen, water_quality$fish_weight)

# Correlation test with p-value
cor_test <- cor.test(water_quality$dissolved_oxygen, water_quality$fish_weight)
print(cor_test)

# Correlation matrix
cor_matrix <- cor(water_quality)
print(round(cor_matrix, 3))

# Visualize correlation
ggplot(water_quality, aes(x = dissolved_oxygen, y = fish_weight)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Dissolved Oxygen vs Fish Weight",
    subtitle = paste("r =", round(cor_test$estimate, 3),
                     ", p =", round(cor_test$p.value, 4)),
    x = "Dissolved Oxygen (mg/L)",
    y = "Fish Weight (g)"
  ) +
  theme_minimal()

# -----------------------------------------
# PART 7: LINEAR REGRESSION
# -----------------------------------------

# Example: Predicting fish yield based on feed input

aquaculture_data <- data.frame(
  feed_input = c(100, 150, 200, 250, 300, 350, 400, 450, 500, 550),
  fish_yield = c(180, 260, 350, 420, 510, 580, 660, 720, 800, 870)
)

# Simple linear regression
lm_model <- lm(fish_yield ~ feed_input, data = aquaculture_data)
summary(lm_model)

# Model coefficients
coef(lm_model)
# Intercept: yield when feed = 0
# Slope: increase in yield per unit feed

# R-squared: proportion of variance explained
summary(lm_model)$r.squared

# Predict new values
new_feed <- data.frame(feed_input = c(275, 425))
predict(lm_model, new_feed)

# Confidence intervals for predictions
predict(lm_model, new_feed, interval = "confidence")

# Visualize regression
ggplot(aquaculture_data, aes(x = feed_input, y = fish_yield)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Fish Yield vs Feed Input",
    subtitle = paste("R² =", round(summary(lm_model)$r.squared, 3)),
    x = "Feed Input (kg)",
    y = "Fish Yield (kg)"
  ) +
  theme_minimal()

# Check regression assumptions
par(mfrow = c(2, 2))
plot(lm_model)
par(mfrow = c(1, 1))

# -----------------------------------------
# PART 8: MULTIPLE REGRESSION
# -----------------------------------------

# Example: Fish growth predicted by multiple environmental factors

pond_data <- data.frame(
  temperature = c(25, 27, 28, 26, 29, 24, 27, 28, 26, 25,
                  28, 27, 29, 26, 24),
  dissolved_oxygen = c(6.5, 7.2, 6.8, 7.0, 6.2, 7.5, 6.9, 7.1, 6.7, 7.3,
                       6.4, 7.0, 6.1, 6.8, 7.4),
  ph = c(7.2, 7.5, 7.8, 7.4, 8.0, 7.1, 7.6, 7.7, 7.3, 7.2,
         7.9, 7.5, 8.1, 7.4, 7.0),
  fish_growth = c(2.1, 2.8, 2.5, 2.4, 2.2, 2.6, 2.7, 2.9, 2.3, 2.5,
                  2.3, 2.6, 2.0, 2.4, 2.7)
)

# Multiple regression model
multi_model <- lm(fish_growth ~ temperature + dissolved_oxygen + ph,
                   data = pond_data)
summary(multi_model)

# Check which predictors are significant
# Look for p-values < 0.05 in the coefficients table

# -----------------------------------------
# PART 9: CHI-SQUARE TEST
# -----------------------------------------

# Example: Testing if fish species distribution differs between habitats

# Observed counts
habitat_data <- matrix(c(
  45, 30, 25,   # Reef
  20, 45, 35,   # Seagrass
  35, 25, 40    # Sandy bottom
), nrow = 3, byrow = TRUE)

rownames(habitat_data) <- c("Reef", "Seagrass", "Sandy")
colnames(habitat_data) <- c("Species_A", "Species_B", "Species_C")

print(habitat_data)

# Chi-square test
chi_result <- chisq.test(habitat_data)
print(chi_result)

# Expected frequencies (if no association)
chi_result$expected

# -----------------------------------------
# PART 10: NON-PARAMETRIC TESTS
# -----------------------------------------

# Use when data doesn't meet normality assumptions

# Mann-Whitney U test (alternative to t-test)
# Example: Comparing parasite counts between wild and farmed fish

wild_fish <- c(12, 8, 15, 22, 18, 25, 14, 20)
farmed_fish <- c(3, 5, 2, 7, 4, 6, 3, 5)

wilcox_result <- wilcox.test(wild_fish, farmed_fish)
print(wilcox_result)

# Kruskal-Wallis test (alternative to one-way ANOVA)
# Example: Species abundance across 3 sites

abundance_data <- data.frame(
  site = factor(rep(c("Site_A", "Site_B", "Site_C"), each = 8)),
  count = c(
    c(15, 12, 18, 20, 14, 16, 19, 13),  # Site A
    c(8, 5, 10, 7, 6, 9, 8, 5),          # Site B
    c(25, 30, 22, 28, 32, 27, 24, 29)   # Site C
  )
)

kruskal_result <- kruskal.test(count ~ site, data = abundance_data)
print(kruskal_result)

# Post-hoc pairwise comparisons
pairwise.wilcox.test(abundance_data$count, abundance_data$site,
                     p.adjust.method = "bonferroni")

# -----------------------------------------
# PART 11: SURVIVAL ANALYSIS BASICS
# -----------------------------------------

# Example: Fish survival over time in different treatments

# Simple survival calculation
initial_count <- 100
final_count <- 78
survival_rate <- (final_count / initial_count) * 100
print(paste("Survival Rate:", survival_rate, "%"))

# Comparing survival between groups (proportion test)
# Treatment A: 82/100 survived, Treatment B: 68/100 survived
prop_result <- prop.test(c(82, 68), c(100, 100))
print(prop_result)

# -----------------------------------------
# SUMMARY: CHOOSING THE RIGHT TEST
# -----------------------------------------

# COMPARING TWO GROUPS:
# - Parametric (normal data): t-test
# - Non-parametric: Mann-Whitney U / Wilcoxon

# COMPARING 3+ GROUPS:
# - Parametric: One-way ANOVA + Tukey HSD
# - Non-parametric: Kruskal-Wallis + pairwise Wilcoxon

# FACTORIAL DESIGNS:
# - Two-way ANOVA (2 factors)
# - Multi-factor ANOVA

# RELATIONSHIPS:
# - Correlation (strength of linear relationship)
# - Linear regression (prediction)
# - Multiple regression (multiple predictors)

# CATEGORICAL DATA:
# - Chi-square test (independence/association)
# - Fisher's exact test (small samples)

# ============================================
# End of Statistical Analysis Tutorial
# ============================================

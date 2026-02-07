# ============================================
# DIRECT (CONSTRAINED) ORDINATION METHODS
# Comprehensive Guide for Marine Ecology
# ============================================

# Direct ordination CONSTRAINS the ordination axes to be
# linear combinations of environmental variables. This directly
# tests species-environment relationships.

# -----------------------------------------
# REQUIRED PACKAGES
# -----------------------------------------

# install.packages(c("vegan", "ggplot2", "ggrepel"))

library(vegan)
library(ggplot2)

# -----------------------------------------
# SAMPLE DATASETS
# -----------------------------------------

# Dataset 1: Reef Fish Assemblages
# Fish abundance at 30 reef sites with environmental data

set.seed(456)

reef_fish <- data.frame(
  # Coral-associated species
  Chaetodon_trifasciatus = c(18, 22, 20, 25, 15, 12, 8, 5, 3, 2,
                              24, 28, 22, 20, 18, 10, 6, 4, 2, 1,
                              26, 30, 25, 22, 16, 8, 5, 3, 1, 0),
  Chaetodon_lunulatus = c(15, 18, 16, 20, 12, 10, 6, 4, 2, 1,
                           20, 24, 18, 16, 14, 8, 5, 3, 1, 0,
                           22, 26, 20, 18, 12, 6, 4, 2, 1, 0),
  Chromis_viridis = c(85, 95, 90, 100, 70, 55, 40, 30, 20, 15,
                       100, 110, 95, 85, 75, 50, 35, 25, 15, 10,
                       105, 120, 100, 90, 65, 45, 30, 20, 10, 5),

  # Generalist species
  Thalassoma_lunare = c(25, 28, 26, 30, 28, 30, 32, 35, 38, 40,
                         30, 32, 28, 26, 28, 32, 36, 40, 42, 45,
                         28, 30, 27, 25, 30, 35, 38, 42, 45, 48),
  Halichoeres_hortulanus = c(18, 20, 19, 22, 20, 22, 25, 28, 30, 32,
                              22, 24, 20, 18, 20, 24, 28, 32, 35, 38,
                              20, 22, 18, 17, 22, 28, 32, 36, 40, 42),

  # Rubble/algae specialists
  Stegastes_nigricans = c(5, 6, 5, 4, 10, 15, 22, 30, 38, 45,
                           4, 5, 6, 8, 12, 20, 28, 35, 42, 50,
                           3, 4, 5, 7, 15, 25, 32, 40, 48, 55),
  Acanthurus_nigrofuscus = c(8, 10, 9, 8, 15, 20, 28, 35, 42, 50,
                              6, 8, 10, 12, 18, 25, 32, 40, 48, 55,
                              5, 7, 8, 10, 20, 30, 38, 45, 52, 60),

  # Sand/rubble species
  Parupeneus_multifasciatus = c(12, 10, 11, 8, 15, 18, 22, 25, 28, 30,
                                 8, 6, 10, 12, 16, 20, 25, 28, 32, 35,
                                 6, 5, 8, 10, 18, 24, 28, 32, 36, 40),
  Mulloidichthys_flavolineatus = c(10, 8, 9, 6, 12, 16, 20, 24, 28, 32,
                                    6, 5, 8, 10, 14, 18, 24, 28, 32, 36,
                                    5, 4, 6, 8, 16, 22, 28, 32, 38, 42)
)

rownames(reef_fish) <- paste0("Reef_", sprintf("%02d", 1:30))

# Environmental data
reef_env <- data.frame(
  site = paste0("Reef_", sprintf("%02d", 1:30)),
  coral_cover = c(65, 72, 68, 75, 45, 32, 20, 12, 6, 3,
                  70, 78, 66, 62, 50, 28, 18, 10, 5, 2,
                  72, 80, 70, 65, 42, 22, 14, 8, 4, 1),
  structural_complexity = c(4.2, 4.5, 4.3, 4.8, 3.5, 2.8, 2.2, 1.5, 1.0, 0.8,
                            4.4, 4.8, 4.2, 4.0, 3.6, 2.5, 1.8, 1.2, 0.9, 0.6,
                            4.5, 5.0, 4.4, 4.2, 3.2, 2.0, 1.4, 1.0, 0.7, 0.5),
  algae_cover = c(8, 6, 7, 5, 18, 28, 38, 48, 58, 65,
                  6, 4, 8, 10, 20, 32, 42, 52, 60, 68,
                  5, 3, 6, 8, 22, 38, 48, 56, 64, 72),
  depth_m = c(8, 10, 9, 12, 8, 6, 5, 4, 3, 2,
              12, 15, 10, 8, 7, 5, 4, 3, 2, 2,
              15, 18, 12, 10, 6, 4, 3, 3, 2, 1),
  sedimentation = c(1.2, 1.0, 1.1, 0.8, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5,
                    1.0, 0.8, 1.2, 1.5, 2.8, 4.0, 5.0, 6.0, 7.0, 8.0,
                    0.9, 0.6, 1.0, 1.2, 3.2, 4.5, 5.5, 6.5, 7.5, 8.5),
  distance_river_km = c(15, 18, 16, 20, 10, 6, 4, 2.5, 1.5, 0.8,
                        18, 22, 15, 12, 8, 5, 3, 2, 1.2, 0.5,
                        20, 25, 18, 14, 7, 4, 2.5, 1.5, 1.0, 0.3),
  protection = factor(c(rep("MPA", 10), rep("Partial", 10), rep("Open", 10)))
)

# Dataset 2: Estuarine Macrobenthos
macrobenthos <- data.frame(
  Hediste_diversicolor = c(45, 52, 48, 55, 35, 28, 20, 15, 8, 5, 2, 0),
  Corophium_volutator = c(120, 135, 125, 140, 85, 60, 40, 25, 12, 5, 2, 0),
  Hydrobia_ulvae = c(250, 280, 260, 290, 180, 120, 80, 50, 25, 10, 3, 0),
  Macoma_balthica = c(35, 40, 38, 42, 45, 48, 50, 52, 48, 42, 35, 28),
  Cerastoderma_edule = c(15, 18, 16, 20, 28, 35, 42, 48, 52, 55, 50, 45),
  Arenicola_marina = c(5, 8, 6, 10, 18, 25, 32, 38, 42, 45, 48, 50),
  Nephtys_hombergii = c(3, 5, 4, 6, 12, 18, 25, 32, 38, 42, 48, 52),
  Scoloplos_armiger = c(2, 3, 2, 4, 8, 15, 22, 30, 38, 45, 52, 58)
)
rownames(macrobenthos) <- paste0("Est_", sprintf("%02d", 1:12))

estuary_env <- data.frame(
  station = paste0("Est_", sprintf("%02d", 1:12)),
  salinity = c(5, 8, 6, 10, 15, 20, 25, 28, 30, 32, 34, 35),
  organic_matter = c(12, 10, 11, 9, 6, 4.5, 3.5, 2.8, 2.2, 1.8, 1.5, 1.2),
  mud_content = c(85, 80, 82, 78, 60, 45, 35, 28, 22, 18, 15, 12),
  distance_mouth_km = c(25, 22, 24, 20, 15, 11, 8, 6, 4, 3, 2, 1),
  tidal_height = c(3.2, 3.0, 3.1, 2.8, 2.4, 2.0, 1.6, 1.3, 1.0, 0.8, 0.6, 0.4)
)


# =========================================
# PART 1: UNDERSTANDING CONSTRAINED ORDINATION
# =========================================

cat("
=========================================
CONSTRAINED vs UNCONSTRAINED ORDINATION
=========================================

UNCONSTRAINED (Indirect):
- Ordination axes capture MAXIMUM variance in species data
- Environmental interpretation comes AFTER
- Examples: PCA, CA, NMDS, PCoA

CONSTRAINED (Direct):
- Ordination axes are LINEAR COMBINATIONS of environmental variables
- Directly models species-environment relationships
- Only explains variance RELATED to your predictors
- Examples: RDA, CCA, db-RDA, CAP

KEY CONCEPTS:

1. TOTAL VARIANCE = Constrained + Unconstrained
   - Constrained: Explained by environmental variables
   - Unconstrained (Residual): Not explained

2. INERTIA
   - Total variation in species data
   - Partitioned into constrained/unconstrained

3. ADJUSTED R²
   - Proportion of variance explained
   - Adjusted for number of predictors

WHEN TO USE CONSTRAINED ORDINATION:
- You have environmental predictors
- You want to test species-environment relationships
- You want to partition variance among variable groups
=========================================
")


# =========================================
# PART 2: RDA - Redundancy Analysis
# =========================================

cat("
=========================================
RDA - REDUNDANCY ANALYSIS
=========================================

WHAT IT IS:
- Constrained version of PCA
- 'PCA of fitted values from multiple regression'
- Assumes LINEAR species-environment relationships

WHEN TO USE:
- Short ecological gradients (< 2 SD)
- Environmental data as predictors
- Transformed community data (Hellinger recommended)

KEY OUTPUTS:
- Constrained axes (RDA1, RDA2, ...)
- Unconstrained axes (PC1, PC2, ...)
- Variable loadings (biplot arrows)
- Species scores
- Explained variance

INTERPRETATION:
- Arrows point in direction of increasing values
- Arrow length = correlation strength
- Angle between arrows = correlation between variables
- Species near arrow tip = positive association
=========================================
")

# -----------------------------------------
# RDA Analysis - Reef Fish Example
# -----------------------------------------

# Step 1: Transform community data (Hellinger transformation)
reef_fish_hell <- decostand(reef_fish, method = "hellinger")

# Step 2: Prepare environmental matrix (numeric variables only)
env_matrix <- reef_env[, c("coral_cover", "structural_complexity",
                            "algae_cover", "depth_m", "sedimentation",
                            "distance_river_km")]

# Step 3: Check for collinearity
print("Correlation matrix of environmental variables:")
print(round(cor(env_matrix), 2))

# VIF (Variance Inflation Factor) check
# VIF > 10 indicates problematic collinearity

# Step 4: Perform RDA
rda_full <- rda(reef_fish_hell ~ coral_cover + structural_complexity +
                  algae_cover + depth_m + sedimentation + distance_river_km,
                data = reef_env)

# Check VIF
print("Variance Inflation Factors:")
print(vif.cca(rda_full))
# If VIF > 10, remove that variable

# Step 5: Examine RDA results
print(rda_full)
summary(rda_full)

# -----------------------------------------
# RDA Variance Explained
# -----------------------------------------

# Total, constrained, and unconstrained variance
rda_summary <- summary(rda_full)

total_var <- rda_full$tot.chi
constrained_var <- rda_full$CCA$tot.chi
unconstrained_var <- rda_full$CA$tot.chi

cat("\n=== VARIANCE PARTITIONING ===\n")
cat("Total inertia:", round(total_var, 4), "\n")
cat("Constrained:", round(constrained_var, 4),
    "(", round(constrained_var/total_var * 100, 1), "%)\n")
cat("Unconstrained:", round(unconstrained_var, 4),
    "(", round(unconstrained_var/total_var * 100, 1), "%)\n")

# Adjusted R-squared (more reliable)
r2_adj <- RsquareAdj(rda_full)
cat("\nR² =", round(r2_adj$r.squared, 3), "\n")
cat("Adjusted R² =", round(r2_adj$adj.r.squared, 3), "\n")

# Variance explained by each axis
rda_eig <- rda_full$CCA$eig
rda_var_axis <- rda_eig / total_var * 100

print("\nVariance explained by constrained axes:")
print(data.frame(
  Axis = paste0("RDA", 1:length(rda_eig)),
  Eigenvalue = round(rda_eig, 4),
  Variance_Pct = round(rda_var_axis, 2),
  Cumulative = round(cumsum(rda_var_axis), 2)
))

# -----------------------------------------
# RDA Significance Testing
# -----------------------------------------

# Test overall RDA significance
anova_rda <- anova(rda_full, permutations = 999)
print("Overall RDA significance:")
print(anova_rda)

# Test significance of each axis
anova_axis <- anova(rda_full, by = "axis", permutations = 999)
print("Significance of each axis:")
print(anova_axis)

# Test significance of each variable (Type III)
anova_terms <- anova(rda_full, by = "terms", permutations = 999)
print("Significance of each environmental variable:")
print(anova_terms)

# Marginal effects (Type I - sequential)
anova_margin <- anova(rda_full, by = "margin", permutations = 999)
print("Marginal effects:")
print(anova_margin)

# -----------------------------------------
# RDA Visualization with ggplot2
# -----------------------------------------

# Extract scores
# Scaling 1: Site-focused (distances among sites preserved)
# Scaling 2: Species-focused (species relationships preserved)

# Using Scaling 2 (species-focused, symmetric)
rda_sites <- as.data.frame(scores(rda_full, display = "sites", scaling = 2))
rda_species <- as.data.frame(scores(rda_full, display = "species", scaling = 2))
rda_biplot <- as.data.frame(scores(rda_full, display = "bp", scaling = 2))

rda_sites$site <- rownames(rda_sites)
rda_sites$protection <- reef_env$protection
rda_sites$coral_cover <- reef_env$coral_cover

rda_species$species <- rownames(rda_species)
rda_biplot$variable <- rownames(rda_biplot)

# Calculate arrow scaling factor
arrow_scale <- 0.8

# Create RDA triplot
rda_plot <- ggplot() +
  # Site points
  geom_point(data = rda_sites,
             aes(x = RDA1, y = RDA2, color = protection, size = coral_cover),
             alpha = 0.7) +
  scale_size_continuous(range = c(2, 6)) +

  # Species labels
  geom_text(data = rda_species,
            aes(x = RDA1, y = RDA2, label = species),
            size = 2.5, color = "darkgreen", fontface = "italic") +

  # Environmental arrows
  geom_segment(data = rda_biplot,
               aes(x = 0, y = 0, xend = RDA1 * arrow_scale, yend = RDA2 * arrow_scale),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "darkred", linewidth = 0.8) +
  geom_text(data = rda_biplot,
            aes(x = RDA1 * arrow_scale * 1.1, y = RDA2 * arrow_scale * 1.1,
                label = variable),
            color = "darkred", size = 3, fontface = "bold") +

  # Labels
  labs(
    title = "RDA Triplot - Reef Fish Communities",
    subtitle = paste("Adjusted R² =", round(r2_adj$adj.r.squared, 3)),
    x = paste0("RDA1 (", round(rda_var_axis[1], 1), "%)"),
    y = paste0("RDA2 (", round(rda_var_axis[2], 1), "%)"),
    color = "Protection",
    size = "Coral Cover (%)"
  ) +
  theme_minimal() +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70")

print(rda_plot)

# Alternative: Site scores only with ellipses
rda_site_plot <- ggplot(rda_sites, aes(x = RDA1, y = RDA2)) +
  geom_point(aes(color = protection), size = 3) +
  stat_ellipse(aes(color = protection), level = 0.95) +
  # Add environmental arrows
  geom_segment(data = rda_biplot,
               aes(x = 0, y = 0, xend = RDA1 * arrow_scale, yend = RDA2 * arrow_scale),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "red", linewidth = 0.7) +
  geom_text(data = rda_biplot,
            aes(x = RDA1 * arrow_scale * 1.15, y = RDA2 * arrow_scale * 1.15,
                label = variable),
            color = "red", size = 2.8) +
  labs(
    title = "RDA - Sites by Protection Status",
    x = paste0("RDA1 (", round(rda_var_axis[1], 1), "%)"),
    y = paste0("RDA2 (", round(rda_var_axis[2], 1), "%)")
  ) +
  theme_minimal() +
  coord_fixed()

print(rda_site_plot)

# -----------------------------------------
# RDA Model Selection
# -----------------------------------------

# Forward selection of variables
rda_null <- rda(reef_fish_hell ~ 1, data = reef_env)
rda_forward <- ordistep(rda_null,
                         scope = formula(rda_full),
                         direction = "forward",
                         permutations = 999)

print("Forward-selected RDA model:")
print(rda_forward)

# Compare models with AIC (using capscale for AIC)
# anova(rda_forward, rda_full, permutations = 999)


# =========================================
# PART 3: CCA - Canonical Correspondence Analysis
# =========================================

cat("
=========================================
CCA - CANONICAL CORRESPONDENCE ANALYSIS
=========================================

WHAT IT IS:
- Constrained version of CA
- Assumes UNIMODAL species-environment relationships
- Species have optimal environmental conditions

WHEN TO USE:
- Long ecological gradients (> 4 SD)
- Count/abundance data
- Species turnover along gradients

DIFFERENCE FROM RDA:
- RDA: Linear responses (short gradients)
- CCA: Unimodal responses (long gradients)
- CCA uses chi-square distances

KEY OUTPUTS:
- Same structure as RDA
- Species optima along environmental gradients
- Site scores as weighted averages
=========================================
")

# -----------------------------------------
# CCA Analysis - Estuarine Macrobenthos
# -----------------------------------------

# CCA with raw abundance data (no transformation needed)
cca_result <- cca(macrobenthos ~ salinity + organic_matter + mud_content +
                    distance_mouth_km + tidal_height,
                  data = estuary_env)

# Summary
print(cca_result)

# Variance explained
cca_total <- cca_result$tot.chi
cca_constrained <- cca_result$CCA$tot.chi
cca_r2 <- RsquareAdj(cca_result)

cat("\n=== CCA VARIANCE ===\n")
cat("Total inertia:", round(cca_total, 4), "\n")
cat("Constrained:", round(cca_constrained, 4),
    "(", round(cca_constrained/cca_total * 100, 1), "%)\n")
cat("Adjusted R²:", round(cca_r2$adj.r.squared, 3), "\n")

# Eigenvalues per axis
cca_eig <- cca_result$CCA$eig
cca_var_axis <- cca_eig / cca_total * 100

# Significance tests
anova(cca_result, permutations = 999)
anova(cca_result, by = "terms", permutations = 999)

# -----------------------------------------
# CCA Visualization
# -----------------------------------------

# Extract scores
cca_sites <- as.data.frame(scores(cca_result, display = "sites", scaling = 2))
cca_species <- as.data.frame(scores(cca_result, display = "species", scaling = 2))
cca_biplot <- as.data.frame(scores(cca_result, display = "bp", scaling = 2))

cca_sites$station <- estuary_env$station
cca_sites$salinity <- estuary_env$salinity
cca_species$species <- rownames(cca_species)
cca_biplot$variable <- rownames(cca_biplot)

# CCA triplot
cca_arrow_scale <- 1.2

cca_plot <- ggplot() +
  # Sites colored by salinity gradient
  geom_point(data = cca_sites,
             aes(x = CCA1, y = CCA2, color = salinity),
             size = 4) +
  scale_color_viridis_c(option = "plasma") +

  # Species as triangles with labels
  geom_point(data = cca_species,
             aes(x = CCA1, y = CCA2),
             shape = 17, size = 3, color = "darkgreen") +
  geom_text(data = cca_species,
            aes(x = CCA1, y = CCA2, label = species),
            size = 2.5, vjust = -0.8, color = "darkgreen", fontface = "italic") +

  # Environmental arrows
  geom_segment(data = cca_biplot,
               aes(x = 0, y = 0, xend = CCA1 * cca_arrow_scale, yend = CCA2 * cca_arrow_scale),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "red", linewidth = 0.8) +
  geom_text(data = cca_biplot,
            aes(x = CCA1 * cca_arrow_scale * 1.1, y = CCA2 * cca_arrow_scale * 1.1,
                label = variable),
            color = "red", size = 3, fontface = "bold") +

  labs(
    title = "CCA Triplot - Estuarine Macrobenthos",
    subtitle = paste("Adjusted R² =", round(cca_r2$adj.r.squared, 3)),
    x = paste0("CCA1 (", round(cca_var_axis[1], 1), "%)"),
    y = paste0("CCA2 (", round(cca_var_axis[2], 1), "%)"),
    color = "Salinity"
  ) +
  theme_minimal() +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70")

print(cca_plot)


# =========================================
# PART 4: db-RDA - Distance-based RDA
# =========================================

cat("
=========================================
db-RDA - DISTANCE-BASED RDA
=========================================

WHAT IT IS:
- RDA performed on principal coordinates
- Allows ANY distance measure
- Also called CAP (Canonical Analysis of Principal coordinates)

WHEN TO USE:
- When Bray-Curtis (or other) distance is preferred
- Community data that doesn't fit RDA/CCA assumptions
- More flexible than standard RDA

FUNCTION:
- capscale() in vegan (same syntax as rda/cca)
- dbrda() is an alias

ADVANTAGES:
- Use any ecologically meaningful distance
- Combines flexibility of NMDS with inference of RDA
=========================================
")

# -----------------------------------------
# db-RDA Analysis
# -----------------------------------------

# db-RDA with Bray-Curtis distance
dbrda_result <- capscale(reef_fish ~ coral_cover + structural_complexity +
                           algae_cover + depth_m + sedimentation,
                         data = reef_env,
                         distance = "bray")

# Alternative syntax using dbrda()
# dbrda_result <- dbrda(reef_fish ~ coral_cover + structural_complexity +
#                        algae_cover + depth_m + sedimentation,
#                       data = reef_env, distance = "bray")

print(dbrda_result)

# Variance explained
dbrda_r2 <- RsquareAdj(dbrda_result)
cat("\ndb-RDA Adjusted R²:", round(dbrda_r2$adj.r.squared, 3), "\n")

# Significance tests
anova(dbrda_result, permutations = 999)
anova(dbrda_result, by = "terms", permutations = 999)

# -----------------------------------------
# db-RDA Visualization
# -----------------------------------------

dbrda_sites <- as.data.frame(scores(dbrda_result, display = "sites"))
dbrda_biplot <- as.data.frame(scores(dbrda_result, display = "bp"))

dbrda_sites$protection <- reef_env$protection
dbrda_sites$coral_cover <- reef_env$coral_cover
dbrda_biplot$variable <- rownames(dbrda_biplot)

dbrda_eig <- dbrda_result$CCA$eig
dbrda_var <- dbrda_eig / sum(dbrda_eig) * 100

dbrda_plot <- ggplot() +
  geom_point(data = dbrda_sites,
             aes(x = CAP1, y = CAP2, color = protection, size = coral_cover),
             alpha = 0.7) +
  stat_ellipse(data = dbrda_sites,
               aes(x = CAP1, y = CAP2, color = protection),
               level = 0.95) +
  geom_segment(data = dbrda_biplot,
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "red", linewidth = 0.7) +
  geom_text(data = dbrda_biplot,
            aes(x = CAP1 * 1.15, y = CAP2 * 1.15, label = variable),
            color = "red", size = 3) +
  labs(
    title = "db-RDA (CAP) - Reef Fish Communities",
    subtitle = paste("Bray-Curtis distance, Adj R² =", round(dbrda_r2$adj.r.squared, 3)),
    x = paste0("CAP1 (", round(dbrda_var[1], 1), "%)"),
    y = paste0("CAP2 (", round(dbrda_var[2], 1), "%)")
  ) +
  theme_minimal() +
  coord_fixed()

print(dbrda_plot)


# =========================================
# PART 5: PARTIAL ORDINATION
# =========================================

cat("
=========================================
PARTIAL ORDINATION
=========================================

WHAT IT IS:
- Remove effect of 'conditioning' (nuisance) variables
- Analyze remaining variation with other predictors
- Similar to partial regression

USES:
- Control for spatial autocorrelation
- Remove effect of depth when testing other factors
- Account for sampling design (blocks)

SYNTAX:
rda(Y ~ X | Z)    # X = predictors, Z = conditioning
cca(Y ~ X | Z)
capscale(Y ~ X | Z, distance = 'bray')
=========================================
")

# -----------------------------------------
# Partial RDA Example
# -----------------------------------------

# Control for depth effect, then test other variables
partial_rda <- rda(reef_fish_hell ~ coral_cover + algae_cover + sedimentation |
                     depth_m,
                   data = reef_env)

print(partial_rda)
print(RsquareAdj(partial_rda))

anova(partial_rda, permutations = 999)

# Compare with full model
cat("\nFull model R²:", round(RsquareAdj(rda_full)$adj.r.squared, 3), "\n")
cat("Partial model R² (after removing depth):",
    round(RsquareAdj(partial_rda)$adj.r.squared, 3), "\n")


# =========================================
# PART 6: VARIANCE PARTITIONING
# =========================================

cat("
=========================================
VARIANCE PARTITIONING
=========================================

WHAT IT IS:
- Partition community variation among variable groups
- Identify unique vs shared contributions
- Based on partial RDA/CCA

OUTPUT:
- [a] = Unique contribution of group 1
- [b] = Shared contribution
- [c] = Unique contribution of group 2
- [d] = Unexplained (residual)

IMPORTANT:
- Shared fractions can be NEGATIVE
- Negative values indicate suppression effects
- Use adjusted R² for unbiased estimates
=========================================
")

# -----------------------------------------
# Variance Partitioning Example
# -----------------------------------------

# Group 1: Habitat structure (coral, complexity)
# Group 2: Water quality (sedimentation, distance to river)
# Group 3: Physical (depth)

varpart_result <- varpart(reef_fish_hell,
                           ~ coral_cover + structural_complexity,  # Group 1: Habitat
                           ~ sedimentation + distance_river_km,    # Group 2: Water quality
                           ~ depth_m,                              # Group 3: Physical
                           data = reef_env)

print(varpart_result)

# Plot Venn diagram
plot(varpart_result,
     Xnames = c("Habitat", "Water Quality", "Depth"),
     bg = c("red", "blue", "green"),
     alpha = 80,
     digits = 2,
     main = "Variance Partitioning - Reef Fish Community")

# Test significance of each fraction
# Unique contribution of habitat
anova(rda(reef_fish_hell ~ coral_cover + structural_complexity +
            Condition(sedimentation + distance_river_km + depth_m),
          data = reef_env), permutations = 999)

# Unique contribution of water quality
anova(rda(reef_fish_hell ~ sedimentation + distance_river_km +
            Condition(coral_cover + structural_complexity + depth_m),
          data = reef_env), permutations = 999)


# =========================================
# PART 7: RDA vs CCA - WHEN TO USE WHICH
# =========================================

cat("
=========================================
RDA vs CCA COMPARISON
=========================================

CHECK GRADIENT LENGTH (using DCA):
dca <- decorana(community_data)
# Look at axis lengths

If DCA1 < 3 SD:  Use RDA (linear)
If DCA1 > 4 SD:  Use CCA (unimodal)
If 3-4 SD:       Either may work

ADDITIONAL CONSIDERATIONS:

Use RDA when:
- Short environmental gradients
- Species increase/decrease linearly
- Hellinger-transformed data
- Environmental data analysis

Use CCA when:
- Long environmental gradients
- Species show optima (bell curves)
- Raw count data
- Classical community gradients

Use db-RDA when:
- You prefer Bray-Curtis distance
- Neither RDA nor CCA assumptions fit
- Maximum flexibility needed
=========================================
")

# Check gradient length for reef fish data
dca_check <- decorana(reef_fish)
print("DCA Axis Lengths for Reef Fish:")
print(dca_check)
cat("DCA1 length:", round(dca_check$rproj[1], 2), "SD\n")
# This helps decide between RDA and CCA


# =========================================
# PART 8: COMPLETE WORKFLOW EXAMPLE
# =========================================

cat("
=========================================
COMPLETE CONSTRAINED ORDINATION WORKFLOW
=========================================

1. EXPLORE DATA
   - Check gradient length (DCA)
   - Examine correlations among predictors
   - Consider transformations

2. SELECT METHOD
   - Short gradient → RDA
   - Long gradient → CCA
   - Need specific distance → db-RDA

3. BUILD MODEL
   - Include relevant predictors
   - Check VIF for collinearity
   - Consider partial ordination

4. TEST SIGNIFICANCE
   - Overall model (anova)
   - Individual axes (by = 'axis')
   - Individual terms (by = 'terms')

5. MODEL SELECTION
   - Forward/backward selection
   - Compare adjusted R²

6. VISUALIZE
   - Create triplot (sites + species + environment)
   - Add grouping factors
   - Report variance explained

7. INTERPRET
   - Which variables are important?
   - How do species relate to environment?
   - What is explained vs unexplained?
=========================================
")


# =========================================
# SUMMARY: DIRECT ORDINATION REFERENCE
# =========================================

cat("
=========================================
QUICK REFERENCE - DIRECT ORDINATION
=========================================

RDA (Redundancy Analysis)
- Function: rda(Y ~ X, data)
- Response: Linear
- Use: Short gradients, Hellinger-transformed data
- Test: anova.cca()

CCA (Canonical Correspondence Analysis)
- Function: cca(Y ~ X, data)
- Response: Unimodal
- Use: Long gradients, count data
- Test: anova.cca()

db-RDA / CAP
- Function: capscale(Y ~ X, data, distance = 'bray')
- Response: Flexible
- Use: Any distance measure
- Test: anova.cca()

PARTIAL ORDINATION
- Syntax: rda(Y ~ X | Z)
- Z = conditioning variables
- Removes effect of Z before analyzing X

VARIANCE PARTITIONING
- Function: varpart(Y, X1, X2, X3, data)
- Shows unique and shared contributions
- Use adjusted R² values

KEY OUTPUTS:
- Eigenvalues (importance of axes)
- Adjusted R² (variance explained)
- Variable loadings (biplot arrows)
- Species scores (environmental preferences)
- Significance tests (permutation-based)
=========================================
")

# ============================================
# End of Direct Ordination Tutorial
# ============================================

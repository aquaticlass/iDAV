# ============================================
# INDIRECT (UNCONSTRAINED) ORDINATION METHODS
# Comprehensive Guide for Marine Ecology
# ============================================

# Indirect ordination extracts major patterns of variation from
# species/community data WITHOUT using environmental variables
# as constraints. Environmental interpretation comes AFTER ordination.

# -----------------------------------------
# REQUIRED PACKAGES
# -----------------------------------------

# install.packages(c("vegan", "ggplot2", "ggrepel", "ade4", "factoextra"))

library(vegan)
library(ggplot2)
library(MASS)

# -----------------------------------------
# SAMPLE DATASETS
# -----------------------------------------

# Dataset 1: Demersal Fish Trawl Survey
# Fish abundance from 24 stations along a coastal gradient

set.seed(123)

demersal_fish <- data.frame(
  # Coastal/estuarine species (prefer shallow, low salinity)
  Platycephalus_fuscus = c(45, 52, 48, 55, 40, 35, 30, 25, 15, 12, 8, 5,
                            3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Acanthopagrus_australis = c(38, 42, 45, 40, 35, 32, 28, 22, 18, 15, 10, 8,
                               5, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  Mugil_cephalus = c(60, 65, 58, 62, 55, 48, 42, 35, 28, 22, 15, 10,
                      5, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0),

  # Transitional species (mid-range distribution)
  Sillago_ciliata = c(20, 25, 28, 30, 35, 40, 42, 45, 42, 38, 35, 30,
                       25, 20, 15, 12, 8, 5, 3, 2, 1, 0, 0, 0),
  Pelates_sexlineatus = c(15, 18, 22, 25, 30, 35, 38, 42, 45, 42, 38, 35,
                           30, 25, 20, 15, 10, 8, 5, 3, 2, 1, 0, 0),
  Gerres_subfasciatus = c(25, 28, 30, 32, 35, 38, 40, 42, 40, 38, 35, 32,
                           28, 25, 22, 18, 15, 12, 8, 5, 3, 2, 1, 0),

  # Offshore species (prefer deep, high salinity)
  Nemipterus_theodorei = c(0, 0, 0, 0, 1, 2, 3, 5, 8, 12, 18, 22,
                            28, 32, 38, 42, 45, 48, 50, 52, 55, 58, 60, 62),
  Saurida_undosquamis = c(0, 0, 0, 0, 0, 1, 2, 3, 5, 8, 12, 18,
                           25, 30, 35, 40, 45, 50, 55, 58, 62, 65, 68, 70),
  Lepidotrigla_argus = c(0, 0, 0, 0, 0, 0, 1, 2, 4, 8, 15, 20,
                          28, 35, 42, 48, 52, 55, 58, 60, 62, 65, 68, 72),

  # Ubiquitous species (found everywhere)
  Pseudorhombus_arsius = c(12, 15, 14, 16, 18, 20, 22, 25, 28, 30, 28, 25,
                            22, 20, 18, 16, 15, 14, 12, 10, 8, 6, 5, 4),
  Cynoglossus_bilineatus = c(8, 10, 12, 14, 16, 18, 20, 22, 24, 25, 24, 22,
                              20, 18, 16, 14, 12, 10, 8, 7, 6, 5, 4, 3)
)

rownames(demersal_fish) <- paste0("Stn_", sprintf("%02d", 1:24))

# Environmental data
fish_env <- data.frame(
  station = paste0("Stn_", sprintf("%02d", 1:24)),
  depth_m = c(5, 8, 6, 10, 12, 15, 18, 22, 28, 32, 38, 42,
              48, 55, 62, 68, 75, 82, 90, 98, 105, 115, 125, 140),
  salinity = c(28, 29, 28, 30, 31, 32, 33, 34, 34.5, 35, 35.2, 35.4,
               35.5, 35.6, 35.7, 35.8, 35.9, 36, 36, 36.1, 36.1, 36.2, 36.2, 36.3),
  temperature = c(24, 23.5, 24, 23, 22.5, 22, 21.5, 21, 20.5, 20, 19.5, 19,
                  18.5, 18, 17.5, 17, 16.5, 16, 15.5, 15, 14.5, 14, 13.5, 13),
  distance_coast_km = c(2, 3, 2.5, 4, 5, 7, 10, 14, 18, 22, 28, 32,
                        38, 45, 52, 60, 70, 80, 92, 105, 120, 135, 150, 170),
  substrate = factor(c(rep("Mud", 6), rep("Sandy_Mud", 6),
                       rep("Sand", 6), rep("Sandy_Gravel", 6))),
  zone = factor(c(rep("Coastal", 8), rep("Shelf", 8), rep("Outer_Shelf", 8)),
                levels = c("Coastal", "Shelf", "Outer_Shelf"))
)

# Dataset 2: Zooplankton community
zooplankton <- data.frame(
  Acartia_sp = c(850, 920, 780, 650, 420, 280, 150, 80, 40, 20),
  Oithona_sp = c(620, 680, 590, 520, 450, 380, 320, 280, 240, 200),
  Paracalanus_sp = c(180, 220, 280, 350, 420, 480, 520, 480, 420, 350),
  Temora_sp = c(45, 60, 85, 120, 180, 250, 320, 380, 420, 450),
  Centropages_sp = c(20, 35, 55, 80, 120, 180, 240, 320, 380, 420),
  Calanus_sp = c(5, 10, 20, 40, 80, 150, 280, 420, 550, 680),
  Euchaeta_sp = c(2, 5, 10, 25, 50, 100, 180, 280, 400, 520)
)
rownames(zooplankton) <- paste0("Sample_", 1:10)

zoo_env <- data.frame(
  sample = paste0("Sample_", 1:10),
  distance_offshore_km = seq(5, 50, by = 5),
  chlorophyll_a = c(8.5, 7.2, 5.8, 4.5, 3.2, 2.5, 1.8, 1.2, 0.8, 0.5),
  water_mass = factor(c(rep("Coastal", 3), rep("Mixed", 4), rep("Oceanic", 3)))
)

# =========================================
# PART 1: UNDERSTANDING ORDINATION
# =========================================

cat("
=========================================
WHAT IS ORDINATION?
=========================================

Ordination arranges samples along axes that represent the main
gradients of variation in your data. Think of it as finding
the 'best view' of your multidimensional data in 2D or 3D.

TWO MAIN CATEGORIES:

1. INDIRECT (Unconstrained) Ordination
   - Finds patterns in species data FIRST
   - Environmental interpretation comes AFTER
   - Methods: PCA, CA, DCA, NMDS, PCoA

2. DIRECT (Constrained) Ordination
   - Species patterns are CONSTRAINED by environmental variables
   - Tests species-environment relationships directly
   - Methods: RDA, CCA, db-RDA, CAP

CHOOSING A METHOD - Key Questions:
1. Linear or unimodal species responses?
2. Raw data or distance matrix?
3. Do you have environmental predictors?
=========================================
")

# =========================================
# PART 2: PCA - Principal Component Analysis
# =========================================

cat("
=========================================
PCA - PRINCIPAL COMPONENT ANALYSIS
=========================================

WHEN TO USE:
- Environmental data (continuous variables)
- Community data with SHORT gradients (< 2 SD)
- When species responses are approximately LINEAR

ASSUMPTIONS:
- Linear relationships between variables
- Data should be standardized if units differ

NOT SUITABLE FOR:
- Long ecological gradients (species turnover)
- Many zeros in the data (use CA/NMDS instead)
=========================================
")

# -----------------------------------------
# PCA on Environmental Data
# -----------------------------------------

# Select numeric environmental variables
env_numeric <- fish_env[, c("depth_m", "salinity", "temperature", "distance_coast_km")]

# Standardize the data (important when units differ)
env_scaled <- scale(env_numeric)

# Perform PCA
pca_env <- prcomp(env_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA results
summary(pca_env)

# Eigenvalues and variance explained
eigenvalues <- pca_env$sdev^2
var_explained <- eigenvalues / sum(eigenvalues) * 100
cumulative_var <- cumsum(var_explained)

pca_summary <- data.frame(
  PC = paste0("PC", 1:length(eigenvalues)),
  Eigenvalue = round(eigenvalues, 3),
  Variance_Pct = round(var_explained, 2),
  Cumulative_Pct = round(cumulative_var, 2)
)
print(pca_summary)

# Loadings (variable contributions)
print("PCA Loadings (Variable Contributions):")
print(round(pca_env$rotation, 3))

# -----------------------------------------
# PCA Visualization with ggplot2
# -----------------------------------------

# Extract scores
pca_scores <- as.data.frame(pca_env$x)
pca_scores$station <- fish_env$station
pca_scores$zone <- fish_env$zone
pca_scores$substrate <- fish_env$substrate

# Extract loadings for arrows
pca_loadings <- as.data.frame(pca_env$rotation)
pca_loadings$variable <- rownames(pca_loadings)

# Scale loadings for visualization
arrow_scale <- 3

# Create PCA biplot
pca_plot <- ggplot() +
  # Add points (samples)
  geom_point(data = pca_scores,
             aes(x = PC1, y = PC2, color = zone, shape = substrate),
             size = 3, alpha = 0.8) +
  # Add loading arrows
  geom_segment(data = pca_loadings,
               aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "darkred", linewidth = 0.8) +
  # Add variable labels
  geom_text(data = pca_loadings,
            aes(x = PC1 * arrow_scale * 1.1, y = PC2 * arrow_scale * 1.1, label = variable),
            color = "darkred", size = 3.5, fontface = "bold") +
  # Labels and theme
  labs(
    title = "PCA of Environmental Variables",
    subtitle = "Demersal Fish Trawl Survey Stations",
    x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
    y = paste0("PC2 (", round(var_explained[2], 1), "%)")
  ) +
  theme_minimal() +
  coord_fixed() +
  theme(legend.position = "right")

print(pca_plot)

# -----------------------------------------
# PCA on Community Data (Hellinger-transformed)
# -----------------------------------------

# For community data, use Hellinger transformation
fish_hell <- decostand(demersal_fish, method = "hellinger")

# PCA using vegan's rda() function (no constraints = PCA)
pca_fish <- rda(fish_hell)

# Summary
summary(pca_fish, display = NULL)

# Extract proportion explained
pca_fish_eig <- pca_fish$CA$eig
pca_fish_var <- pca_fish_eig / sum(pca_fish_eig) * 100

# Extract scores
site_scores_pca <- as.data.frame(scores(pca_fish, display = "sites", scaling = 1))
species_scores_pca <- as.data.frame(scores(pca_fish, display = "species", scaling = 1))

site_scores_pca$zone <- fish_env$zone
species_scores_pca$species <- rownames(species_scores_pca)

# PCA biplot for community data
pca_community_plot <- ggplot() +
  # Sites
  geom_point(data = site_scores_pca,
             aes(x = PC1, y = PC2, color = zone),
             size = 3) +
  # Species arrows
  geom_segment(data = species_scores_pca,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "gray40", alpha = 0.7) +
  # Species labels
  geom_text(data = species_scores_pca,
            aes(x = PC1 * 1.1, y = PC2 * 1.1, label = species),
            size = 2.5, color = "gray30") +
  labs(
    title = "PCA of Demersal Fish Community",
    subtitle = "Hellinger-transformed abundance data",
    x = paste0("PC1 (", round(pca_fish_var[1], 1), "%)"),
    y = paste0("PC2 (", round(pca_fish_var[2], 1), "%)")
  ) +
  theme_minimal() +
  coord_fixed()

print(pca_community_plot)


# =========================================
# PART 3: CA - Correspondence Analysis
# =========================================

cat("
=========================================
CA - CORRESPONDENCE ANALYSIS
=========================================

WHEN TO USE:
- Count/abundance data
- Species with UNIMODAL responses along gradients
- When gradient length is LONG (> 2 SD)

KEY CONCEPT:
- Based on chi-square distances
- Species optima and tolerances can be visualized
- Sites and species on same scale (symmetric scaling)

ADVANTAGE OVER PCA:
- Better for species turnover (beta diversity)
- Handles zeros appropriately

LIMITATION:
- 'Arch effect' on long gradients (use DCA instead)
=========================================
")

# Perform CA
ca_result <- cca(demersal_fish)

# Summary
summary(ca_result, display = NULL)

# Eigenvalues
ca_eig <- ca_result$CA$eig
ca_var <- ca_eig / sum(ca_eig) * 100

print("CA Eigenvalues and Variance:")
print(data.frame(
  Axis = paste0("CA", 1:4),
  Eigenvalue = round(ca_eig[1:4], 4),
  Variance_Pct = round(ca_var[1:4], 2)
))

# Extract scores
ca_sites <- as.data.frame(scores(ca_result, display = "sites", scaling = "symmetric"))
ca_species <- as.data.frame(scores(ca_result, display = "species", scaling = "symmetric"))

ca_sites$zone <- fish_env$zone
ca_sites$depth <- fish_env$depth_m
ca_species$species <- rownames(ca_species)

# CA biplot
ca_plot <- ggplot() +
  # Sites colored by depth
  geom_point(data = ca_sites,
             aes(x = CA1, y = CA2, color = depth),
             size = 3) +
  scale_color_viridis_c(option = "plasma") +
  # Species points
  geom_point(data = ca_species,
             aes(x = CA1, y = CA2),
             shape = 17, size = 2, color = "darkgreen") +
  geom_text(data = ca_species,
            aes(x = CA1, y = CA2, label = species),
            size = 2.5, vjust = -0.5, color = "darkgreen") +
  labs(
    title = "Correspondence Analysis - Demersal Fish",
    subtitle = "Sites colored by depth; triangles = species optima",
    x = paste0("CA1 (", round(ca_var[1], 1), "%)"),
    y = paste0("CA2 (", round(ca_var[2], 1), "%)"),
    color = "Depth (m)"
  ) +
  theme_minimal() +
  coord_fixed()

print(ca_plot)


# =========================================
# PART 4: DCA - Detrended Correspondence Analysis
# =========================================

cat("
=========================================
DCA - DETRENDED CORRESPONDENCE ANALYSIS
=========================================

WHEN TO USE:
- Long ecological gradients
- When CA shows 'arch effect'
- To estimate gradient length (in SD units)

KEY FEATURES:
- Removes arch artifact from CA
- Axis units = standard deviations of species turnover
- Gradient length helps choose ordination method:
  * < 2 SD: Use linear methods (PCA, RDA)
  * > 4 SD: Use unimodal methods (CA, CCA)
  * 2-4 SD: Either can work

LIMITATIONS:
- Detrending can distort relationships
- NMDS often preferred for hypothesis testing
=========================================
")

# Perform DCA
dca_result <- decorana(demersal_fish)

# Print results (includes axis lengths)
print(dca_result)

# Axis lengths in SD units
cat("\nGradient lengths (SD units):\n")
cat("DCA1:", round(dca_result$rproj[1], 2), "SD\n")
cat("DCA2:", round(dca_result$rproj[2], 2), "SD\n")

# Interpretation:
# If DCA1 > 4 SD: Long gradient, use unimodal methods
# If DCA1 < 2 SD: Short gradient, linear methods OK

# Extract DCA scores
dca_sites <- as.data.frame(scores(dca_result, display = "sites"))
dca_species <- as.data.frame(scores(dca_result, display = "species"))

dca_sites$zone <- fish_env$zone
dca_species$species <- rownames(dca_species)

# DCA plot
dca_plot <- ggplot() +
  geom_point(data = dca_sites,
             aes(x = DCA1, y = DCA2, color = zone),
             size = 3) +
  geom_text(data = dca_species,
            aes(x = DCA1, y = DCA2, label = species),
            size = 2.5, alpha = 0.7, color = "gray40") +
  labs(
    title = "Detrended Correspondence Analysis",
    subtitle = paste0("Gradient length: ", round(dca_result$rproj[1], 2), " SD"),
    x = "DCA1",
    y = "DCA2"
  ) +
  theme_minimal()

print(dca_plot)


# =========================================
# PART 5: NMDS - Non-metric Multidimensional Scaling
# =========================================

cat("
=========================================
NMDS - NON-METRIC MULTIDIMENSIONAL SCALING
=========================================

WHEN TO USE:
- Most community ecology applications
- When you want flexibility in distance measure
- Non-linear relationships
- Hypothesis testing (with PERMANOVA)

KEY FEATURES:
- Preserves RANK ORDER of distances (not exact distances)
- Works with any dissimilarity measure
- No assumptions about species response curves
- Stress value indicates goodness of fit

STRESS INTERPRETATION:
- < 0.05: Excellent representation
- 0.05-0.10: Good
- 0.10-0.20: Acceptable (use with caution)
- > 0.20: Poor (consider more dimensions)

BEST PRACTICES:
- Always report stress value
- Run multiple times (different starting points)
- Use appropriate transformation/distance
=========================================
")

# -----------------------------------------
# NMDS Analysis
# -----------------------------------------

# Run NMDS with Bray-Curtis dissimilarity
set.seed(123)
nmds_result <- metaMDS(demersal_fish,
                        distance = "bray",
                        k = 2,              # 2 dimensions
                        trymax = 100,       # Maximum random starts
                        autotransform = TRUE,
                        trace = FALSE)

# Check results
print(nmds_result)
cat("\nStress value:", round(nmds_result$stress, 4), "\n")

# Shepard plot - check fit
stressplot(nmds_result, main = "Shepard Plot - NMDS Fit")

# -----------------------------------------
# NMDS Visualization with ggplot2
# -----------------------------------------

# Extract scores
nmds_sites <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_species <- as.data.frame(scores(nmds_result, display = "species"))

nmds_sites$zone <- fish_env$zone
nmds_sites$depth <- fish_env$depth_m
nmds_sites$substrate <- fish_env$substrate
nmds_species$species <- rownames(nmds_species)

# Basic NMDS plot with zones
nmds_plot_basic <- ggplot(nmds_sites, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = zone, shape = substrate), size = 4, alpha = 0.8) +
  stat_ellipse(aes(color = zone), level = 0.95, linetype = 2) +
  labs(
    title = "NMDS Ordination - Demersal Fish Community",
    subtitle = paste("Stress =", round(nmds_result$stress, 3)),
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme_minimal() +
  coord_fixed()

print(nmds_plot_basic)

# NMDS with species overlay
nmds_plot_species <- ggplot() +
  # Convex hulls for zones
  stat_ellipse(data = nmds_sites,
               aes(x = NMDS1, y = NMDS2, fill = zone),
               geom = "polygon", alpha = 0.2, level = 0.95) +
  # Site points
  geom_point(data = nmds_sites,
             aes(x = NMDS1, y = NMDS2, color = zone),
             size = 3) +
  # Species points
  geom_point(data = nmds_species,
             aes(x = NMDS1, y = NMDS2),
             shape = 4, size = 2, color = "gray30") +
  geom_text(data = nmds_species,
            aes(x = NMDS1, y = NMDS2, label = species),
            size = 2.2, vjust = -0.7, color = "gray30", fontface = "italic") +
  labs(
    title = "NMDS with Species Scores",
    subtitle = paste("Stress =", round(nmds_result$stress, 3)),
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme_minimal() +
  coord_fixed() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

print(nmds_plot_species)

# NMDS colored by continuous variable (depth)
nmds_plot_depth <- ggplot(nmds_sites, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = depth, size = depth), alpha = 0.8) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    title = "NMDS - Community Change with Depth",
    subtitle = paste("Stress =", round(nmds_result$stress, 3)),
    x = "NMDS1",
    y = "NMDS2",
    color = "Depth (m)",
    size = "Depth (m)"
  ) +
  theme_minimal() +
  coord_fixed()

print(nmds_plot_depth)

# -----------------------------------------
# Fitting Environmental Vectors to NMDS
# -----------------------------------------

# Fit continuous environmental variables
env_fit <- envfit(nmds_result, fish_env[, c("depth_m", "salinity", "temperature",
                                             "distance_coast_km")],
                  permutations = 999)
print("Environmental Vector Fitting:")
print(env_fit)

# Extract vectors for plotting
env_vectors <- as.data.frame(scores(env_fit, display = "vectors"))
env_vectors$variable <- rownames(env_vectors)
env_vectors$pval <- env_fit$vectors$pvals
env_vectors$r2 <- env_fit$vectors$r

# Only plot significant vectors
env_vectors_sig <- env_vectors[env_vectors$pval < 0.05, ]

# NMDS with environmental vectors
arrow_mult <- 1.5  # Scaling factor for arrows

nmds_env_plot <- ggplot() +
  geom_point(data = nmds_sites,
             aes(x = NMDS1, y = NMDS2, color = zone),
             size = 3) +
  geom_segment(data = env_vectors_sig,
               aes(x = 0, y = 0, xend = NMDS1 * arrow_mult, yend = NMDS2 * arrow_mult),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "red", linewidth = 1) +
  geom_text(data = env_vectors_sig,
            aes(x = NMDS1 * arrow_mult * 1.1, y = NMDS2 * arrow_mult * 1.1,
                label = paste0(variable, "\n(R²=", round(r2, 2), ")")),
            color = "red", size = 3) +
  labs(
    title = "NMDS with Environmental Vectors",
    subtitle = "Arrows show significant correlations (p < 0.05)",
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme_minimal() +
  coord_fixed()

print(nmds_env_plot)

# Fit categorical variables
factor_fit <- envfit(nmds_result, fish_env[, c("substrate", "zone")],
                     permutations = 999)
print("Factor Fitting:")
print(factor_fit)


# =========================================
# PART 6: PCoA - Principal Coordinates Analysis
# =========================================

cat("
=========================================
PCoA - PRINCIPAL COORDINATES ANALYSIS
=========================================

ALSO KNOWN AS:
- Metric Multidimensional Scaling (MDS)
- Classical Scaling

WHEN TO USE:
- When you need eigenvalue-based ordination
- With any dissimilarity measure
- When you want explained variance for each axis

DIFFERENCE FROM NMDS:
- PCoA preserves actual distances (metric)
- NMDS preserves rank order (non-metric)
- PCoA provides eigenvalues/variance explained
- NMDS often better for community data

DIFFERENCE FROM PCA:
- PCA works on raw data (Euclidean)
- PCoA works on distance matrices
- PCoA can use any distance measure
=========================================
")

# Calculate Bray-Curtis dissimilarity
dist_bray <- vegdist(demersal_fish, method = "bray")

# Perform PCoA
pcoa_result <- cmdscale(dist_bray, k = 2, eig = TRUE)

# Variance explained
pcoa_eig <- pcoa_result$eig[pcoa_result$eig > 0]
pcoa_var <- pcoa_eig / sum(pcoa_eig) * 100

print("PCoA Variance Explained:")
print(data.frame(
  Axis = paste0("PCoA", 1:5),
  Eigenvalue = round(pcoa_eig[1:5], 4),
  Variance_Pct = round(pcoa_var[1:5], 2)
))

# Extract scores
pcoa_scores <- as.data.frame(pcoa_result$points)
colnames(pcoa_scores) <- c("PCoA1", "PCoA2")
pcoa_scores$zone <- fish_env$zone
pcoa_scores$depth <- fish_env$depth_m

# PCoA plot
pcoa_plot <- ggplot(pcoa_scores, aes(x = PCoA1, y = PCoA2)) +
  geom_point(aes(color = zone, size = depth), alpha = 0.8) +
  stat_ellipse(aes(color = zone), level = 0.95) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    title = "PCoA (Principal Coordinates Analysis)",
    subtitle = "Bray-Curtis dissimilarity",
    x = paste0("PCoA1 (", round(pcoa_var[1], 1), "%)"),
    y = paste0("PCoA2 (", round(pcoa_var[2], 1), "%)")
  ) +
  theme_minimal() +
  coord_fixed()

print(pcoa_plot)

# Using vegan's wcmdscale (weighted PCoA)
pcoa_vegan <- wcmdscale(dist_bray, k = 2, eig = TRUE)


# =========================================
# PART 7: COMPARING ORDINATION METHODS
# =========================================

cat("
=========================================
COMPARING ORDINATION METHODS
=========================================

METHOD COMPARISON:

| Method | Distance | Response | Variance | Best For |
|--------|----------|----------|----------|----------|
| PCA    | Euclidean| Linear   | Yes      | Env data, short gradients |
| CA     | Chi-sq   | Unimodal | Yes      | Count data, long gradients |
| DCA    | Chi-sq   | Unimodal | No       | Very long gradients |
| NMDS   | Any      | Any      | No       | Community data, hypothesis testing |
| PCoA   | Any      | Linear   | Yes      | Any data, need variance explained |

DECISION TREE:

1. Do you have environmental predictors?
   YES → Consider constrained methods (RDA, CCA)
   NO → Use unconstrained methods

2. What is your gradient length? (check DCA)
   Short (<2 SD) → PCA, RDA
   Long (>4 SD) → CA, CCA, NMDS
   Medium (2-4 SD) → Any method

3. Do you need explained variance?
   YES → PCA, CA, PCoA
   NO → NMDS (often best for communities)

4. What distance measure?
   Euclidean only → PCA
   Chi-square → CA, DCA
   Any (Bray-Curtis recommended) → NMDS, PCoA
=========================================
")

# -----------------------------------------
# Visual Comparison of Methods
# -----------------------------------------

# Create comparison data frame
comparison_df <- data.frame(
  Station = fish_env$station,
  Zone = fish_env$zone,
  Depth = fish_env$depth_m,

  # PCA scores
  PCA1 = site_scores_pca$PC1,
  PCA2 = site_scores_pca$PC2,

  # CA scores
  CA1 = ca_sites$CA1,
  CA2 = ca_sites$CA2,

  # DCA scores
  DCA1 = dca_sites$DCA1,
  DCA2 = dca_sites$DCA2,

  # NMDS scores
  NMDS1 = nmds_sites$NMDS1,
  NMDS2 = nmds_sites$NMDS2,

  # PCoA scores
  PCoA1 = pcoa_scores$PCoA1,
  PCoA2 = pcoa_scores$PCoA2
)

# Procrustes correlation between methods
procrustes_nmds_pcoa <- procrustes(nmds_result, pcoa_result$points)
print("Procrustes correlation NMDS vs PCoA:")
print(protest(nmds_result, pcoa_result$points, permutations = 999))

# Multi-panel comparison plot
library(gridExtra)

p1 <- ggplot(comparison_df, aes(x = PCA1, y = PCA2, color = Zone)) +
  geom_point(size = 2) + theme_minimal() + ggtitle("PCA") + coord_fixed()

p2 <- ggplot(comparison_df, aes(x = CA1, y = CA2, color = Zone)) +
  geom_point(size = 2) + theme_minimal() + ggtitle("CA") + coord_fixed()

p3 <- ggplot(comparison_df, aes(x = NMDS1, y = NMDS2, color = Zone)) +
  geom_point(size = 2) + theme_minimal() + ggtitle("NMDS") + coord_fixed()

p4 <- ggplot(comparison_df, aes(x = PCoA1, y = PCoA2, color = Zone)) +
  geom_point(size = 2) + theme_minimal() + ggtitle("PCoA") + coord_fixed()

# grid.arrange(p1, p2, p3, p4, ncol = 2)


# =========================================
# SUMMARY: INDIRECT ORDINATION REFERENCE
# =========================================

cat("
=========================================
QUICK REFERENCE - INDIRECT ORDINATION
=========================================

PCA (Principal Component Analysis)
- Function: prcomp() or rda() in vegan
- Distance: Euclidean (implicit)
- Use for: Environmental data, short gradients
- Output: Eigenvalues, loadings, scores

CA (Correspondence Analysis)
- Function: cca() in vegan (no constraints)
- Distance: Chi-square
- Use for: Count data, unimodal responses
- Watch for: Arch effect on long gradients

DCA (Detrended Correspondence Analysis)
- Function: decorana() in vegan
- Use for: Check gradient length, remove arch
- Key output: Axis lengths in SD units

NMDS (Non-metric MDS)
- Function: metaMDS() in vegan
- Distance: Any (Bray-Curtis common)
- Use for: Most community analyses
- Always report: Stress value

PCoA (Principal Coordinates Analysis)
- Function: cmdscale() or wcmdscale()
- Distance: Any
- Use for: When variance explained needed
=========================================
")

# ============================================
# End of Indirect Ordination Tutorial
# ============================================

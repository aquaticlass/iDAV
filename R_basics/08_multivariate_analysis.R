# ============================================
# Multivariate Analysis for Marine Ecology
# Community Structure and Environmental Relationships
# ============================================

# -----------------------------------------
# REQUIRED PACKAGES
# -----------------------------------------

# Install packages (run once)
# install.packages(c("vegan", "ggplot2", "ade4", "cluster", "factoextra"))

library(vegan)
library(ggplot2)
library(MASS)  # For isoMDS

# -----------------------------------------
# SAMPLE DATA: BENTHIC COMMUNITY SURVEYS
# -----------------------------------------

# Benthic invertebrate abundance from 20 coastal sites
# Rows = sites, Columns = taxa

set.seed(123)

benthic_community <- data.frame(
  # Polychaetes
  Nereis_sp = c(45, 52, 12, 8, 60, 55, 15, 10, 48, 58, 18, 5, 50, 62, 20, 8, 42, 55, 14, 6),
  Capitella_sp = c(80, 75, 25, 15, 85, 78, 30, 18, 72, 82, 28, 12, 76, 88, 32, 14, 70, 80, 26, 10),
  Glycera_sp = c(15, 18, 35, 42, 12, 16, 38, 45, 14, 20, 40, 48, 16, 22, 36, 50, 18, 15, 32, 44),

  # Molluscs
  Mytilus_sp = c(5, 8, 65, 72, 6, 10, 58, 68, 7, 12, 62, 75, 8, 15, 55, 70, 4, 9, 60, 78),
  Macoma_sp = c(25, 30, 40, 35, 28, 32, 38, 42, 22, 28, 45, 38, 26, 35, 42, 40, 20, 27, 36, 45),
  Nassarius_sp = c(10, 12, 28, 32, 8, 14, 25, 30, 11, 15, 30, 35, 9, 18, 26, 38, 12, 10, 22, 40),

  # Crustaceans
  Corophium_sp = c(35, 40, 18, 12, 38, 42, 20, 15, 32, 45, 22, 10, 36, 48, 16, 8, 30, 38, 24, 14),
  Gammarus_sp = c(20, 25, 30, 28, 22, 28, 32, 25, 18, 30, 35, 30, 24, 32, 28, 22, 16, 22, 34, 26),
  Carcinus_sp = c(3, 5, 12, 15, 4, 6, 10, 18, 2, 8, 14, 20, 5, 10, 8, 16, 4, 3, 11, 22),

  # Echinoderms
  Asterias_sp = c(2, 3, 18, 22, 1, 4, 15, 25, 3, 5, 20, 28, 2, 6, 12, 24, 1, 2, 16, 30),
  Ophiura_sp = c(8, 10, 25, 30, 6, 12, 22, 28, 9, 14, 28, 35, 7, 16, 20, 32, 5, 8, 24, 38)
)

rownames(benthic_community) <- paste0("Site_", sprintf("%02d", 1:20))

# Environmental data
env_data <- data.frame(
  site = paste0("Site_", sprintf("%02d", 1:20)),
  habitat = factor(c(rep("Mudflat", 4), rep("Sandy", 4), rep("Mudflat", 4),
                     rep("Sandy", 4), rep("Mudflat", 4))),
  pollution = factor(c(rep("High", 2), rep("Low", 2), rep("High", 2), rep("Low", 2),
                       rep("High", 2), rep("Low", 2), rep("High", 2), rep("Low", 2),
                       rep("High", 2), rep("Low", 2))),
  organic_matter = c(8.5, 7.8, 2.1, 1.8, 9.2, 8.0, 2.5, 2.0, 7.5, 8.8, 2.8, 1.5,
                     8.2, 9.0, 2.2, 1.6, 7.0, 8.5, 3.0, 1.2),
  grain_size = c(0.08, 0.10, 0.45, 0.52, 0.06, 0.12, 0.40, 0.55, 0.09, 0.11,
                 0.48, 0.60, 0.07, 0.14, 0.42, 0.58, 0.10, 0.08, 0.38, 0.65),
  depth_m = c(3, 4, 8, 12, 2, 5, 10, 15, 3, 6, 9, 14, 4, 7, 11, 16, 2, 5, 8, 18),
  salinity = c(28, 30, 34, 35, 26, 31, 33, 35, 29, 32, 34, 36, 27, 30, 32, 35, 25, 29, 33, 36)
)

# -----------------------------------------
# PART 1: DATA TRANSFORMATION
# -----------------------------------------

# Community data often needs transformation before analysis

# Square root transformation (moderate down-weighting of abundant species)
benthic_sqrt <- sqrt(benthic_community)

# Log transformation (strong down-weighting)
benthic_log <- log1p(benthic_community)  # log(x + 1) to handle zeros

# Fourth root transformation (common in marine ecology)
benthic_4rt <- benthic_community^0.25

# Presence-absence (binary)
benthic_pa <- decostand(benthic_community, method = "pa")

# Hellinger transformation (recommended for ordination)
benthic_hell <- decostand(benthic_community, method = "hellinger")

# Wisconsin double standardization
benthic_wisc <- wisconsin(benthic_community)

# For this tutorial, we'll use Hellinger transformation
print("Using Hellinger-transformed data for ordination")

# -----------------------------------------
# PART 2: DISTANCE/DISSIMILARITY MATRICES
# -----------------------------------------

# Calculate dissimilarity between sites

# Bray-Curtis dissimilarity (most common for abundance data)
dist_bray <- vegdist(benthic_community, method = "bray")

# Jaccard dissimilarity (presence-absence)
dist_jaccard <- vegdist(benthic_pa, method = "jaccard")

# Euclidean distance (for transformed data)
dist_eucl <- vegdist(benthic_hell, method = "euclidean")

# View dissimilarity matrix
print("Bray-Curtis Dissimilarity (first 5 sites):")
print(round(as.matrix(dist_bray)[1:5, 1:5], 3))

# -----------------------------------------
# PART 3: CLUSTER ANALYSIS
# -----------------------------------------

# Hierarchical clustering to identify site groups

# Using Bray-Curtis dissimilarity
hclust_bray <- hclust(dist_bray, method = "average")  # UPGMA

# Plot dendrogram
plot(hclust_bray,
     main = "Cluster Dendrogram - Benthic Communities",
     xlab = "Sites",
     ylab = "Bray-Curtis Dissimilarity",
     hang = -1)

# Add rectangles to show groups
rect.hclust(hclust_bray, k = 4, border = c("red", "blue", "green", "purple"))

# Cut dendrogram to get group assignments
cluster_groups <- cutree(hclust_bray, k = 4)
print("Cluster Assignments:")
print(cluster_groups)

# Compare with known groups
table(cluster_groups, env_data$habitat)
table(cluster_groups, env_data$pollution)

# Different linkage methods
par(mfrow = c(2, 2))
plot(hclust(dist_bray, method = "single"), main = "Single Linkage")
plot(hclust(dist_bray, method = "complete"), main = "Complete Linkage")
plot(hclust(dist_bray, method = "average"), main = "Average (UPGMA)")
plot(hclust(dist_bray, method = "ward.D2"), main = "Ward's Method")
par(mfrow = c(1, 1))

# -----------------------------------------
# PART 4: NMDS (Non-metric Multidimensional Scaling)
# -----------------------------------------

# NMDS is the most commonly used ordination for community data
# Preserves rank-order of distances between samples

# Run NMDS
nmds_result <- metaMDS(benthic_community,
                        distance = "bray",
                        k = 2,           # Number of dimensions
                        trymax = 100,    # Maximum iterations
                        autotransform = TRUE)

# Check stress value
print(paste("NMDS Stress:", round(nmds_result$stress, 3)))

# Stress interpretation:
# < 0.05 = Excellent
# 0.05-0.10 = Good
# 0.10-0.20 = Acceptable
# > 0.20 = Poor (use with caution)

# Shepard plot (goodness of fit)
stressplot(nmds_result, main = "Shepard Plot")

# Extract NMDS scores
nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_scores$site <- rownames(nmds_scores)
nmds_scores <- cbind(nmds_scores, env_data[, -1])

# Basic NMDS plot
plot(nmds_result, type = "t", main = "NMDS - Benthic Communities")

# ggplot2 visualization
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = habitat, shape = pollution), size = 4) +
  stat_ellipse(aes(color = habitat), level = 0.95) +
  labs(
    title = "NMDS Ordination of Benthic Communities",
    subtitle = paste("Stress =", round(nmds_result$stress, 3))
  ) +
  theme_minimal() +
  coord_fixed()

# Add species scores
species_scores <- as.data.frame(scores(nmds_result, display = "species"))
species_scores$species <- rownames(species_scores)

ggplot() +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2, color = habitat),
             size = 3, alpha = 0.7) +
  geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
            size = 2.5, color = "darkgray") +
  stat_ellipse(data = nmds_scores, aes(x = NMDS1, y = NMDS2, color = habitat)) +
  labs(title = "NMDS with Species Scores") +
  theme_minimal() +
  coord_fixed()

# -----------------------------------------
# PART 5: PCA (Principal Component Analysis)
# -----------------------------------------

# PCA is better for environmental data or transformed community data
# Use Hellinger-transformed data for community PCA

pca_result <- rda(benthic_hell)  # rda() with no constraints = PCA

# Summary of PCA
summary(pca_result)

# Eigenvalues and explained variance
eigenvalues <- pca_result$CA$eig
explained_var <- eigenvalues / sum(eigenvalues) * 100
cumulative_var <- cumsum(explained_var)

print("PCA Variance Explained:")
print(data.frame(
  PC = paste0("PC", 1:5),
  Eigenvalue = round(eigenvalues[1:5], 3),
  Variance_Pct = round(explained_var[1:5], 2),
  Cumulative_Pct = round(cumulative_var[1:5], 2)
))

# Scree plot
barplot(explained_var[1:10],
        names.arg = paste0("PC", 1:10),
        main = "Scree Plot",
        ylab = "Variance Explained (%)",
        col = "steelblue")

# PCA biplot
biplot(pca_result,
       display = c("sites", "species"),
       type = c("text", "points"),
       main = "PCA Biplot")

# Extract PCA scores for ggplot
pca_sites <- as.data.frame(scores(pca_result, display = "sites"))
pca_sites <- cbind(pca_sites, env_data)

pca_species <- as.data.frame(scores(pca_result, display = "species"))
pca_species$species <- rownames(pca_species)

ggplot() +
  geom_point(data = pca_sites, aes(x = PC1, y = PC2, color = habitat, shape = pollution),
             size = 3) +
  geom_segment(data = pca_species,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "gray50") +
  geom_text(data = pca_species, aes(x = PC1, y = PC2, label = species),
            size = 2.5, vjust = -0.5) +
  labs(
    title = "PCA of Benthic Communities",
    x = paste0("PC1 (", round(explained_var[1], 1), "%)"),
    y = paste0("PC2 (", round(explained_var[2], 1), "%)")
  ) +
  theme_minimal() +
  coord_fixed()

# -----------------------------------------
# PART 6: PERMANOVA (Permutational ANOVA)
# -----------------------------------------

# Test if community composition differs between groups
# Non-parametric multivariate ANOVA

# Test effect of habitat
permanova_habitat <- adonis2(benthic_community ~ habitat,
                              data = env_data,
                              method = "bray",
                              permutations = 999)
print("PERMANOVA - Effect of Habitat:")
print(permanova_habitat)

# Test effect of pollution
permanova_pollution <- adonis2(benthic_community ~ pollution,
                                data = env_data,
                                method = "bray",
                                permutations = 999)
print("PERMANOVA - Effect of Pollution:")
print(permanova_pollution)

# Two-factor PERMANOVA with interaction
permanova_full <- adonis2(benthic_community ~ habitat * pollution,
                           data = env_data,
                           method = "bray",
                           permutations = 999)
print("PERMANOVA - Habitat x Pollution:")
print(permanova_full)

# Interpretation:
# R2 = proportion of variance explained by factor
# F = pseudo-F statistic
# Pr(>F) = p-value from permutation test

# -----------------------------------------
# PART 7: ANOSIM (Analysis of Similarities)
# -----------------------------------------

# Alternative to PERMANOVA
# Tests if between-group dissimilarity > within-group dissimilarity

anosim_habitat <- anosim(benthic_community, env_data$habitat, distance = "bray")
print("ANOSIM - Habitat:")
print(anosim_habitat)

# R statistic interpretation:
# R close to 1 = very different between groups
# R close to 0 = no difference
# R < 0 = more similar between groups than within (rare)

plot(anosim_habitat, main = "ANOSIM - Habitat Groups")

# -----------------------------------------
# PART 8: SIMPER (Similarity Percentages)
# -----------------------------------------

# Identify which species contribute to differences between groups

simper_result <- simper(benthic_community, env_data$habitat)
print("SIMPER - Species contributing to differences between habitats:")
summary(simper_result)

# More detailed output
simper_mudflat_sandy <- simper_result$Mudflat_Sandy
print(head(simper_mudflat_sandy, 10))

# -----------------------------------------
# PART 9: CCA/RDA (Constrained Ordination)
# -----------------------------------------

# RDA (Redundancy Analysis) - Linear relationships
# CCA (Canonical Correspondence Analysis) - Unimodal relationships

# Prepare environmental matrix (numeric only)
env_matrix <- env_data[, c("organic_matter", "grain_size", "depth_m", "salinity")]

# RDA: Constrained ordination with environmental variables
rda_result <- rda(benthic_hell ~ organic_matter + grain_size + depth_m + salinity,
                   data = env_data)

# RDA summary
print("RDA Summary:")
summary(rda_result)

# Variance partitioning
print("Constrained variance explained:")
print(RssquareAdj(rda_result))

# Test significance of RDA
anova(rda_result, permutations = 999)

# Test significance of each axis
anova(rda_result, by = "axis", permutations = 999)

# Test significance of each variable
anova(rda_result, by = "terms", permutations = 999)

# RDA triplot
plot(rda_result, scaling = 2, main = "RDA Triplot")

# CCA (for comparison)
cca_result <- cca(benthic_community ~ organic_matter + grain_size + depth_m + salinity,
                   data = env_data)
plot(cca_result, main = "CCA Triplot")

# -----------------------------------------
# PART 10: VARIANCE PARTITIONING
# -----------------------------------------

# Partition variance among groups of environmental variables

# Group 1: Sediment characteristics
# Group 2: Physical factors

varpart_result <- varpart(benthic_hell,
                           ~ organic_matter + grain_size,  # Sediment
                           ~ depth_m + salinity,           # Physical
                           data = env_data)

print("Variance Partitioning:")
print(varpart_result)

# Visualize
plot(varpart_result,
     Xnames = c("Sediment", "Physical"),
     bg = c("red", "blue"),
     main = "Variance Partitioning")

# -----------------------------------------
# PART 11: ENVIRONMENTAL FITTING
# -----------------------------------------

# Fit environmental vectors to NMDS ordination

envfit_result <- envfit(nmds_result, env_matrix, permutations = 999)
print("Environmental Fitting to NMDS:")
print(envfit_result)

# Plot with environmental vectors
plot(nmds_result, type = "t", main = "NMDS with Environmental Vectors")
plot(envfit_result, p.max = 0.05, col = "red")  # Only significant vectors

# Fit factors (categorical variables)
envfit_factors <- envfit(nmds_result, env_data[, c("habitat", "pollution")],
                          permutations = 999)
print(envfit_factors)

# -----------------------------------------
# PART 12: INDICATOR SPECIES ANALYSIS
# -----------------------------------------

# Identify species indicative of certain groups
# Requires indicspecies package

# install.packages("indicspecies")
# library(indicspecies)

# indval_result <- multipatt(benthic_community, env_data$habitat,
#                            func = "IndVal.g", control = how(nperm = 999))
# summary(indval_result)

# Alternative using vegan
# Species most associated with each group
species_means_mudflat <- colMeans(benthic_community[env_data$habitat == "Mudflat", ])
species_means_sandy <- colMeans(benthic_community[env_data$habitat == "Sandy", ])

indicator_ratio <- species_means_mudflat / species_means_sandy
print("Species preference (Mudflat:Sandy ratio):")
print(round(sort(indicator_ratio, decreasing = TRUE), 2))

# -----------------------------------------
# SUMMARY: METHOD SELECTION GUIDE
# -----------------------------------------

cat("
=========================================
MULTIVARIATE ANALYSIS QUICK GUIDE
=========================================

DATA TRANSFORMATION:
- Hellinger: Best for ordination (PCA, RDA)
- Square root: Moderate down-weighting
- Log(x+1): Strong down-weighting
- Presence-absence: Ignore abundance

DISSIMILARITY MEASURES:
- Bray-Curtis: Standard for abundance data
- Jaccard: Presence-absence data
- Euclidean: Transformed data only

ORDINATION METHODS:
- NMDS: Best for community data, non-linear
- PCA: Linear, for transformed data/environment
- CA/DCA: Unimodal species responses

CONSTRAINED ORDINATION:
- RDA: Linear species-environment relationships
- CCA: Unimodal species-environment relationships

HYPOTHESIS TESTING:
- PERMANOVA: Test group differences (adonis2)
- ANOSIM: Alternative to PERMANOVA
- SIMPER: Species contributions to differences

VARIABLE SELECTION:
- envfit: Fit vectors to ordination
- varpart: Variance partitioning
=========================================
")

# ============================================
# End of Multivariate Analysis Tutorial
# ============================================

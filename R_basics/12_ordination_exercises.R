# ============================================
# Ordination Methods - Practice Exercises
# Marine Ecology and Fisheries Applications
# ============================================

library(vegan)
library(ggplot2)

# ============================================
# DATASETS FOR EXERCISES
# ============================================

# -----------------------------------------
# DATASET 1: Pelagic Fish Acoustics Survey
# -----------------------------------------
# Fish school biomass estimates from acoustic surveys
# across an upwelling gradient

set.seed(789)

pelagic_fish <- data.frame(
  Sardinella_longiceps = c(850, 920, 780, 620, 380, 220, 120, 60, 30, 15,
                            900, 980, 820, 580, 320, 180, 90, 45, 20, 10),
  Rastrelliger_kanagurta = c(420, 480, 520, 580, 620, 580, 480, 350, 220, 120,
                              450, 520, 560, 620, 650, 550, 420, 300, 180, 100),
  Decapterus_russelli = c(180, 220, 280, 380, 520, 620, 680, 620, 480, 320,
                           200, 250, 320, 420, 560, 650, 720, 580, 420, 280),
  Scomber_australasicus = c(80, 120, 180, 280, 420, 580, 720, 780, 720, 580,
                             100, 150, 220, 320, 480, 620, 780, 820, 680, 520),
  Auxis_thazard = c(20, 40, 80, 150, 280, 420, 580, 720, 820, 880,
                    30, 50, 100, 180, 320, 480, 620, 780, 850, 920),
  Katsuwonus_pelamis = c(5, 10, 25, 60, 120, 220, 380, 520, 680, 780,
                          8, 15, 35, 80, 150, 280, 420, 580, 720, 820)
)

rownames(pelagic_fish) <- paste0("Transect_", sprintf("%02d", 1:20))

pelagic_env <- data.frame(
  transect = paste0("Transect_", sprintf("%02d", 1:20)),
  distance_coast_km = c(5, 10, 20, 35, 55, 80, 110, 150, 200, 260,
                        8, 15, 28, 45, 70, 100, 140, 180, 240, 300),
  sst_celsius = c(29.5, 29.2, 28.8, 28.2, 27.5, 26.5, 25.2, 24.0, 22.5, 21.0,
                  29.3, 29.0, 28.5, 27.8, 27.0, 26.0, 24.8, 23.5, 22.0, 20.5),
  chlorophyll_a = c(3.5, 3.8, 4.2, 4.8, 5.5, 6.2, 6.8, 6.5, 5.8, 4.8,
                    3.8, 4.0, 4.5, 5.2, 5.8, 6.5, 7.0, 6.2, 5.5, 4.5),
  mixed_layer_depth = c(15, 18, 22, 28, 35, 45, 58, 72, 88, 105,
                        18, 22, 28, 35, 42, 55, 68, 82, 98, 115),
  upwelling_index = c(0.2, 0.3, 0.5, 0.8, 1.2, 1.8, 2.5, 3.2, 3.8, 4.2,
                      0.3, 0.4, 0.6, 0.9, 1.4, 2.0, 2.8, 3.5, 4.0, 4.5),
  season = factor(c(rep("Monsoon", 10), rep("Inter_monsoon", 10))),
  zone = factor(c(rep("Coastal", 3), rep("Neritic", 4), rep("Oceanic", 3),
                  rep("Coastal", 3), rep("Neritic", 4), rep("Oceanic", 3)),
                levels = c("Coastal", "Neritic", "Oceanic"))
)

# -----------------------------------------
# DATASET 2: Intertidal Rocky Shore Communities
# -----------------------------------------

intertidal <- data.frame(
  # Algae
  Ulva_lactuca = c(45, 52, 48, 55, 30, 22, 15, 8, 5, 2, 0, 0),
  Enteromorpha_sp = c(35, 40, 38, 42, 25, 18, 12, 6, 3, 1, 0, 0),
  Fucus_vesiculosus = c(20, 25, 28, 32, 38, 42, 45, 48, 42, 35, 25, 15),
  Laminaria_digitata = c(0, 0, 2, 5, 12, 20, 30, 42, 55, 65, 72, 78),

  # Molluscs
  Patella_vulgata = c(28, 32, 30, 35, 45, 52, 48, 42, 35, 28, 20, 15),
  Littorina_littorea = c(42, 48, 45, 50, 38, 30, 22, 15, 10, 6, 3, 1),
  Mytilus_edulis = c(15, 20, 25, 32, 45, 55, 62, 58, 50, 42, 32, 25),
  Nucella_lapillus = c(8, 12, 15, 20, 28, 35, 40, 38, 32, 25, 18, 12),

  # Crustaceans
  Semibalanus_balanoides = c(55, 62, 58, 65, 72, 68, 60, 52, 42, 32, 22, 15),
  Carcinus_maenas = c(5, 8, 10, 15, 22, 28, 32, 30, 25, 18, 12, 8)
)

rownames(intertidal) <- paste0("Quad_", sprintf("%02d", 1:12))

intertidal_env <- data.frame(
  quadrat = paste0("Quad_", sprintf("%02d", 1:12)),
  shore_height_m = c(3.5, 3.2, 3.3, 3.0, 2.5, 2.0, 1.5, 1.0, 0.5, 0.2, -0.2, -0.5),
  exposure_index = c(2, 3, 2, 4, 5, 6, 7, 8, 8, 9, 9, 10),
  rock_pool_pct = c(5, 8, 6, 10, 15, 20, 28, 35, 42, 50, 55, 60),
  wave_fetch_km = c(2, 5, 3, 8, 15, 25, 40, 60, 85, 120, 150, 180),
  substrate_roughness = c(2.5, 2.8, 2.6, 3.0, 3.5, 4.0, 4.5, 4.8, 4.5, 4.2, 3.8, 3.5),
  zone = factor(c(rep("Upper", 4), rep("Mid", 4), rep("Lower", 4)),
                levels = c("Upper", "Mid", "Lower"))
)

# -----------------------------------------
# DATASET 3: Seabird Colony Distribution
# -----------------------------------------

seabird_colonies <- data.frame(
  Puffinus_puffinus = c(250, 320, 280, 180, 120, 80, 45, 25, 12, 5),
  Fratercula_arctica = c(180, 220, 200, 150, 100, 65, 35, 18, 8, 2),
  Uria_aalge = c(120, 180, 220, 280, 320, 280, 200, 120, 60, 25),
  Alca_torda = c(80, 120, 150, 200, 250, 280, 240, 180, 100, 45),
  Rissa_tridactyla = c(50, 80, 120, 180, 250, 320, 350, 320, 250, 150),
  Morus_bassanus = c(15, 30, 55, 100, 180, 280, 380, 420, 380, 280),
  Fulmarus_glacialis = c(5, 12, 25, 55, 100, 180, 280, 350, 380, 350)
)

rownames(seabird_colonies) <- paste0("Island_", LETTERS[1:10])

seabird_env <- data.frame(
  island = paste0("Island_", LETTERS[1:10]),
  cliff_height_m = c(25, 40, 55, 80, 120, 180, 250, 320, 380, 420),
  distance_mainland_km = c(2, 5, 12, 25, 45, 75, 120, 180, 250, 320),
  island_area_ha = c(5, 12, 28, 55, 100, 180, 280, 420, 580, 750),
  fish_biomass_index = c(2.8, 3.2, 3.8, 4.5, 5.2, 5.8, 6.2, 6.5, 6.2, 5.8),
  predator_presence = factor(c("High", "High", "Med", "Med", "Low", "Low",
                                "None", "None", "None", "None"),
                              levels = c("High", "Med", "Low", "None"))
)


# ============================================
# EXERCISES
# ============================================

# -----------------------------------------
# EXERCISE 1: Choosing the Right Method
# -----------------------------------------
# Using the pelagic_fish dataset:
# a) Calculate gradient length using DCA
# b) Based on the gradient length, which ordination method is most appropriate?
# c) Justify your choice

# Your code here:



# -----------------------------------------
# EXERCISE 2: PCA on Environmental Data
# -----------------------------------------
# Using pelagic_env (numeric variables only):
# a) Perform PCA on standardized environmental data
# b) How much variance do the first two axes explain?
# c) Create a biplot showing sites colored by zone
# d) Which environmental variables are most correlated with PC1?

# Your code here:



# -----------------------------------------
# EXERCISE 3: NMDS Analysis
# -----------------------------------------
# Using the intertidal dataset:
# a) Perform NMDS with Bray-Curtis dissimilarity
# b) Check the stress value - is it acceptable?
# c) Create a plot with sites colored by shore zone
# d) Add 95% confidence ellipses for each zone
# e) Fit environmental vectors and identify significant ones

# Your code here:



# -----------------------------------------
# EXERCISE 4: Comparing PCoA and NMDS
# -----------------------------------------
# Using the seabird_colonies dataset:
# a) Perform both PCoA and NMDS (Bray-Curtis)
# b) Compare the ordinations using Procrustes analysis
# c) Which method would you report and why?

# Your code here:



# -----------------------------------------
# EXERCISE 5: RDA Analysis
# -----------------------------------------
# Using pelagic_fish and pelagic_env:
# a) Perform RDA with appropriate transformation
# b) Check VIF for collinearity
# c) Test overall significance and significance of each variable
# d) What is the adjusted R²?
# e) Create a triplot with ggplot2

# Your code here:



# -----------------------------------------
# EXERCISE 6: CCA Analysis
# -----------------------------------------
# Using intertidal and intertidal_env:
# a) Perform CCA
# b) Compare variance explained with unconstrained CA
# c) Which environmental variables are significant?
# d) Create a triplot showing species-environment relationships

# Your code here:



# -----------------------------------------
# EXERCISE 7: db-RDA (CAP)
# -----------------------------------------
# Using seabird_colonies and seabird_env:
# a) Perform db-RDA with Bray-Curtis distance
# b) Test significance of the model
# c) Compare results with standard RDA (Hellinger-transformed)
# d) Which approach is more appropriate for this data?

# Your code here:



# -----------------------------------------
# EXERCISE 8: Partial RDA
# -----------------------------------------
# Using pelagic_fish and pelagic_env:
# a) Run RDA controlling for 'season' effect
# b) Compare adjusted R² with full model (without conditioning)
# c) How much variation is explained by season alone?

# Your code here:



# -----------------------------------------
# EXERCISE 9: Variance Partitioning
# -----------------------------------------
# Using intertidal and intertidal_env:
# Partition variance between:
# - Physical factors: shore_height_m, exposure_index
# - Habitat factors: rock_pool_pct, substrate_roughness
#
# a) Perform variance partitioning
# b) Create and interpret the Venn diagram
# c) Test significance of unique contributions

# Your code here:



# -----------------------------------------
# EXERCISE 10: Complete Ordination Workflow
# -----------------------------------------
# Using pelagic_fish and pelagic_env, perform a complete analysis:
# a) Check gradient length (DCA)
# b) Choose appropriate method based on gradient
# c) Perform unconstrained ordination
# d) Perform constrained ordination
# e) Model selection (forward selection)
# f) Variance partitioning
# g) Final visualization
# h) Write ecological interpretation

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
# SOLUTION 1: Choosing the Right Method
# -----------------------------------------
dca_pelagic <- decorana(pelagic_fish)
print(dca_pelagic)

cat("\nDCA1 Axis Length:", round(dca_pelagic$rproj[1], 2), "SD\n")

# Interpretation:
# < 2 SD: Use linear methods (PCA, RDA)
# 2-4 SD: Either linear or unimodal
# > 4 SD: Use unimodal methods (CA, CCA)


# -----------------------------------------
# SOLUTION 2: PCA on Environmental Data
# -----------------------------------------
env_numeric <- pelagic_env[, c("distance_coast_km", "sst_celsius",
                                "chlorophyll_a", "mixed_layer_depth",
                                "upwelling_index")]

pca_env <- prcomp(env_numeric, center = TRUE, scale. = TRUE)
summary(pca_env)

# Variance explained
var_exp <- pca_env$sdev^2 / sum(pca_env$sdev^2) * 100
cat("PC1:", round(var_exp[1], 1), "%, PC2:", round(var_exp[2], 1), "%\n")

# Loadings
print(round(pca_env$rotation[, 1:2], 3))

# Biplot with ggplot2
pca_scores <- as.data.frame(pca_env$x)
pca_scores$zone <- pelagic_env$zone

pca_loadings <- as.data.frame(pca_env$rotation)
pca_loadings$variable <- rownames(pca_loadings)

ggplot() +
  geom_point(data = pca_scores, aes(x = PC1, y = PC2, color = zone), size = 3) +
  geom_segment(data = pca_loadings,
               aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text(data = pca_loadings,
            aes(x = PC1 * 3.3, y = PC2 * 3.3, label = variable),
            color = "red", size = 3) +
  labs(title = "PCA of Environmental Variables",
       x = paste0("PC1 (", round(var_exp[1], 1), "%)"),
       y = paste0("PC2 (", round(var_exp[2], 1), "%)")) +
  theme_minimal() +
  coord_fixed()


# -----------------------------------------
# SOLUTION 3: NMDS Analysis
# -----------------------------------------
nmds_inter <- metaMDS(intertidal, distance = "bray", k = 2, trymax = 100)
print(paste("Stress:", round(nmds_inter$stress, 3)))

# Stress < 0.1 is good, < 0.2 is acceptable

# Extract scores
nmds_scores <- as.data.frame(scores(nmds_inter, display = "sites"))
nmds_scores$zone <- intertidal_env$zone

# Plot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = zone)) +
  geom_point(size = 4) +
  stat_ellipse(level = 0.95, linetype = 2) +
  labs(title = paste("NMDS - Intertidal Community (Stress =",
                     round(nmds_inter$stress, 3), ")")) +
  theme_minimal() +
  coord_fixed()

# Environmental fitting
env_fit <- envfit(nmds_inter,
                  intertidal_env[, c("shore_height_m", "exposure_index",
                                     "rock_pool_pct", "wave_fetch_km")],
                  permutations = 999)
print(env_fit)


# -----------------------------------------
# SOLUTION 4: Comparing PCoA and NMDS
# -----------------------------------------
# PCoA
dist_seabird <- vegdist(seabird_colonies, "bray")
pcoa_seabird <- cmdscale(dist_seabird, k = 2, eig = TRUE)

# NMDS
nmds_seabird <- metaMDS(seabird_colonies, distance = "bray", k = 2)

# Procrustes comparison
procrust <- procrustes(nmds_seabird, pcoa_seabird$points)
protest_result <- protest(nmds_seabird, pcoa_seabird$points, permutations = 999)
print(protest_result)

# High correlation = both methods give similar results
# NMDS often preferred for community data due to flexibility


# -----------------------------------------
# SOLUTION 5: RDA Analysis
# -----------------------------------------
# Hellinger transformation
pelagic_hell <- decostand(pelagic_fish, "hellinger")

# RDA
rda_pelagic <- rda(pelagic_hell ~ sst_celsius + chlorophyll_a +
                     mixed_layer_depth + upwelling_index,
                   data = pelagic_env)

# VIF check
print("VIF:")
print(vif.cca(rda_pelagic))

# Significance tests
anova(rda_pelagic, permutations = 999)
anova(rda_pelagic, by = "terms", permutations = 999)

# Adjusted R²
r2 <- RsquareAdj(rda_pelagic)
cat("\nAdjusted R²:", round(r2$adj.r.squared, 3), "\n")

# Triplot
rda_sites <- as.data.frame(scores(rda_pelagic, display = "sites", scaling = 2))
rda_species <- as.data.frame(scores(rda_pelagic, display = "species", scaling = 2))
rda_bp <- as.data.frame(scores(rda_pelagic, display = "bp", scaling = 2))

rda_sites$zone <- pelagic_env$zone
rda_species$species <- rownames(rda_species)
rda_bp$variable <- rownames(rda_bp)

rda_eig <- rda_pelagic$CCA$eig
rda_var <- rda_eig / rda_pelagic$tot.chi * 100

ggplot() +
  geom_point(data = rda_sites, aes(x = RDA1, y = RDA2, color = zone), size = 3) +
  geom_text(data = rda_species, aes(x = RDA1, y = RDA2, label = species),
            size = 2.5, fontface = "italic", color = "darkgreen") +
  geom_segment(data = rda_bp, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text(data = rda_bp, aes(x = RDA1 * 1.1, y = RDA2 * 1.1, label = variable),
            color = "red", size = 3) +
  labs(title = "RDA Triplot - Pelagic Fish",
       subtitle = paste("Adj R² =", round(r2$adj.r.squared, 3)),
       x = paste0("RDA1 (", round(rda_var[1], 1), "%)"),
       y = paste0("RDA2 (", round(rda_var[2], 1), "%)")) +
  theme_minimal() +
  coord_fixed()


# -----------------------------------------
# SOLUTION 6: CCA Analysis
# -----------------------------------------
# CCA
cca_inter <- cca(intertidal ~ shore_height_m + exposure_index +
                   rock_pool_pct + wave_fetch_km,
                 data = intertidal_env)

print(cca_inter)

# Compare with unconstrained CA
ca_inter <- cca(intertidal)

cat("\nTotal inertia:", round(cca_inter$tot.chi, 3), "\n")
cat("Constrained:", round(cca_inter$CCA$tot.chi, 3), "\n")
cat("Proportion explained:", round(cca_inter$CCA$tot.chi / cca_inter$tot.chi, 3), "\n")

# Significance
anova(cca_inter, by = "terms", permutations = 999)

# Triplot
cca_sites <- as.data.frame(scores(cca_inter, display = "sites", scaling = 2))
cca_species <- as.data.frame(scores(cca_inter, display = "species", scaling = 2))
cca_bp <- as.data.frame(scores(cca_inter, display = "bp", scaling = 2))

cca_sites$zone <- intertidal_env$zone
cca_species$species <- rownames(cca_species)
cca_bp$variable <- rownames(cca_bp)

ggplot() +
  geom_point(data = cca_sites, aes(x = CCA1, y = CCA2, color = zone), size = 4) +
  geom_point(data = cca_species, aes(x = CCA1, y = CCA2),
             shape = 17, color = "darkgreen", size = 2) +
  geom_text(data = cca_species, aes(x = CCA1, y = CCA2, label = species),
            vjust = -0.8, size = 2.5, fontface = "italic") +
  geom_segment(data = cca_bp, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.25, "cm")), color = "red") +
  geom_text(data = cca_bp, aes(x = CCA1 * 1.15, y = CCA2 * 1.15, label = variable),
            color = "red", size = 3) +
  labs(title = "CCA Triplot - Intertidal Community") +
  theme_minimal() +
  coord_fixed()


# -----------------------------------------
# SOLUTION 7: db-RDA (CAP)
# -----------------------------------------
# db-RDA with Bray-Curtis
dbrda_bird <- capscale(seabird_colonies ~ cliff_height_m + distance_mainland_km +
                          island_area_ha + fish_biomass_index,
                        data = seabird_env, distance = "bray")

print(dbrda_bird)
anova(dbrda_bird, permutations = 999)

# Compare with RDA (Hellinger)
bird_hell <- decostand(seabird_colonies, "hellinger")
rda_bird <- rda(bird_hell ~ cliff_height_m + distance_mainland_km +
                  island_area_ha + fish_biomass_index,
                data = seabird_env)

cat("\ndb-RDA adj R²:", round(RsquareAdj(dbrda_bird)$adj.r.squared, 3), "\n")
cat("RDA adj R²:", round(RsquareAdj(rda_bird)$adj.r.squared, 3), "\n")

# db-RDA often better for community data (uses ecologically meaningful distance)


# -----------------------------------------
# SOLUTION 8: Partial RDA
# -----------------------------------------
# Full RDA
rda_full <- rda(pelagic_hell ~ sst_celsius + chlorophyll_a +
                  upwelling_index + season,
                data = pelagic_env)

# Partial RDA (controlling for season)
rda_partial <- rda(pelagic_hell ~ sst_celsius + chlorophyll_a +
                     upwelling_index | season,
                   data = pelagic_env)

# RDA with only season
rda_season <- rda(pelagic_hell ~ season, data = pelagic_env)

cat("Full model adj R²:", round(RsquareAdj(rda_full)$adj.r.squared, 3), "\n")
cat("Partial (without season) adj R²:", round(RsquareAdj(rda_partial)$adj.r.squared, 3), "\n")
cat("Season only adj R²:", round(RsquareAdj(rda_season)$adj.r.squared, 3), "\n")


# -----------------------------------------
# SOLUTION 9: Variance Partitioning
# -----------------------------------------
inter_hell <- decostand(intertidal, "hellinger")

varpart_inter <- varpart(inter_hell,
                          ~ shore_height_m + exposure_index,  # Physical
                          ~ rock_pool_pct + substrate_roughness,  # Habitat
                          data = intertidal_env)

print(varpart_inter)

plot(varpart_inter,
     Xnames = c("Physical", "Habitat"),
     bg = c("red", "blue"),
     main = "Variance Partitioning - Intertidal Community")

# Test unique contributions
# Physical unique
anova(rda(inter_hell ~ shore_height_m + exposure_index +
            Condition(rock_pool_pct + substrate_roughness),
          data = intertidal_env), permutations = 999)

# Habitat unique
anova(rda(inter_hell ~ rock_pool_pct + substrate_roughness +
            Condition(shore_height_m + exposure_index),
          data = intertidal_env), permutations = 999)


# -----------------------------------------
# SOLUTION 10: Complete Ordination Workflow
# -----------------------------------------

# a) Check gradient length
dca_result <- decorana(pelagic_fish)
cat("Gradient length DCA1:", round(dca_result$rproj[1], 2), "SD\n")
# Choose method based on length

# b) Transform data
pelagic_hell <- decostand(pelagic_fish, "hellinger")

# c) Unconstrained ordination (NMDS)
nmds_result <- metaMDS(pelagic_fish, distance = "bray", k = 2)
cat("NMDS Stress:", round(nmds_result$stress, 3), "\n")

# d) Constrained ordination (RDA)
rda_result <- rda(pelagic_hell ~ sst_celsius + chlorophyll_a +
                    mixed_layer_depth + upwelling_index,
                  data = pelagic_env)

# e) Forward selection
rda_null <- rda(pelagic_hell ~ 1, data = pelagic_env)
rda_forward <- ordistep(rda_null,
                         scope = formula(rda_result),
                         direction = "forward",
                         permutations = 999)
print(rda_forward)

# f) Variance partitioning
varpart_result <- varpart(pelagic_hell,
                           ~ sst_celsius,
                           ~ chlorophyll_a + upwelling_index,
                           data = pelagic_env)
print(varpart_result)

# g) Final visualization
final_sites <- as.data.frame(scores(rda_forward, display = "sites"))
final_sites$zone <- pelagic_env$zone
final_sites$season <- pelagic_env$season

final_bp <- as.data.frame(scores(rda_forward, display = "bp"))
final_bp$variable <- rownames(final_bp)

final_r2 <- RsquareAdj(rda_forward)

ggplot() +
  geom_point(data = final_sites,
             aes(x = RDA1, y = RDA2, color = zone, shape = season),
             size = 4) +
  stat_ellipse(data = final_sites,
               aes(x = RDA1, y = RDA2, color = zone), level = 0.95) +
  geom_segment(data = final_bp,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.25, "cm")), color = "red") +
  geom_text(data = final_bp,
            aes(x = RDA1 * 1.2, y = RDA2 * 1.2, label = variable),
            color = "red", size = 3.5) +
  labs(title = "Final RDA - Pelagic Fish Communities",
       subtitle = paste("Adj R² =", round(final_r2$adj.r.squared, 3))) +
  theme_minimal() +
  coord_fixed()

# h) Interpretation
cat("
ECOLOGICAL INTERPRETATION:
--------------------------
1. Community composition changes along a coastal-oceanic gradient
2. SST and upwelling index are the strongest drivers
3. Coastal species (Sardinella) prefer warm, low-upwelling waters
4. Oceanic species (Katsuwonus) prefer cool, high-upwelling waters
5. The model explains ~XX% of community variation
6. Remaining variation may be due to unmeasured factors
   (e.g., fishing pressure, prey availability)
")

# ============================================
# End of Ordination Exercises
# ============================================

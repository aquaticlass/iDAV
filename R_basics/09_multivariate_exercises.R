# ============================================
# Multivariate & Diversity Analysis Exercises
# Marine and Coastal Ecology Applications
# ============================================

library(vegan)
library(ggplot2)

# ============================================
# DATASETS FOR EXERCISES
# ============================================

# -----------------------------------------
# DATASET 1: Mangrove Fish Assemblages
# -----------------------------------------
# Fish abundance from mangrove sites with different disturbance levels

set.seed(42)

mangrove_fish <- data.frame(
  # Resident species
  Lutjanus_argentimaculatus = c(25, 28, 22, 30, 8, 5, 10, 6, 3, 2, 4, 3),
  Lates_calcarifer = c(12, 15, 10, 18, 4, 3, 5, 4, 1, 2, 2, 1),
  Scatophagus_argus = c(45, 50, 42, 55, 20, 15, 25, 18, 8, 5, 10, 6),

  # Transient species
  Mugil_cephalus = c(30, 35, 28, 40, 35, 38, 32, 40, 45, 50, 42, 48),
  Chanos_chanos = c(18, 22, 15, 25, 25, 28, 22, 30, 35, 40, 32, 38),

  # Opportunistic species
  Ambassis_gymnocephalus = c(60, 55, 65, 52, 80, 85, 75, 88, 120, 130, 115, 125),
  Gerres_oyena = c(40, 45, 38, 48, 55, 60, 50, 65, 90, 95, 85, 100),
  Leiognathus_equulus = c(35, 38, 32, 42, 65, 70, 60, 75, 110, 115, 105, 120)
)

rownames(mangrove_fish) <- paste0("Site_", sprintf("%02d", 1:12))

mangrove_env <- data.frame(
  site = paste0("Site_", sprintf("%02d", 1:12)),
  disturbance = factor(c(rep("Pristine", 4), rep("Moderate", 4), rep("Degraded", 4))),
  mangrove_cover = c(85, 90, 82, 88, 55, 50, 60, 52, 20, 15, 25, 18),
  water_turbidity = c(5, 4, 6, 5, 15, 18, 12, 16, 35, 40, 32, 38),
  prop_roots_density = c(45, 50, 42, 48, 28, 25, 32, 26, 10, 8, 12, 9),
  distance_ocean_km = c(2, 3, 1.5, 2.5, 4, 5, 3.5, 4.5, 6, 7, 5.5, 6.5)
)

# -----------------------------------------
# DATASET 2: Seagrass Epifauna
# -----------------------------------------
# Invertebrate abundance on seagrass at different depths

seagrass_fauna <- data.frame(
  # Gastropods
  Smaragdia_viridis = c(45, 52, 48, 55, 30, 35, 28, 38, 15, 18, 12, 20),
  Bittium_reticulatum = c(80, 75, 85, 70, 55, 50, 60, 48, 25, 22, 28, 20),

  # Bivalves
  Pinna_nobilis = c(5, 8, 6, 10, 12, 15, 10, 18, 20, 25, 18, 28),
  Modiolus_sp = c(15, 18, 12, 20, 22, 25, 20, 28, 35, 40, 32, 42),

  # Crustaceans
  Hippolyte_inermis = c(35, 40, 32, 45, 25, 28, 22, 30, 12, 15, 10, 18),
  Palaemon_elegans = c(28, 32, 25, 35, 20, 22, 18, 25, 8, 10, 6, 12),
  Idotea_baltica = c(50, 55, 48, 58, 35, 38, 32, 42, 18, 20, 15, 22),

  # Echinoderms
  Paracentrotus_lividus = c(8, 10, 6, 12, 15, 18, 12, 20, 25, 30, 22, 32),
  Holothuria_sp = c(3, 5, 2, 6, 8, 10, 6, 12, 15, 18, 12, 20)
)

rownames(seagrass_fauna) <- paste0("Station_", sprintf("%02d", 1:12))

seagrass_env <- data.frame(
  station = paste0("Station_", sprintf("%02d", 1:12)),
  depth_zone = factor(c(rep("Shallow", 4), rep("Mid", 4), rep("Deep", 4)),
                      levels = c("Shallow", "Mid", "Deep")),
  depth_m = c(2, 3, 2.5, 3.5, 6, 7, 5.5, 7.5, 12, 14, 11, 15),
  seagrass_biomass = c(450, 480, 420, 500, 320, 350, 300, 380, 180, 200, 160, 220),
  light_intensity = c(85, 90, 82, 88, 55, 60, 50, 62, 25, 30, 22, 32),
  sediment_organic = c(2.5, 2.2, 2.8, 2.0, 3.5, 3.2, 3.8, 3.0, 5.5, 5.0, 6.0, 4.8)
)

# -----------------------------------------
# DATASET 3: Coral Reef Fish (for diversity)
# -----------------------------------------

coral_fish <- data.frame(
  # High abundance species
  Chromis_viridis = c(150, 180, 45, 165, 50, 40, 160, 55),
  Pomacentrus_moluccensis = c(85, 95, 25, 90, 30, 22, 88, 28),

  # Medium abundance
  Chaetodon_lunulatus = c(25, 30, 8, 28, 10, 6, 27, 9),
  Thalassoma_lunare = c(40, 45, 15, 42, 18, 12, 44, 16),
  Halichoeres_hortulanus = c(30, 35, 12, 32, 14, 10, 33, 13),

  # Low abundance
  Cephalopholis_argus = c(8, 10, 3, 9, 4, 2, 10, 3),
  Epinephelus_merra = c(5, 7, 2, 6, 3, 1, 6, 2),

  # Rare species
  Cheilinus_undulatus = c(2, 3, 0, 2, 1, 0, 3, 1),
  Bolbometopon_muricatum = c(1, 2, 0, 1, 0, 0, 2, 0)
)

rownames(coral_fish) <- paste0("Reef_", LETTERS[1:8])

coral_env <- data.frame(
  reef = paste0("Reef_", LETTERS[1:8]),
  protection = factor(c("MPA", "MPA", "Fished", "MPA", "Fished", "Fished", "MPA", "Fished")),
  coral_cover = c(65, 72, 25, 68, 30, 18, 70, 28),
  structural_complexity = c(4.2, 4.5, 2.1, 4.3, 2.4, 1.8, 4.4, 2.2)
)


# ============================================
# EXERCISES
# ============================================

# -----------------------------------------
# EXERCISE 1: Diversity Indices
# -----------------------------------------
# Using the coral_fish dataset:
# a) Calculate species richness, Shannon, and Simpson diversity for each reef
# b) Calculate Pielou's evenness
# c) Compare diversity between MPA and Fished reefs (use t-test or Wilcoxon)
# d) Create a bar plot comparing Shannon diversity between protection levels

# Your code here:



# -----------------------------------------
# EXERCISE 2: Species Accumulation & Rarefaction
# -----------------------------------------
# Using the coral_fish dataset:
# a) Create a species accumulation curve
# b) Calculate rarefied richness (standardize to minimum sample size)
# c) Estimate total species richness using Chao estimator
# d) Interpret: Is the sampling adequate?

# Your code here:



# -----------------------------------------
# EXERCISE 3: Cluster Analysis
# -----------------------------------------
# Using the mangrove_fish dataset:
# a) Calculate Bray-Curtis dissimilarity
# b) Perform hierarchical clustering (UPGMA)
# c) Cut the dendrogram into 3 groups
# d) Do the clusters match the disturbance categories?

# Your code here:



# -----------------------------------------
# EXERCISE 4: NMDS Ordination
# -----------------------------------------
# Using the seagrass_fauna dataset:
# a) Perform NMDS ordination (2 dimensions)
# b) Check the stress value - is it acceptable?
# c) Create a plot showing sites colored by depth_zone
# d) Add 95% confidence ellipses for each depth zone

# Your code here:



# -----------------------------------------
# EXERCISE 5: PERMANOVA
# -----------------------------------------
# Using the mangrove_fish and mangrove_env datasets:
# a) Test if fish community differs between disturbance levels
# b) Report R² and p-value
# c) If significant, which disturbance level is most different?
# d) Run pairwise PERMANOVA (if possible)

# Your code here:



# -----------------------------------------
# EXERCISE 6: SIMPER Analysis
# -----------------------------------------
# Using the mangrove_fish dataset:
# a) Identify species contributing to differences between Pristine and Degraded sites
# b) Which species contribute most to the dissimilarity?
# c) Are these species more abundant in Pristine or Degraded sites?

# Your code here:



# -----------------------------------------
# EXERCISE 7: Environmental Fitting
# -----------------------------------------
# Using the seagrass_fauna and seagrass_env datasets:
# a) Run NMDS on the fauna data
# b) Fit environmental variables to the ordination
# c) Which environmental variables significantly correlate with community structure?
# d) Create a plot with significant environmental vectors

# Your code here:



# -----------------------------------------
# EXERCISE 8: RDA (Constrained Ordination)
# -----------------------------------------
# Using the seagrass_fauna and seagrass_env datasets:
# a) Perform RDA with depth_m, seagrass_biomass, and light_intensity as constraints
# b) What proportion of variance is explained by the environmental variables?
# c) Test the significance of the RDA
# d) Which environmental variable is most important?

# Your code here:



# -----------------------------------------
# EXERCISE 9: Beta Diversity
# -----------------------------------------
# Using the coral_fish dataset:
# a) Calculate pairwise Bray-Curtis dissimilarity
# b) Compare beta diversity within MPA vs within Fished sites
# c) Is community composition more variable in fished areas?

# Your code here:



# -----------------------------------------
# EXERCISE 10: Complete Analysis
# -----------------------------------------
# Using the mangrove_fish and mangrove_env datasets, perform a
# complete ecological analysis:
# a) Calculate diversity indices for each site
# b) Test if diversity differs between disturbance levels
# c) Perform NMDS ordination
# d) Test community differences with PERMANOVA
# e) Identify indicator species using SIMPER
# f) Write a brief ecological interpretation

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
# SOLUTION 1: Diversity Indices
# -----------------------------------------
# Calculate indices
richness <- specnumber(coral_fish)
shannon <- diversity(coral_fish, "shannon")
simpson <- diversity(coral_fish, "simpson")
evenness <- shannon / log(richness)

diversity_df <- data.frame(
  reef = rownames(coral_fish),
  richness = richness,
  shannon = round(shannon, 3),
  simpson = round(simpson, 3),
  evenness = round(evenness, 3),
  protection = coral_env$protection
)
print(diversity_df)

# Compare between protection levels
wilcox.test(shannon ~ protection, data = diversity_df)

# Bar plot
ggplot(diversity_df, aes(x = reef, y = shannon, fill = protection)) +
  geom_bar(stat = "identity") +
  labs(title = "Shannon Diversity by Reef and Protection Status",
       y = "Shannon Diversity (H')") +
  theme_minimal() +
  scale_fill_manual(values = c("MPA" = "darkgreen", "Fished" = "coral"))

# -----------------------------------------
# SOLUTION 2: Species Accumulation & Rarefaction
# -----------------------------------------
# Species accumulation
spec_accum <- specaccum(coral_fish, method = "random")
plot(spec_accum, ci.type = "polygon", col = "blue",
     main = "Species Accumulation Curve")

# Rarefied richness
min_n <- min(rowSums(coral_fish))
rarefied <- rarefy(coral_fish, sample = min_n)
print(paste("Minimum sample size:", min_n))
print("Rarefied richness:")
print(round(rarefied, 2))

# Chao estimator
chao <- specpool(coral_fish)
print(chao)
# If observed ≈ Chao estimate, sampling is adequate

# -----------------------------------------
# SOLUTION 3: Cluster Analysis
# -----------------------------------------
dist_bray <- vegdist(mangrove_fish, "bray")
hc <- hclust(dist_bray, method = "average")

plot(hc, main = "Cluster Dendrogram - Mangrove Fish",
     xlab = "Sites", hang = -1)
rect.hclust(hc, k = 3, border = c("red", "green", "blue"))

clusters <- cutree(hc, k = 3)
print("Cluster vs Disturbance:")
print(table(clusters, mangrove_env$disturbance))
# Should show good correspondence

# -----------------------------------------
# SOLUTION 4: NMDS Ordination
# -----------------------------------------
nmds <- metaMDS(seagrass_fauna, distance = "bray", k = 2, trymax = 100)
print(paste("Stress:", round(nmds$stress, 3)))

# Extract scores
scores_df <- as.data.frame(scores(nmds, "sites"))
scores_df$depth_zone <- seagrass_env$depth_zone

ggplot(scores_df, aes(x = NMDS1, y = NMDS2, color = depth_zone)) +
  geom_point(size = 4) +
  stat_ellipse(level = 0.95) +
  labs(title = paste("NMDS - Seagrass Epifauna (Stress =",
                     round(nmds$stress, 3), ")")) +
  theme_minimal() +
  coord_fixed()

# -----------------------------------------
# SOLUTION 5: PERMANOVA
# -----------------------------------------
permanova <- adonis2(mangrove_fish ~ disturbance,
                      data = mangrove_env,
                      method = "bray",
                      permutations = 999)
print(permanova)

# Pairwise comparisons (manual approach)
pristine_moderate <- adonis2(
  mangrove_fish[mangrove_env$disturbance %in% c("Pristine", "Moderate"), ] ~
    disturbance,
  data = mangrove_env[mangrove_env$disturbance %in% c("Pristine", "Moderate"), ],
  method = "bray")
print("Pristine vs Moderate:")
print(pristine_moderate)

# -----------------------------------------
# SOLUTION 6: SIMPER Analysis
# -----------------------------------------
simper_result <- simper(mangrove_fish, mangrove_env$disturbance)
summary(simper_result)

# Focus on Pristine vs Degraded
print("Top species contributing to Pristine vs Degraded differences:")
print(simper_result$Pristine_Degraded)

# -----------------------------------------
# SOLUTION 7: Environmental Fitting
# -----------------------------------------
nmds_fauna <- metaMDS(seagrass_fauna, distance = "bray", k = 2)

env_matrix <- seagrass_env[, c("depth_m", "seagrass_biomass",
                                "light_intensity", "sediment_organic")]
envfit_result <- envfit(nmds_fauna, env_matrix, permutations = 999)
print(envfit_result)

plot(nmds_fauna, type = "t", main = "NMDS with Environmental Vectors")
plot(envfit_result, p.max = 0.05, col = "red")

# -----------------------------------------
# SOLUTION 8: RDA
# -----------------------------------------
fauna_hell <- decostand(seagrass_fauna, "hellinger")

rda_result <- rda(fauna_hell ~ depth_m + seagrass_biomass + light_intensity,
                   data = seagrass_env)
print(summary(rda_result))
print("Adjusted R²:")
print(RsquareAdj(rda_result))

# Significance
anova(rda_result, permutations = 999)
anova(rda_result, by = "terms", permutations = 999)

plot(rda_result, main = "RDA Triplot")

# -----------------------------------------
# SOLUTION 9: Beta Diversity
# -----------------------------------------
dist_matrix <- as.matrix(vegdist(coral_fish, "bray"))

# Within MPA
mpa_sites <- which(coral_env$protection == "MPA")
mpa_dissim <- dist_matrix[mpa_sites, mpa_sites]
mpa_beta <- mean(mpa_dissim[lower.tri(mpa_dissim)])

# Within Fished
fished_sites <- which(coral_env$protection == "Fished")
fished_dissim <- dist_matrix[fished_sites, fished_sites]
fished_beta <- mean(fished_dissim[lower.tri(fished_dissim)])

print(paste("Mean beta diversity within MPA:", round(mpa_beta, 3)))
print(paste("Mean beta diversity within Fished:", round(fished_beta, 3)))

# -----------------------------------------
# SOLUTION 10: Complete Analysis
# -----------------------------------------

# a) Diversity indices
mangrove_diversity <- data.frame(
  site = rownames(mangrove_fish),
  disturbance = mangrove_env$disturbance,
  richness = specnumber(mangrove_fish),
  shannon = diversity(mangrove_fish, "shannon"),
  simpson = diversity(mangrove_fish, "simpson")
)

# b) Test diversity differences
kruskal.test(shannon ~ disturbance, data = mangrove_diversity)

ggplot(mangrove_diversity, aes(x = disturbance, y = shannon, fill = disturbance)) +
  geom_boxplot() +
  labs(title = "Shannon Diversity by Disturbance Level") +
  theme_minimal()

# c) NMDS
nmds_mangrove <- metaMDS(mangrove_fish, distance = "bray", k = 2)
nmds_scores <- as.data.frame(scores(nmds_mangrove, "sites"))
nmds_scores$disturbance <- mangrove_env$disturbance

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = disturbance)) +
  geom_point(size = 4) +
  stat_ellipse(level = 0.95) +
  labs(title = paste("NMDS - Mangrove Fish (Stress =",
                     round(nmds_mangrove$stress, 3), ")")) +
  theme_minimal()

# d) PERMANOVA
adonis2(mangrove_fish ~ disturbance, data = mangrove_env,
        method = "bray", permutations = 999)

# e) SIMPER
simper(mangrove_fish, mangrove_env$disturbance)

# f) Interpretation:
cat("
ECOLOGICAL INTERPRETATION:
--------------------------
1. Species diversity (Shannon) decreases with increasing disturbance
2. NMDS shows clear separation of sites by disturbance level
3. PERMANOVA confirms significant community differences (p < 0.05)
4. Pristine sites are dominated by resident species (Lutjanus, Lates, Scatophagus)
5. Degraded sites are dominated by opportunistic species (Ambassis, Gerres, Leiognathus)
6. Loss of mangrove cover leads to shift from specialist to generalist fish assemblages
")

# ============================================
# End of Multivariate Exercises
# ============================================

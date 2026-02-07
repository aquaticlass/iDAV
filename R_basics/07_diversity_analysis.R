# ============================================
# Biodiversity Analysis for Marine Ecology
# Species Diversity, Richness, and Evenness
# ============================================

# -----------------------------------------
# REQUIRED PACKAGES
# -----------------------------------------

# Install packages (run once)
# install.packages(c("vegan", "BiodiversityR", "ggplot2", "reshape2"))

library(vegan)
library(ggplot2)

# -----------------------------------------
# SAMPLE DATA: CORAL REEF FISH COMMUNITIES
# -----------------------------------------

# Fish abundance data from 12 reef sites
# Rows = sites, Columns = species

fish_community <- data.frame(
  # Damselfish species
  Chromis_viridis = c(45, 52, 38, 60, 25, 48, 55, 42, 30, 65, 40, 35),
  Dascyllus_aruanus = c(30, 28, 35, 25, 40, 32, 20, 38, 45, 22, 36, 42),
  Pomacentrus_moluccensis = c(25, 30, 20, 35, 15, 28, 32, 22, 18, 40, 24, 20),

  # Butterflyfish species
  Chaetodon_trifascialis = c(8, 12, 5, 15, 3, 10, 14, 6, 4, 18, 8, 5),
  Chaetodon_lunulatus = c(12, 15, 8, 18, 6, 14, 16, 10, 7, 20, 11, 8),

  # Wrasse species
  Thalassoma_lunare = c(20, 25, 15, 28, 10, 22, 26, 18, 12, 30, 19, 14),
  Halichoeres_hortulanus = c(15, 18, 12, 20, 8, 16, 19, 14, 10, 22, 15, 11),

  # Parrotfish species
  Scarus_rivulatus = c(10, 8, 12, 6, 15, 9, 7, 13, 16, 5, 11, 14),
  Chlorurus_sordidus = c(8, 6, 10, 5, 12, 7, 5, 11, 14, 4, 9, 12),

  # Grouper species
  Cephalopholis_argus = c(3, 5, 2, 6, 1, 4, 5, 2, 1, 7, 3, 2),
  Epinephelus_merra = c(2, 3, 1, 4, 1, 2, 3, 2, 1, 5, 2, 1),

  # Surgeonfish species
  Acanthurus_triostegus = c(18, 22, 14, 25, 10, 20, 24, 16, 12, 28, 17, 13),
  Zebrasoma_scopas = c(12, 15, 10, 18, 7, 14, 16, 11, 8, 20, 12, 9)
)

rownames(fish_community) <- paste0("Site_", sprintf("%02d", 1:12))

# Environmental data for each site
env_data <- data.frame(
  site = paste0("Site_", sprintf("%02d", 1:12)),
  reef_zone = factor(c("Crest", "Slope", "Flat", "Crest", "Flat", "Slope",
                       "Crest", "Flat", "Flat", "Slope", "Crest", "Flat")),
  coral_cover = c(55, 65, 40, 70, 30, 60, 68, 45, 35, 75, 52, 38),
  depth_m = c(5, 12, 3, 6, 2, 15, 8, 4, 2, 18, 7, 3),
  distance_shore_km = c(2.5, 4.0, 1.5, 3.0, 1.0, 5.0, 3.5, 2.0, 1.2, 6.0, 2.8, 1.8),
  water_temp = c(28.5, 27.2, 29.0, 28.0, 29.5, 26.8, 27.8, 28.8, 29.2, 26.5, 28.2, 29.0),
  visibility_m = c(15, 20, 10, 18, 8, 22, 19, 12, 9, 25, 16, 11)
)

# View the data
print("Fish Community Data (first 6 sites, first 6 species):")
print(fish_community[1:6, 1:6])

# -----------------------------------------
# PART 1: SPECIES RICHNESS
# -----------------------------------------

# Species richness = number of species present at each site

# Simple count of species (abundance > 0)
species_richness <- specnumber(fish_community)
print("Species Richness by Site:")
print(species_richness)

# All sites have all species in this example
# In real data, some species would be absent (0) at some sites

# Margalef's richness index (accounts for sample size)
# d = (S - 1) / ln(N)
# where S = number of species, N = total individuals

total_abundance <- rowSums(fish_community)
margalef <- (species_richness - 1) / log(total_abundance)
print("Margalef's Richness Index:")
print(round(margalef, 3))

# -----------------------------------------
# PART 2: SPECIES DIVERSITY INDICES
# -----------------------------------------

# Shannon-Wiener Diversity Index (H')
# Most commonly used diversity index
# Accounts for both richness and evenness
# Higher values = more diverse

shannon <- diversity(fish_community, index = "shannon")
print("Shannon Diversity (H'):")
print(round(shannon, 3))

# Typical interpretation:
# H' < 1.0 = Low diversity
# H' 1.0-2.0 = Medium diversity
# H' 2.0-3.0 = High diversity
# H' > 3.0 = Very high diversity

# Simpson's Diversity Index (1-D)
# Probability that two randomly selected individuals are different species
# Range: 0 to 1 (higher = more diverse)

simpson <- diversity(fish_community, index = "simpson")
print("Simpson's Diversity (1-D):")
print(round(simpson, 3))

# Inverse Simpson (more intuitive interpretation)
# Represents "effective number of species"
inv_simpson <- diversity(fish_community, index = "invsimpson")
print("Inverse Simpson:")
print(round(inv_simpson, 3))

# -----------------------------------------
# PART 3: EVENNESS (EQUITABILITY)
# -----------------------------------------

# Pielou's Evenness (J')
# How evenly individuals are distributed among species
# Range: 0 to 1 (1 = perfectly even distribution)

# J' = H' / ln(S)
pielou_evenness <- shannon / log(species_richness)
print("Pielou's Evenness (J'):")
print(round(pielou_evenness, 3))

# Interpretation:
# J' close to 0 = dominated by few species
# J' close to 1 = even distribution

# -----------------------------------------
# PART 4: COMPILE DIVERSITY METRICS
# -----------------------------------------

diversity_summary <- data.frame(
  Site = rownames(fish_community),
  Abundance = total_abundance,
  Richness = species_richness,
  Margalef = round(margalef, 3),
  Shannon = round(shannon, 3),
  Simpson = round(simpson, 3),
  Inv_Simpson = round(inv_simpson, 3),
  Evenness = round(pielou_evenness, 3)
)

print("Complete Diversity Summary:")
print(diversity_summary)

# Add environmental data
diversity_summary <- cbind(diversity_summary, env_data[, -1])

# -----------------------------------------
# PART 5: COMPARING DIVERSITY BETWEEN GROUPS
# -----------------------------------------

# Compare diversity between reef zones
# Group by reef zone and summarize

diversity_by_zone <- aggregate(
  cbind(Shannon, Simpson, Evenness, Richness) ~ reef_zone,
  data = diversity_summary,
  FUN = function(x) c(mean = mean(x), sd = sd(x))
)
print("Diversity by Reef Zone:")
print(diversity_by_zone)

# Statistical test: Is Shannon diversity different between zones?
# Use Kruskal-Wallis (non-parametric) or ANOVA

kruskal.test(Shannon ~ reef_zone, data = diversity_summary)

# Visualization
ggplot(diversity_summary, aes(x = reef_zone, y = Shannon, fill = reef_zone)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, size = 2) +
  labs(
    title = "Shannon Diversity by Reef Zone",
    x = "Reef Zone",
    y = "Shannon Diversity (H')"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

# -----------------------------------------
# PART 6: DIVERSITY-ENVIRONMENT RELATIONSHIPS
# -----------------------------------------

# Correlation between diversity and environmental variables
cor.test(diversity_summary$Shannon, diversity_summary$coral_cover)
cor.test(diversity_summary$Shannon, diversity_summary$depth_m)

# Scatter plot
ggplot(diversity_summary, aes(x = coral_cover, y = Shannon)) +
  geom_point(aes(color = reef_zone), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgray") +
  labs(
    title = "Shannon Diversity vs Coral Cover",
    x = "Coral Cover (%)",
    y = "Shannon Diversity (H')",
    color = "Reef Zone"
  ) +
  theme_minimal()

# Multiple regression: What predicts diversity?
diversity_model <- lm(Shannon ~ coral_cover + depth_m + visibility_m,
                       data = diversity_summary)
summary(diversity_model)

# -----------------------------------------
# PART 7: SPECIES ACCUMULATION CURVES
# -----------------------------------------

# How many species do we expect to find with more sampling?
# Useful for assessing sampling completeness

# Species accumulation curve
spec_accum <- specaccum(fish_community, method = "random", permutations = 100)
plot(spec_accum,
     ci.type = "polygon",
     ci.col = "lightblue",
     col = "blue",
     lwd = 2,
     xlab = "Number of Sites Sampled",
     ylab = "Cumulative Species Richness",
     main = "Species Accumulation Curve")

# Estimate total species richness (extrapolation)
# Chao estimator
chao_estimate <- specpool(fish_community)
print("Species Richness Estimates:")
print(chao_estimate)

# -----------------------------------------
# PART 8: RAREFACTION
# -----------------------------------------

# Compare diversity when sample sizes differ
# Rarefaction standardizes to the same number of individuals

# Minimum sample size
min_n <- min(rowSums(fish_community))
print(paste("Minimum sample size:", min_n))

# Rarefied richness
rarefied_richness <- rarefy(fish_community, sample = min_n)
print("Rarefied Species Richness:")
print(round(rarefied_richness, 2))

# Rarefaction curves for each site
rarecurve(fish_community,
          step = 10,
          col = rainbow(12),
          xlab = "Number of Individuals",
          ylab = "Species Richness",
          main = "Rarefaction Curves by Site")
legend("bottomright", legend = rownames(fish_community),
       col = rainbow(12), lty = 1, cex = 0.6)

# -----------------------------------------
# PART 9: RANK-ABUNDANCE CURVES
# -----------------------------------------

# Visualize species dominance patterns

# For one site
site1_abundance <- as.numeric(fish_community[1, ])
names(site1_abundance) <- colnames(fish_community)
site1_sorted <- sort(site1_abundance, decreasing = TRUE)

# Create rank-abundance data
rank_abund <- data.frame(
  Rank = 1:length(site1_sorted),
  Species = names(site1_sorted),
  Abundance = site1_sorted
)

ggplot(rank_abund, aes(x = Rank, y = Abundance)) +
  geom_point(size = 3, color = "steelblue") +
  geom_line(color = "steelblue") +
  geom_text(aes(label = Species), hjust = -0.1, angle = 45, size = 2.5) +
  labs(
    title = "Rank-Abundance Curve (Site 01)",
    x = "Species Rank",
    y = "Abundance"
  ) +
  theme_minimal() +
  expand_limits(x = c(0, 16))

# -----------------------------------------
# PART 10: BETA DIVERSITY
# -----------------------------------------

# Beta diversity = variation in species composition between sites

# Whittaker's beta diversity
# β = (γ/α) - 1
# where γ = total richness, α = mean site richness

gamma_richness <- ncol(fish_community)  # Total species in all sites
alpha_mean <- mean(specnumber(fish_community))
whittaker_beta <- (gamma_richness / alpha_mean) - 1
print(paste("Whittaker's Beta Diversity:", round(whittaker_beta, 3)))

# Pairwise beta diversity (Sorensen dissimilarity)
beta_matrix <- vegdist(fish_community, method = "bray", binary = FALSE)
print("Bray-Curtis Dissimilarity Matrix (first 6 sites):")
print(round(as.matrix(beta_matrix)[1:6, 1:6], 3))

# Beta diversity partitioning (turnover vs nestedness)
# Requires betapart package
# install.packages("betapart")
# library(betapart)
# beta_partition <- beta.pair(fish_community, index.family = "sorensen")

# -----------------------------------------
# SUMMARY: DIVERSITY INDEX REFERENCE
# -----------------------------------------

cat("
=========================================
DIVERSITY INDEX QUICK REFERENCE
=========================================

RICHNESS MEASURES:
- Species Richness (S): Simple count of species
- Margalef Index: Adjusts richness for sample size
- Rarefied Richness: Standardized comparison

DIVERSITY INDICES:
- Shannon (H'): Most common, sensitive to rare species
  Low: <1.0, Medium: 1-2, High: 2-3, Very High: >3

- Simpson (1-D): Probability of interspecific encounter
  Range: 0-1, Higher = more diverse

- Inverse Simpson: Effective number of species

EVENNESS:
- Pielou's J': Distribution of individuals among species
  Range: 0-1, Higher = more even

BETA DIVERSITY:
- Whittaker's β: Turnover between sites
- Bray-Curtis: Quantitative dissimilarity
- Sorensen/Jaccard: Presence-absence dissimilarity
=========================================
")

# ============================================
# End of Diversity Analysis Tutorial
# ============================================

library(psych)
library(dplyr)
library(tidyr)
library(ggplot2)

library(rjags)
library(coda)
library(lattice)
load.module("glm")

#### Functions ####

# Calculate euclidean distance:
calc_dist <- function(point1, point2) {
  sqrt(sum((point1 - point2) ^ 2))
}

# Calculate interactions' and species' originalities
alpha_originality <- function(unique_plot, scores){
  
  alpha <- unique_plot %>%
    left_join(distinct(scores, !!sym(names(scores)[1]), !!sym(names(scores)[2]), .keep_all = TRUE), 
              by = c(names(scores)[1], names(scores)[2]), relationship = "many-to-many")
  
  centroids <- scores %>%
    group_by(Treatment3) %>%
    summarise(RC1 = mean(RC1), RC2 = mean(RC2))  
  
  alpha$AlphaOrig <- mapply(function(x, y, trat) {
    center <- centroids[centroids$Treatment3 == trat, c("RC1", "RC2")]
    calc_dist(c(x, y), center)
  }, alpha$RC1, alpha$RC2, alpha$Treatment3)
  
  return(alpha)
}

#### Data ####

# Interactions:
int <- read.csv(file = "interactions.csv")
int$Plot_ID <- as.factor(int$Plot_ID)

# Traits:

Plants <- read.csv(file = "plants.csv")
Plants$Plot_ID <- as.factor(Plants$Plot_ID)

Animals <- read.csv(file = "animals.csv")
Animals$Plot_ID <- as.factor(Animals$Plot_ID)


## Animals traits:
body_mass <- Animals %>%
  filter(trait == "BodyMass") %>%
  group_by(species) %>%
  mutate(BodyMass = mean_value_numeric,
         LogBodyMass = log(BodyMass)) %>%
  dplyr::select(animal_species = species, BodyMass, LogBodyMass) %>%
  distinct()


matching_animals <- Animals %>%
  filter((taxon == "Birds" & trait == "Beak.Width") | 
           ((taxon == "Rodentia" | taxon == "Primates" | taxon == "Didelphimorphia"
             | taxon == "Carnivora" | taxon == "Chiroptera" | taxon == "Artiodactyla") & trait == "mandibleWidth")) %>%
  dplyr::select(animal_species = species, Width = mean_value_numeric) %>%
  mutate(LogWidth = log(Width)) %>%
  distinct()

hand_wing_index <- Animals %>%
  filter(taxon == "Birds", trait == "Hand.wing.Index") %>%
  dplyr::select(animal_species = species, HandWingIndex = mean_value_numeric) %>%
  distinct()

animals_traits <- body_mass %>%
  left_join(matching_animals, by = "animal_species") %>%
  distinct(animal_species, .keep_all = TRUE) %>%
  left_join(hand_wing_index, by = "animal_species") %>%
  distinct(animal_species, .keep_all = TRUE)

# remove size effects:

m2 <- lm(LogWidth ~ LogBodyMass, data = animals_traits)
cc2 <- animals_traits[complete.cases(animals_traits[, c("LogWidth", "LogBodyMass")]),]
r2 <- data.frame(animal_species = cc2$animal_species, resWidth = resid(m2))
animals_traits <- animals_traits %>%
  left_join(r2, by = "animal_species") %>%
  distinct()

## Plants traits

fruit_width <- Plants %>%
  filter(trait == "FruitWidth") %>%
  group_by(species) %>%
  summarize(FruitWidth = mean(value_numeric, na.rm = TRUE)) %>%
  dplyr::select(plant_species = species, FruitWidth) %>%
  mutate(LogFruitWidth = log(FruitWidth)) %>%
  distinct()

height <- Plants %>%
  filter(trait == "height") %>%
  group_by(species) %>%
  summarize(Height = mean(value_numeric, na.rm = TRUE)) %>%
  dplyr::select (plant_species = species, Height) %>%
  mutate(LogHeight = log(Height)) %>%
  distinct()

# Crop mass:
fruit_weight <- Plants %>%
  filter(trait == "FruitWeight") %>%
  group_by(species) %>%
  summarize(MeanFruitWeight = mean(value_numeric, na.rm = TRUE), .groups = 'drop') %>%
  dplyr::select(plant_species = species, MeanFruitWeight) %>%
  mutate(LogFruitWeight = log(MeanFruitWeight)) %>%
  distinct()

plants_traits <- Plants %>%
  filter(trait == "fruit_abundance") %>%
  mutate(fruit_abundance = value_numeric) %>%
  left_join(fruit_weight, by = c("species"="plant_species")) 

plants_traits <- plants_traits %>%
  mutate(CropMass = fruit_abundance * MeanFruitWeight)

crop_mass <- plants_traits %>%
  group_by(species) %>%
  summarize(CropMass = mean(CropMass, na.rm = TRUE), .groups = 'drop') %>%
  dplyr::select(plant_species = species, CropMass)  %>%
  mutate(LogCropMass = log(CropMass))

plants_traits <- crop_mass %>%
  left_join(fruit_width, by = "plant_species") %>%
  left_join(height, by = "plant_species")

# Remove size effects: 

m4 <- lm(LogFruitWidth ~ LogCropMass, data = plants_traits)
cc4 <- plants_traits[complete.cases(plants_traits[, c("LogFruitWidth", "LogCropMass")]),]
r4 <- data.frame(plant_species = cc4$plant_species, resFruitWidth = resid(m4))
plants_traits <- plants_traits %>%
  left_join(r4, by = "plant_species")

interactions <- int %>%
  mutate(interaction = paste(plant_species, animal_species, sep = ".")) %>%
  dplyr::select(Plot_ID, taxon, animal_species, plant_species,
                interaction, type, unit, RegTime, Treatment3) %>%
  mutate(Treatment3 = if_else(Treatment3 == "active", "regeneration early", Treatment3))


interactions <- interactions %>%
  left_join(animals_traits, by = "animal_species") %>%
  left_join(plants_traits, by = "plant_species") %>%
  filter(!animal_species %in% c("", "Metachirus_sp", "Trasandinomys_sp", "Trogon_sp."))

all_plants <- Plants %>%
  mutate(plant_species = species) %>%
  dplyr::select(Plot_ID,  plant_species, RegTime, Treatment3) %>%
  mutate(Treatment3 = if_else(Treatment3 == "active", "regeneration early", Treatment3)) %>%
  left_join(plants_traits, by = "plant_species")

plant_species_diff <- setdiff(unique(all_plants$plant_species), unique(interactions$plant_species))

int_plants <- all_plants %>%
  filter(!plant_species %in% plant_species_diff)

#### Data frames ####

# Per treatment (for trait spaces):

unique_interactions_t3 <- interactions %>%
  group_by(interaction, Treatment3) %>%
  filter(!plant_species %in% c("Theobroma_cacao", "Manihot_esculenta", "Artocarpus_heterophyllus", "Psidium_guajava",
                               "Borojoa_sp.", "Persea_americana")) %>%
  summarise(Treatment3 = first(Treatment3),
            taxon = first(taxon), animal_species = first(animal_species),
            plant_species = first(plant_species), interaction = first(interaction), 
            LogBodyMass = first(LogBodyMass), LogCropMass = first(LogCropMass), 
            resWidth = first(resWidth), resFruitWidth = first(resFruitWidth), 
            HandWingIndex = first(HandWingIndex), LogHeight = first(LogHeight),
            .groups = 'drop') 
unique_interactions_t3 <- unique_interactions_t3 %>%
  mutate(
    StdBodyMass = as.numeric(scale(LogBodyMass)),
    StdCropMass = as.numeric(scale(LogCropMass)),
    StdWidth = as.numeric(scale(resWidth)),
    StdFruitWidth = as.numeric(scale(resFruitWidth)),
    StdHandWingIndex = as.numeric(scale(HandWingIndex)),
    StdHeight = as.numeric(scale(LogHeight))
  )

unique_plants_t3 <- int_plants %>%
  group_by(plant_species, Treatment3) %>%
  filter(!plant_species %in% c("Theobroma_cacao", "Manihot_esculenta", "Artocarpus_heterophyllus", "Psidium_guajava",
                               "Borojoa_sp.", "Persea_americana")) %>%
  summarise(Treatment3 = first(Treatment3),
            plant_species = first(plant_species),
            LogCropMass = first(LogCropMass), resFruitWidth = first(resFruitWidth),
            LogHeight = first(LogHeight),
            .groups = "drop") 
unique_plants_t3 <- unique_plants_t3 %>%
  mutate(
    StdCropMass = as.numeric(scale(LogCropMass)),
    StdFruitWidth = as.numeric(scale(resFruitWidth)),
    StdHeight = as.numeric(scale(LogHeight))
  )

unique_animals_t3 <- interactions %>%
  group_by(animal_species, Treatment3) %>%
  filter(!plant_species %in% c("Theobroma_cacao", "Manihot_esculenta", "Artocarpus_heterophyllus", "Psidium_guajava",
                               "Borojoa_sp.", "Persea_americana")) %>%
  summarise(Treatment3 = first(Treatment3),
            animal_species = first(animal_species),
            LogBodyMass = first(LogBodyMass), resWidth = first(resWidth),
            HandWingIndex = first(HandWingIndex), 
            .groups = "drop")
unique_animals_t3 <- unique_animals_t3 %>%
  mutate(
    StdBodyMass = as.numeric(scale(LogBodyMass)),
    StdWidth = as.numeric(scale(resWidth)),
    StdHandWingIndex = as.numeric(scale(HandWingIndex)))

# Per plot (for all other analyses):

unique_interactions_plot <- interactions %>%
  group_by(interaction, Plot_ID) %>%
  filter(!plant_species %in% c("Theobroma_cacao", "Manihot_esculenta", "Artocarpus_heterophyllus", "Psidium_guajava",
                               "Borojoa_sp.", "Persea_americana")) %>%
  summarise(Treatment3 = first(Treatment3),
            Plot_ID = first(Plot_ID), interaction = first(interaction), 
            RegTime = first(RegTime),
            .groups = 'drop') 

unique_plants_plot <- int_plants %>%
  group_by(plant_species, Plot_ID) %>%
  filter(!plant_species %in% c("Theobroma_cacao", "Manihot_esculenta", "Artocarpus_heterophyllus", "Psidium_guajava",
                               "Borojoa_sp.", "Persea_americana")) %>%
  summarise(Treatment3 = first(Treatment3),
            Plot_ID = first(Plot_ID), plant_species = first(plant_species),
            RegTime = first(RegTime), 
            .groups = "drop") 

unique_animals_plot <- interactions %>%
  group_by(animal_species, Plot_ID) %>%
  filter(!plant_species %in% c("Theobroma_cacao", "Manihot_esculenta", "Artocarpus_heterophyllus", "Psidium_guajava",
                               "Borojoa_sp.", "Persea_americana")) %>%
  summarise(Treatment3 = first(Treatment3),
            Plot_ID = first(Plot_ID), animal_species = first(animal_species),
            RegTime = first(RegTime), 
            .groups = 'drop')

#### Functional spaces per habitat ####

### Interactions

# # Number of components to be used:
# cor <- cor.smooth(unique_interactions_t3[, c("LogBodyMass", "LogCropMass", "resWidth", "resFruitWidth", "HandWingIndex", "LogHeight")])
# eigen <- eigen(cor)
# permuted <- matrix(nrow=1000, ncol=6)
# for(i in 1:1000){
#   permuted_data <- apply(unique_interactions_t3[, c("LogBodyMass", "LogCropMass", "resWidth", "resFruitWidth", "HandWingIndex", "LogHeight")],2,sample)
#   permuted[i,] <- eigen(cor.smooth(permuted_data))$values
# }
# thresholds <- apply(permuted, 2, function(x) quantile(x, 0.95))

pca_int_hbt <- principal(unique_interactions_t3[, c("StdBodyMass", "StdCropMass", "StdWidth", "StdFruitWidth", "StdHandWingIndex", "StdHeight")], nfactor = 2, scores = TRUE, rotate = "varimax", covar = FALSE, missing = TRUE, use = "pairwise")

scores_int_hbt <- as.data.frame(pca_int_hbt$scores)
scores_int_hbt$Treatment3 <- unique_interactions_t3$Treatment3
scores_int_hbt$interaction <- unique_interactions_t3$interaction
scores_int_hbt <- scores_int_hbt %>% dplyr::select(interaction, Treatment3, RC1, RC2)

scores_int_hbt$Treatment3 <-  factor(scores_int_hbt$Treatment3,
                                     levels = c('old-growth forest', 'regeneration late', 'regeneration early'))

### Plants

# # Number of components to be used:
# cor <- cor.smooth(unique_plants_t3[, c("LogCropMass", "resFruitWidth", "LogHeight")])
# eigen <- eigen(cor)
# permuted <- matrix(nrow=1000, ncol=3)
# for(i in 1:1000){
#   permuted_data <- apply(unique_interactions_t3[, c("LogCropMass", "resFruitWidth", "LogHeight")],2,sample)
#   permuted[i,] <- eigen(cor.smooth(permuted_data))$values
# }
# thresholds <- apply(permuted, 2, function(x) quantile(x, 0.95))

pca_plants_hbt <- principal(unique_plants_t3[, c("StdCropMass", "StdFruitWidth", "StdHeight")], nfactor = 2, scores = TRUE, rotate = "varimax", covar = FALSE, missing = TRUE, use = "pairwise")

scores_plants_hbt <- as.data.frame(pca_plants_hbt$scores)
scores_plants_hbt$Treatment3 <- unique_plants_t3$Treatment3
scores_plants_hbt$plant_species <- unique_plants_t3$plant_species
scores_plants_hbt <- scores_plants_hbt %>% dplyr::select(plant_species, Treatment3, RC1, RC2)

scores_plants_hbt$Treatment3 <-  factor(scores_plants_hbt$Treatment3,
                                        levels = c('old-growth forest', 'regeneration late', 'regeneration early'))

### Animals

# Number of components to be used:
# cor <- cor.smooth(unique_animals_t3[, c("LogBodyMass", "resWidth", "HandWingIndex")])
# eigen <- eigen(cor)
# permuted <- matrix(nrow=1000, ncol=6)
# for(i in 1:1000){
#   permuted_data <- apply(unique_interactions_t3[, c("LogBodyMass", "resWidth", "HandWingIndex")],2,sample)
#   permuted[i,] <- eigen(cor.smooth(permuted_data))$values
# }
# thresholds <- apply(permuted, 2, function(x) quantile(x, 0.95))

pca_animals_hbt <- principal(unique_animals_t3[, c("StdBodyMass", "StdWidth", "StdHandWingIndex")], nfactor = 2, scores = TRUE, rotate = "varimax", covar = FALSE, missing = TRUE, use = "pairwise")

scores_animals_hbt <- as.data.frame(pca_animals_hbt$scores)
scores_animals_hbt$Treatment3 <- unique_animals_t3$Treatment3
scores_animals_hbt$animal_species <- unique_animals_t3$animal_species
scores_animals_hbt <- scores_animals_hbt %>% dplyr::select(animal_species, Treatment3, RC1, RC2)

scores_animals_hbt$Treatment3 <-  factor(scores_animals_hbt$Treatment3,
                                         levels = c('old-growth forest', 'regeneration late', 'regeneration early'))

#### Originality ####

### Per habitat

alpha_int <- alpha_originality(unique_interactions_plot, scores_int_hbt)
alpha_plants <- alpha_originality(unique_plants_plot, scores_plants_hbt)
alpha_animals <- alpha_originality(unique_animals_plot, scores_animals_hbt)

#### Functional diversity ####

alpha_summarised <- alpha_int %>%
  group_by(Plot_ID) %>%
  summarise(
    Treatment3 = first(Treatment3),
    RegTime = first(RegTime),
    AlphaInt = mean(AlphaOrig),
    .groups = "drop"
  ) 

alpha_plants_summarised <- alpha_plants %>%
  group_by(Plot_ID) %>%
  summarise(
    RegTime = first(RegTime),
    AlphaPlants = mean(AlphaOrig),
    .groups = "drop"
  )

alpha_animals_summarised <- alpha_animals %>%
  group_by(Plot_ID) %>%
  summarise(
    RegTime = first(RegTime),
    AlphaAnimals = mean(AlphaOrig),
    .groups = "drop"
  )

alpha_summarised <- alpha_summarised %>% 
  left_join(select(alpha_plants_summarised, Plot_ID, AlphaPlants,
  ), by = "Plot_ID") %>%
  left_join(select(alpha_animals_summarised, Plot_ID, AlphaAnimals,
  ), by = "Plot_ID") %>%
  select(Plot_ID, RegTime, AlphaInt, AlphaPlants, AlphaAnimals, 
         Treatment3)

m1 <- lm(AlphaInt ~ Treatment3, 
         data = alpha_summarised)
summary(m1)

m2 <- lm(AlphaAnimals ~ Treatment3, 
         data = alpha_summarised)
summary(m2)


m3 <- lm(AlphaPlants ~ Treatment3, 
         data = alpha_summarised)
summary(m3)


#### Recovery analysis ####

data1 <- alpha_summarised

data1$type <- factor(ifelse(1:nrow(data1) %in% grep("OG", data1$Plot_ID), "old", "rec"),
                     levels = c("old", "rec"))

data1Sub <- subset(data1, select = c(type, RegTime, AlphaPlants, AlphaAnimals, AlphaInt))
table(data1Sub$type)
str(data1Sub)

# plot the data

par(mfrow = c(1, 3))
plot(AlphaPlants ~ RegTime, data1Sub)
plot(AlphaAnimals ~ RegTime, data1Sub)
plot(AlphaInt ~ RegTime, data1Sub)
dev.off()

# define data for jags model

jagsData <- 
  with(data1Sub, {
    Y <- cbind(AlphaPlants, AlphaAnimals, AlphaInt)
    Y_old <- Y[type == "old", ]
    Y_rec <- Y[type == "rec", ]
    out <- list(
      Y_old = Y_old,
      n_old = nrow(Y_old),
      s_old = apply(Y_old, 2, FUN = function(x) sd(log(x), na.rm = TRUE)),
      Y_rec = Y_rec,
      n_rec = nrow(Y_rec),
      s_rec = apply(Y_rec, 2, FUN = function(x) sd(log(x), na.rm = TRUE)),
      tx = RegTime[type == "rec"],
      nY = ncol(Y)
    )
    return(out)
  })

# setup JAGS code for the model

jagsModel <- 
  "model{
    # loop on the three different response variables
    for (j in 1:nY) {
      # Loop on observations in old-growth forests to calculate the predicted values
      for (i in 1:n_old) {
        # Model Likelihood for old-growth forests
        Y_old[i, j] ~ dlnorm(log(theta_inf[j]), tau_old[j])
      }
      # Loop on observations in secondary forests to calculate the predicted values
      for (i in 1:n_rec) {							
        mu_rec[i, j] <-
          theta_0[j] + 
          (theta_inf[j] - theta_0[j]) *
          (1 - exp(-lambda[j] * tx[i]))
        # Model Likelihood for secondary forests
        Y_rec[i, j] ~ dlnorm(log(mu_rec[i, j]), tau_rec[j])
      }
      # priors on variance components
      tau_old[j] ~ dscaled.gamma(s_old[j], 2)
      sigmaSq_old[j] <- pow(tau_old[j], -1)
      tau_rec[j] ~ dscaled.gamma(s_rec[j], 2)
      sigmaSq_rec[j] <- pow(tau_rec[j], -1)
      # prior on recovery rate
      lambda[j] ~ dlnorm(0, 1)
      # prior on asymptotic attribute value
      theta_inf[j] ~ dlnorm(0, 1)
      # prior on initial attribute value
      theta_0[j] ~ dlnorm(0, 1)
    }
  }"
cat(jagsModel, file = "jagsModel.txt")


# function to calculate recovery time based on Poorter et al. (2021)
# T90 was calculated by calculating for each moment in time the absolute
# attribute value using the site-specific model equations
# Recovery time is defined as the time needed to recover to 90% of OGF values.

# -> see definition of tx in the function below

recoveryFun <- function(samples, which = 1, maxt = 1000) {
  theta_0 <- do.call(rbind, as.mcmc.list(samples$theta_0))[, paste("theta_0", "[", which, "]", sep = "")]
  theta_inf <- do.call(rbind, as.mcmc.list(samples$theta_inf))[, paste("theta_inf", "[", which, "]", sep = "")]
  lambda <- do.call(rbind, as.mcmc.list(samples$lambda))[, paste("lambda", "[", which, "]", sep = "")]
  tx <- seq(1, maxt, 0.1)
  
  # Initialize vector to store t90 for each sample
  t90_values <- numeric(length(theta_0))
  
  # Loop through the samples to compute t90 for each one
  for (x in 1:length(theta_0)) {
    theta_t <- theta_0[x] + (theta_inf[x] - theta_0[x]) * (1 - exp(-lambda[x] * tx))
    
    if (theta_0[x] <= theta_inf[x]) {
      t90_values[x] <- min(tx[which(theta_t > (0.9 * theta_inf[x]))])
    } else {
      t90_values[x] <- min(tx[which(theta_t < (1.1 * theta_inf[x]))])
    }
  }
  
  # Return the result as an MCMC object with just the t90 column
  return(as.mcmc(t90_values))
}
# function to set initial values

initFun <- function(data) {
  out <- with(data, {
    list(
      lambda = runif(nY, 0, 1),
      theta_inf = runif(nY, 0, 1),
      theta_0 = runif(nY, 0, 1),
      tau_old = runif(nY, 0, 1),
      tau_rec = runif(nY, 0, 1)
    )
  })
  return(out)
}

# initialize the model
## i.e., get probability distributions
set.seed(1212) 
init1 <- jags.model("jagsModel.txt",
                    data = jagsData,
                    inits = initFun(jagsData),
                    n.chains = 5, n.adapt = 1e4)

# sample from posterior distribution

#update(init1, n.iter = 1e4)
monitor1 <- c("theta_0", "theta_inf", "lambda", "sigmaSq_old", "sigmaSq_rec")
samp1 <- jags.samples(init1, variable.names = monitor1, n.iter = 2e5, thin = 2e2)
## thin reduces autocorr between consecutive samples in MCMC
## in that way, although we run 4e4 iterations, we only save 4e4 / 1e2 * 5 samples

# diagnostics

gelman.diag(as.mcmc.list(log(samp1$lambda)))
effectiveSize(as.mcmc.list(log(samp1$lambda)))
autocorr.diag(as.mcmc.list(log(samp1$lambda)))

gelman.diag(as.mcmc.list(log(samp1$theta_0)))
effectiveSize(as.mcmc.list(log(samp1$theta_0)))
autocorr.diag(as.mcmc.list(log(samp1$theta_0)))

gelman.diag(as.mcmc.list(log(samp1$theta_inf)))
effectiveSize(as.mcmc.list(log(samp1$theta_inf)))
autocorr.diag(as.mcmc.list(log(samp1$theta_inf)))

gelman.diag(as.mcmc.list(log(samp1$sigmaSq_old)))
effectiveSize(as.mcmc.list(log(samp1$sigmaSq_old)))
autocorr.diag(as.mcmc.list(log(samp1$sigmaSq_old)))

gelman.diag(as.mcmc.list(log(samp1$sigmaSq_rec)))
effectiveSize(as.mcmc.list(log(samp1$sigmaSq_rec)))
autocorr.diag(as.mcmc.list(log(samp1$sigmaSq_rec)))

# plot
## probability distributions

densityplot(as.mcmc.list(samp1$lambda), scale = list(x = list(log = 10)))
densityplot(as.mcmc.list(samp1$theta_0), scale = list(x = list(log = 10)))
densityplot(as.mcmc.list(samp1$theta_inf), scale = list(x = list(log = 10)))
densityplot(as.mcmc.list(samp1$sigmaSq_old), scale = list(x = list(log = 10)))
densityplot(as.mcmc.list(samp1$sigmaSq_rec), scale = list(x = list(log = 10)))

# extracting posterior samples for metrics

metricList <- lapply(1:3, FUN = function(x) recoveryFun(samp1, which = x, maxt = 1000))
names(metricList) <- colnames(jagsData$Y_old)

# extract variables for plotting

theta_inf <- do.call(rbind, as.mcmc.list(samp1$theta_inf))
theta_0 <- do.call(rbind, as.mcmc.list(samp1$theta_0))
lambda <- do.call(rbind, as.mcmc.list(samp1$lambda))
t90 <- do.call(cbind, metricList)
sigmaSq_rec <- do.call(rbind, as.mcmc.list(samp1$sigmaSq_rec))
sigmaSq_old <- do.call(rbind, as.mcmc.list(samp1$sigmaSq_old))

qt90 <-  sapply(metricList, FUN = function(x) {
  quantile(as.numeric(x), prob = c(0.025, 0.25, 0.5, 0.75, 0.975))
})
qtheta_0 <- apply(theta_0, 2, FUN = quantile, prob = c(0.025, 0.25, 0.5, 0.75, 0.975))
qtheta_inf <- apply(theta_inf, 2, FUN = quantile, prob = c(0.025, 0.25, 0.5, 0.75, 0.975))
qlambda <- apply(lambda, 2, FUN = quantile, prob = c(0.025, 0.25, 0.5, 0.75, 0.975))

qsigmarec <- apply(sigmaSq_rec, 2, FUN = quantile, prob = c(0.025, 0.25, 0.5, 0.75, 0.975))
qsigmaold <- apply(sigmaSq_old, 2, FUN = quantile, prob = c(0.025, 0.25, 0.5, 0.75, 0.975))

# plants vs. animals
perc_pxa <- 1 - mean(metricList$AlphaPlants > metricList$AlphaAnimals) 
# plants vs. interactions
perc_pxi <- 1 - mean(metricList$AlphaPlants > metricList$AlphaInt)
# animals vs. interactions
perc_axi <- 1 - mean(metricList$AlphaAnimals > metricList$AlphaInt)

#### Save data ####

# scores <- list(scores_int_hbt, scores_plants_hbt, scores_animals_hbt)
# originalities <- list(alpha_int, alpha_plants, alpha_animals)
# model_samples <- list(lambda, theta_0, theta_inf, t90)
# diff_tests <- list(qt90, perc_pxa, perc_pxi, perc_axi, qtheta_inf)
# 
# saveRDS(scores, "scores.RData")
# saveRDS(originalities, "originalities.RData")
# saveRDS(data1, "analysis_data.RData")
# saveRDS(model_samples, "model_samples.RData")
# saveRDS(diff_tests, "groups_diffs.RData")

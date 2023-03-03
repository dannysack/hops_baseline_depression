# load libraries
library(tidyverse)
library(rms)
library(kableExtra)
library(SimCorMultRes)

# set working directory
setwd("/Users/sackd/Library/CloudStorage/Box-Box/Vanderbilt University/PhD/Post-Dissertation Projects/HoPS+ Depression/Analysis/")

# pull in rclin.clm function from Tian paper (downloaded from https://github.com/YuqiTian35/CPMGEE_Code/blob/master/data_generation.R)
source("./Tian et al/data_generation.R")

# set seed
set.seed(1111)

# pull in base dataset
Load(sim_base)

# create a function that draws from this and creates ordinal, clustered outcome
sim_func <- function(data, betas, alpha){
  # sample from data, but assume full enrollment so the function works
  d <- sample_n(data, 1080, replace = TRUE)
  
  # get cluster size (fixed in hops+)
  cluster_size <- 45
  
  # start with latent correlation matrix
  latent_correlation_matrix <- matrix(alpha, nrow = cluster_size, ncol = cluster_size)
  diag(latent_correlation_matrix) <- 1
  
  # simulate ordinal outcome
  y <- rcont.clm(clsize = cluster_size,
                 betas = betas,
                 xdata = d,
                 xformula = ~ group.female + phq9_b.female,
                 cor.matrix = latent_correlation_matrix)
  
  # get data out (and round ordinal outcome to nearest 1) - recognizing that it breaks up intervention and control to across clinics
  out <- y$simdata %>% mutate(Y = round(y))
}

# set up dataframe for simulations

# number of simulations
n <- 10000

# set up betas (from actual model)
beta1 <- 0.7772096
beta2 <- 0.1622805

# set 3 different alphas
alphas <- c(0.2, 0.3, 0.4)

# create dataframe
sims <- expand_grid(n = 1:n, beta1 = beta1, beta2 = beta2, alpha = alphas,
                    est1 = NA, est2 = NA, se1 = NA, se2 = NA)

for(i in 1:nrow(sims)){
  #simulate data
  dat <- sim_func(data = sim_base, betas = c(sims$beta1[i], sims$beta2[i]), alpha = sims$alpha[i])

  # now calculate orm model
  mod <- orm(Y ~ group.female + phq9_b.female, data = dat, x = TRUE, y = TRUE)
  modrob <- robcov(mod, cluster = dat$id)

  # now pull out betas
  sims$est1[i] <- modrob$coefficients["group.female=Intervention"]
  sims$est2[i] <- modrob$coefficients["phq9_b.female"]

  # pull out standard errors
  sims$se1[i] <- sqrt(diag(modrob$var))["group.female=Intervention"]
  sims$se2[i] <- sqrt(diag(modrob$var))["phq9_b.female"]
}

# now calculate bias and coverage
sims <- sims %>%
  mutate(bias1 = abs(beta1 - est1),
         bias2 = abs(beta2 - est2),
         cov1 = ifelse((est1 - (1.96*se1) < beta1) & (beta1 < est1 + (1.96*se1)), 1, 0),
         cov2 = ifelse((est2 - (1.96*se2) < beta2) & (beta2 < est2 + (1.96*se2)), 1, 0))

# get mean bias and coverage (update so it is by alpha with group_by)
sims %>% group_by(alpha) %>%
  summarise(bias1 = mean(bias1),
            bias2 = mean(bias2),
            coverage1 = mean(cov1),
            coverage2 = mean(cov2))

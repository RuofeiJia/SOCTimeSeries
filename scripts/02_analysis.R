# analyze df_std

# library
library(dplyr)
library(tictoc)
library(brms)

# working directory should be set by default to the folder of entire repo (R project)

# load dataset for analysis (object name: df_std)
load("./data/SOC_withNewData_for_analysis.rda")
df_std <- df_std_withNewData; rm(df_std_withNewData)

# separate out time series data
df_std_ts <- df_std %>% filter(!grepl("OLM-", siteID)) %>%
  mutate(data_type="Time series") %>%
  mutate(data_type=factor(data_type, levels=c("Time series", "Single time")))


#-------------- 1. Wilcoxon test for SOC rates in young vs. old forests --------
# exclude first observations
df_change <- df_std_ts %>% group_by(plotID) %>% 
  arrange(plotID,observation_year) %>%
  filter(observation_year!=first(observation_year)) %>%
  ungroup()

# pull SOC rates
young_forest_rate <- df_change[df_change$forest_cond_gf_besnard60=="young forest",]$abs30_rate_mgha_yr
old_forest_rate <- df_change[df_change$forest_cond_gf_besnard60=="old forest",]$abs30_rate_mgha_yr
# wilxocon test
t1_gr <- wilcox.test(young_forest_rate,old_forest_rate, alternative="greater")
t1_gr$p.value # 3.529714e-05 young forest significantly > old forest rates

# test young and old forest SOC rate without gapfilled age grouping
young_forest_rate_nogf <- df_change[df_change$forest_age_gapfill==F & 
                                      df_change$forest_cond_gf_besnard60=="young forest",]$abs30_rate_mgha_yr
old_forest_rate_nogf <- df_change[df_change$forest_age_gapfill==F & 
                                    df_change$forest_cond_gf_besnard60=="old forest",]$abs30_rate_mgha_yr
# wilxocon test
t1_gr_nogf <- wilcox.test(young_forest_rate_nogf,old_forest_rate_nogf, alternative="greater")
t1_gr_nogf$p.value # 0.005270655 young forest significantly > old forest rates


#-------------- 2. statistical model -------------------------------------------
# model structure is the following:

# brsm_mod <- brm(
#   SOC_stock30_log ~ 
#     t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
#     t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
#     forest_cond_gf_besnard60 +
#     s(std_year, by = std_n_pct_plo, k=5) + 
#     s(std_year, by = std_clay_plo, k=5) + 
#     s(std_year, by = std_ph_plo, k=5) +
#     (1 | siteID),
#   data = df_std,
#   family = gaussian(),
#   prior = c(
#     prior(normal(0, 5), class = "b") # priors for fixed effects
#   ), # rest of prior stays default
#   chains = 12, # number of Markov chain
#   iter = 37000,
#   warmup = 1000, # default warmup = floor(iter/2)
#   thin = 72
# )

# because of the large computer power needed, each chain were run parallel 
# and then combined together
# actual code being run is below (where each chain were ran and saved in parallel) 

# if run model with full dataset, put "No"
# if run model with only time series (repearted SOC) data, put "Yes"
timeseries_only <- "No"

# select full data or only time series data
if (timeseries_only=="Yes") {
  df_std <- df_std_ts
}

## script for chain1
print("start: run chain1")
tic()
try(
  fit_chain1 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 10015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain1")

# save chain
tic(); print("start: saving chain1")
save(fit_chain1, file="./results/chain1_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain1_fit_direct_37k.rda")

## script for chain2
print("start: run chain2")
tic()
try(
  fit_chain2 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 20015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain2")

# save chain
tic(); print("start: saving chain2")
save(fit_chain2, file="./results/chain2_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain2_fit_direct_37k.rda")

## script for chain3
print("start: run chain3")
tic()
try(
  fit_chain3 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 30015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain3")

# save chain
tic(); print("start: saving chain3")
save(fit_chain3, file="./results/chain3_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain3_fit_direct_37k.rda")

## script for chain4
print("start: run chain4")
tic()
try(
  fit_chain4 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 40015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain4")

# save chain
tic(); print("start: saving chain4")
save(fit_chain4, file="./results/chain4_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain4_fit_direct_37k.rda")

## script for chain5
print("start: run chain5")
tic()
try(
  fit_chain5 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 50015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain5")

# save chain
tic(); print("start: saving chain5")
save(fit_chain5, file="./results/chain5_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain5_fit_direct_37k.rda")


## script for chain6
print("start: run chain6")
tic()
try(
  fit_chain6 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 60015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain6")

# save chain
tic(); print("start: saving chain6")
save(fit_chain6, file="./results/chain6_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain6_fit_direct_37k.rda")

## script for chain7
print("start: run chain7")
tic()
try(
  fit_chain7 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 70015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain7")

# save chain
tic(); print("start: saving chain7")
save(fit_chain7, file="./results/chain7_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain7_fit_direct_37k.rda")

## script for chain8
print("start: run chain8")
tic()
try(
  fit_chain8 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 80015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain8")

# save chain
tic(); print("start: saving chain8")
save(fit_chain8, file="./results/chain8_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain8_fit_direct_37k.rda")

## script for chain9
print("start: run chain9")
tic()
try(
  fit_chain9 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 90015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain9")

# save chain
tic(); print("start: saving chain9")
save(fit_chain9, file="./results/chain9_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain9_fit_direct_37k.rda")

## script for chain10
print("start: run chain10")
tic()
try(
  fit_chain10 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 100015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain10")

# save chain
tic(); print("start: saving chain10")
save(fit_chain10, file="./results/chain10_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain10_fit_direct_37k.rda")

## script for chain11
print("start: run chain11")
tic()
try(
  fit_chain11 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 110015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain11")

# save chain
tic(); print("start: saving chain11")
save(fit_chain11, file="./results/chain11_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain11_fit_direct_37k.rda")

## script for chain12
print("start: run chain12")
tic()
try(
  fit_chain12 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 1, # number of Markov chain
    core = 1,
    threads = threading(64), # core*threads = cpus-per-tasks
    seed = 120015, # a random number to ensure reproducibility
    iter = 37000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 72
  )
)
toc()
print("done: run chain12")

# save chain
tic(); print("start: saving chain12")
save(fit_chain12, file="./results/chain12_fit_direct_37k.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/chain12_fit_direct_37k.rda")

## script for combining all 12 chains
# combine chains
print("start: combine chains"); tic()
brms1 <- combine_models(fit_chain1, fit_chain2, fit_chain3, fit_chain4,
                        fit_chain5, fit_chain6, fit_chain7, fit_chain8,
                        fit_chain9, fit_chain10, fit_chain11, fit_chain12)
print("done: combine chains"); toc()

# save combined model
print("start: saving brms1"); tic()
if (timeseries_only=="Yes") {
  # if fitted with only time series data, save to this directory
  save(brms1, file="./results/brms1_timeseries_37k12chain.rda")
} else {
  # if fitted with full data, save to this directory
  save(brms1, file="./results/brms1_withOneTime_37k12chain.rda")
}
print("done: brms1 saved"); toc()

#-------------- 4. model diagnostics -------------------------------------------
library(rstan) # for check_hmc_diagnostics()
library(bayesplot) # for pp_check
library(ggplot2)

print("load model"); tic()
if (timeseries_only=="Yes") {
  # if diagnose model fitted with only time series data, load this model
  load("./results/brms1_timeseries_37k12chain.rda")
} else {
  # if diagnose model fitted with full data, load this models
  load("./results/brms1_withOneTime_37k12chain.rda")
}
print("done: load model"); toc()

# rename
gamm_brms <- brms1; rm(brms1) # withOneTime_brms1

# check R hat and ESS
summary(gamm_brms)

# divergences, treedepth, BFMI
print("check_hmc_diagnostics() result:")
check_hmc_diagnostics(gamm_brms$fit)  

# posterior predictive check
print("pp_check() result:")
pp_check(gamm_brms)

# R squared
# https://easystats.github.io/performance/reference/r2_bayes.html
# the "marginal" R2 calls bayes_R2(re.form = NA), 
# while the "conditional" R2 calls bayes_R2(re.form = NULL)

# marginal R2 (ignoring random intercepts)
print("start: marginal r squared"); tic()
r_squared_marg <- bayes_R2(gamm_brms, re.form=NA)
r_squared_marg
print("done: marginal r squared"); toc()

# conditional R2 (with random intercepts)
print("start: conditional r squared"); tic()
r_squared_cond <- bayes_R2(gamm_brms, re.form=NULL)
r_squared_cond
print("done: conditional r squared"); toc()
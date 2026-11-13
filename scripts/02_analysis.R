# analyze df_std

# library
library(dplyr)
library(tictoc)
library(brms)
library(cmdstanr)

# working directory should be set by default to the folder of entire repo (R project)

# load dataset for analysis (object name: df_std)
load("./data/SOC_timeseries_for_analysis_2026Jul29.rda")

#-------------- 1. Wilcoxon test for SOC rates in young vs. old forests --------
# exclude first observations
df_change <- df_std %>% group_by(plotID) %>% 
  arrange(plotID,observation_year) %>%
  filter(observation_year!=first(observation_year)) %>%
  ungroup()

# pull SOC rates
young_forest_rate <- df_change[df_change$forest_cond_gf_besnard60=="young forest",]$abs30_rate_mgha_yr
old_forest_rate <- df_change[df_change$forest_cond_gf_besnard60=="old forest",]$abs30_rate_mgha_yr
# wilxocon test
t1_gr <- wilcox.test(young_forest_rate,old_forest_rate, alternative="greater")
t1_gr$p.value # 0.001691015 young forest significantly > old forest rates

# test young and old forest SOC rate without gapfilled age grouping
young_forest_rate_nogf <- df_change[df_change$forest_age_gapfill==F & 
                                      df_change$forest_cond_gf_besnard60=="young forest",]$abs30_rate_mgha_yr
old_forest_rate_nogf <- df_change[df_change$forest_age_gapfill==F & 
                                    df_change$forest_cond_gf_besnard60=="old forest",]$abs30_rate_mgha_yr
# wilxocon test
t1_gr_nogf <- wilcox.test(young_forest_rate_nogf,old_forest_rate_nogf, alternative="greater")
t1_gr_nogf$p.value # 0.0340337 young forest significantly > old forest rates


#-------------- 2. statistical model -------------------------------------------
# run model
# this model was run in the MIT Engaging cluster
# https://orcd.mit.edu/resources/about-engaging-cluster 
print("start: run model")
tic()
try(
  brms1 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + # comment out this line for sensitivity analysis of model without soil N
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID) + (1 | plotID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    #save_pars = save_pars(all = TRUE),
    chains = 3, # number of Markov chain
    cores = 3, 
    threads = threading(32), # core*threads = cpus-per-tasks in bash script
    seed = 15, # a random number to ensure reproducibility
    iter = 26000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 50,
    backend="cmdstanr"
  )
)
toc()
print("done: run brms1")

# save model and fitted data
tic(); print("start: saving brms model")
save(brms1, file="./results/brms1_new_timeseries_cmd_3chain26k_50thin.rda")
toc(); print("done: withOneTime_brms1 saved to ../results/brms1_new_timeseries_cmd_3chain26k_50thin.rda")

#-------------- 3. model diagnostics -------------------------------------------
library(rstan) # for check_hmc_diagnostics()
library(bayesplot) # for pp_check
library(ggplot2)

print("load model"); tic()
load("./results/brms1_new_timeseries_cmd_3chain26k_50thin.rda")
print("done: load model"); toc()

# check R hat and ESS
summary(brms1)

# inspect MCMC chains
plot(gamm_brms)

# divergences, treedepth, BFMI
print("check_hmc_diagnostics() result:")
check_hmc_diagnostics(brms1$fit)  

# posterior predictive check
print("pp_check() result:")
pp_check(brms1)

# R squared
# https://easystats.github.io/performance/reference/r2_bayes.html
# the "marginal" R2 calls bayes_R2(re.form = NA), 
# while the "conditional" R2 calls bayes_R2(re.form = NULL)

# marginal R2 (ignoring random intercepts)
print("start: marginal r squared"); tic()
r_squared_marg <- bayes_R2(brms1, re.form=NA)
r_squared_marg
print("done: marginal r squared"); toc()

# conditional R2 (with random intercepts)
print("start: conditional r squared"); tic()
r_squared_cond <- bayes_R2(brms1, re.form=NULL)
r_squared_cond
print("done: conditional r squared"); toc()
#-------------- 4. sensitivity analysis with soil N ----------------------------
# model without soil N term was run with code in section 2 without the term 
# s(std_year, by = std_n_pct_plo, k=5)

# load model with and without soil N
load("./results/brms1_new_timeseries_cmd_3chain26k_50thin.rda")
brms_mod <- brms1; rm(brms1)
load("./results/brms1_new_timeseries_cmd_3chain26k_50thin_noN.rda")
brms_mod_noN <- brms1; rm(brms1)

model_name_list <- c("natural soil N","no soil N")

# add criteria to the models
tic()
brms_mod <- add_criterion(brms_mod, "waic")
brms_mod_noN <- add_criterion(brms_mod_noN, "waic")
toc() # 30 sec

loo_compare(brms_mod, brms_mod_noN,
            criterion = "waic", 
            model_names = model_name_list
)
# elpd_diff se_diff
# natural soil N    0.0       0.0 
# no soil N      -130.3      20.6 

# SOC sink estimates from the model without soil N is not shown in the repo
# The total SOC sink estimate was reported in Methods
# It was run with the same scripts from this repo, but with the model without soil N
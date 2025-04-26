# analyze df_std

# library
library(dplyr)
library(tictoc)
library(brms)

# working directory should be set by default to the folder of entire repo (R project)

# load dataset for analysis (object name: df_std)
load("./data/SOC_time_series_for_analysis.rda")

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
t1_gr$p.value # 3.737498e-05 young forest significantly > old forest rates

# test young and old forest SOC rate without gapfilled age grouping
young_forest_rate_nogf <- df_change[df_change$forest_age_gapfill==F & 
                                      df_change$forest_cond_gf_besnard60=="young forest",]$abs30_rate_mgha_yr
old_forest_rate_nogf <- df_change[df_change$forest_age_gapfill==F & 
                                    df_change$forest_cond_gf_besnard60=="old forest",]$abs30_rate_mgha_yr
# wilxocon test
t1_gr_nogf <- wilcox.test(young_forest_rate_nogf,old_forest_rate_nogf, alternative="greater")
t1_gr_nogf$p.value # 0.005420645 young forest significantly > old forest rates


#-------------- 2. statistical model -------------------------------------------
print("start: run model")
tic()
try(
  gamm_brms12 <- brm(
    SOC_stock30_log ~ 
      t2(std_year, std_MAT_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      t2(std_year, std_MAP_cru, by = forest_cond_gf_besnard60, k=5, m=1) + 
      forest_cond_gf_besnard60 +
      s(std_year, by = std_n_pct_plo, k=5) + 
      s(std_year, by = std_clay_plo, k=5) + 
      s(std_year, by = std_ph_plo, k=5) +
      (1 | siteID) + 
      (1 | plotID),
    data = df_std,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "b") # priors for fixed effects
    ), # rest of prior stays default
    save_pars = save_pars(all = TRUE),
    chains = 4, # number of Markov chain
    cores = 4, 
    threads = threading(4),  # use 4 threads per chain
    seed = 15, # a random number to ensure reproducibility
    iter = 51000,
    warmup = 1000, # default warmup = floor(iter/2)
    thin = 100
  )
)
toc()
print("done: run gamm_brms12")

# save
tic(); print("start: saving brms model")
save(gamm_brms12, file="./results/gamm_brms12.rda")
toc(); print("done: gamm_brms12 saved to ./results/gamm_brms12.rda")


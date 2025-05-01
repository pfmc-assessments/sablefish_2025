library(r4ss)

dat_file <- "2025_sablefish_dat.ss"
ctl_file <- "2025_sablefish_ctl.ss"

model_2023 <- SS_output(here::here("model", "_bridging", "0_2023_model"))
m_prior <- SS_output(here::here("model", "_bridging", "13_m_prior"))
age_based_ret <- SS_output(here::here("model", "_bridging", "14m_fix_survey_selex_params"))

copy_files <- function(x, from_name, to_name){
  file.copy(
    from = here::here("model", from_name,  x),
    to = here::here("model", to_name,  x)
  )
}

#===============================================================================
# Initial discard fleet model
#===============================================================================
model_name <- "0_init_hessian"
old_dir <- model_name
model_0 <- SS_output(here::here("model", model_name))
# NLL = 2434.45
SS_plots(model_0)

model_0 <- SS_output(here::here("model", model_name))
model_mod <- model_0
model_mod$agedbase <- model_mod$agedbase |> dplyr::filter(!Fleet %in% 4:6, Sexes == 3)
SS_plots(model_mod)

dw <- r4ss::tune_comps(
  replist = model_0, 
  dir = here::here("model", model_name),
  option = "Francis")
ctl <- SS_readctl(file = here::here("model", model_name, ctl_file))
ctl$Variance_adjustment_list <- dw
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", model_name, ctl_file),
  overwrite = TRUE)

modelnames <- c(
  "2023 Base", 
  "13. M Prior",
  "14. Age-Based Retention",
  "0-Init Model")
mysummary <- SSsummarize(list(model_2023, m_prior, age_based_ret, model_0))
SSplotComparisons(mysummary,
                  filenameprefix = "0_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_plots"),
                  pdf = TRUE)
old_dir <- model__name

#===============================================================================
# Trawl landings fleet selectivity parameterization
#===============================================================================

model_name <- "1.0_selex_trawl_double_normal_sex_w_block"

selex_twl <- SS_output(here::here("model", model_name))
# double normal (male = female) = 219.917
# add male offset
# LL trawl ages (sex ) = 214.99
# LL trawl ages sex-specific with block on peak and asc = 207.705
# LL trawl ages sex-specific with block on peak, top, asc = 207.689

plot_year_selex(
  replist = selex_twl,
  fleets = 1,
  year = 1890)
plot_year_selex(
    replist = selex_twl,
    fleets = 1,
    year = 2024)
  
model_mod <- selex_twl
model_mod$agedbase <- model_mod$agedbase |> dplyr::filter(Sexes == 3, Yr < 2002)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 1
)
model_mod <- selex_twl
model_mod$agedbase <- model_mod$agedbase |> dplyr::filter(Sexes == 3, Yr >= 2002)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 1
)
model_mod <- selex_twl
model_mod$agedbase <- model_mod$agedbase |> dplyr::filter(Sexes == 3)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 1
)

SS_plots(model_mod, plot = c(17))

SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 1
)

old_dir <- model__name

#===============================================================================
# Trawl landings fleet selectivity parameterization: spline
#===============================================================================

model_name <- "1.1_selex_trawl_spline"
dir.create(here::here("model", model_name))
files <- list.files(here::here("model", old_dir))
copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = model_name)

spline_twl <- SS_output(here::here("model", model_name))
# add male offset
# LL trawl ages = 214.99 (double normal by sex)
# LL with autogen parameters: 216.716 (autogen nodes w/ corrected block)
# LL with spline added blocks = 206.485

plot_year_selex(
  replist = spline_twl,
  fleets = 1,
  year = 2024)

model_mod <- spline_twl
model_mod$agedbase <- model_mod$agedbase |> dplyr::filter(Sexes == 3, Yr < 2002)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 1
)
model_mod <- spline_twl
model_mod$agedbase <- model_mod$agedbase |> dplyr::filter(Sexes == 3, Yr >= 2002)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 1
)
model_mod <- spline_twl
model_mod$agedbase <- model_mod$agedbase |> dplyr::filter(Sexes == 3)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 1
)
SS_plots(model_mod)

old_dir <- model__name

#===============================================================================
# HKL/Pot landings fleet selectivity parameterization: spline
#===============================================================================

model_name <- "1.2_selex_hkl_spline"
dir.create(here::here("model", model_name))
files <- list.files(here::here("model", old_dir))
copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = model_name)

spline_hkl <- SS_output(here::here("model", model_name))
# LL from hkl and pot from the double normal model = 536.254 70.0853
# LL with spline = 573.275 70.278 (with autogen)
# LL with spline add blocks =  545.706 67.0931
# based on the N eff the spline fits the early block much better

plot_year_selex(
  replist = spline_hkl,
  fleets = 2,
  year = 2024)

model_mod <- spline_hkl
model_mod$agedbase <- model_mod$agedbase |> 
  dplyr::filter(Sexes == 3, Yr < 2002)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 3
)
SS_plots(model_mod, plot = c(16,17,26))

old_dir <- model__name

#===============================================================================
# Discard fleet selectivity and blocks
#===============================================================================

model_name <- "1.3_selex_discard_fleets"
discard <- SS_output(here::here("model", model_name))
# Previous model LL = 248.742 71.6872 103.07
# Adj. trawl discard parameters LL (double normal) = 219.681 (no blocks)
# Tried the spline for trawl discards and it fit much worse under the initial set-up

plot_year_selex(
  replist = discard,
  fleets = 4,
  year = 1890)

model_mod <- discard
model_mod$agedbase <- model_mod$agedbase |> dplyr::filter(Sexes == 0)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 4
)
SS_plots(discard, plot = c(16,17,26))

old_dir <- model__name


#===============================================================================
# Discard fleet selectivity and blocks
#===============================================================================

model_name <- "1.4_selex_hkl_discard"
hkl_discard <- SS_output(here::here("model", model_name))
# Previous model LL = 72.1842 103.259
# Remove mirroring between HKL and Pot LL = 67.4911 100.923

plot_year_selex(
  replist = discard,
  fleets = 5,
  year = 1890)
plot_year_selex(
  replist = hkl_discard,
  fleets = 5,
  year = 1890)
plot_year_selex(
  replist = hkl_discard,
  fleets = 6,
  year = 1890)

plot_year_selex(
  replist = discard,
  fleets = 5,
  year = 2011)
plot_year_selex(
  replist = hkl_discard,
  fleets = 5,
  year = 2011)
plot_year_selex(
  replist = hkl_discard,
  fleets = 6,
  year = 2011)

plot_year_selex(
  replist = discard,
  fleets = 5,
  year = 2024)
plot_year_selex(
  replist = hkl_discard,
  fleets = 5,
  year = 2024)
plot_year_selex(
  replist = hkl_discard,
  fleets = 6,
  year = 2024)

model_mod <- hkl_discard
model_mod$agedbase <- model_mod$agedbase |> 
  dplyr::filter(Sexes == 0, Yr < 2011)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 5
)
model_mod <- hkl_discard
model_mod$agedbase <- model_mod$agedbase |> 
  dplyr::filter(Sexes == 0, Yr %in% 2011:2018)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 5
)
model_mod <- hkl_discard
model_mod$agedbase <- model_mod$agedbase |> 
  dplyr::filter(Sexes == 0, Yr >= 2019)
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 5
)
SS_plots(hkl_discard, plot = c(17,26))

#===============================================================================
# Data weight
#===============================================================================

model_name <- "1.5_data_weight"
data_weight <- SS_output(here::here("model", model_name))

dw <- r4ss::tune_comps(
  replist = data_weight, 
  dir = here::here("model", model_name),
  option = "Francis")[, 1:3]
ctl <- SS_readctl(file = here::here("model", model_name, ctl_file))
ctl$Variance_adjustment_list <- dw
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", model_name, ctl_file),
  overwrite = TRUE)

data_weight <- SS_output(here::here("model", model_name))
SS_plots(data_weight)

modelnames <- c(
  "2023 Base", 
  "13. M Prior",
  "0-Init Model",
  "1.5-Fishery Selectivity")
mysummary <- SSsummarize(list(model_2023, m_prior, model_0, data_weight))
SSplotComparisons(mysummary,
                  filenameprefix = "1.5_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_plots"),
                  pdf = TRUE)

#===============================================================================
# WCGBT Selectivity - sex specific
#===============================================================================

model_name <- "1.6_selex_wcgbt"
wcgbt <- SS_output(here::here("model", model_name))

# Initial LL = 842.941
# Asymptotic LL (peak = 1.98298, asc = -9.35776) = 957.192 
# 843.839
# sex-specific selex = 859.164
# autogen 3 spline = 813.001 (811.549)
# autogen 4 spline = 809.64 

model_mod <- wcgbt
model_mod$agedbase <- model_mod$ghostagedbase |> 
  dplyr::filter(Sexes == 3, Fleet == 10) |>
  dplyr::mutate(Pearson = 0, effN =  1, Used = "yes")
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 10
)

SS_plots(wcgbt, plot = c(2, 16:21, 26))

#===============================================================================
# WCGBT Selectivity - sexes equal
#===============================================================================

model_name <- "1.7_selex_wcgbt_3_spline_2"
model_name <- "1.8_selex_wcgbt_2023"
model_name <- "1.9_selex_wcgbt_normal"
wcgbt <- SS_output(here::here("model", model_name))

# Initial LL = 842.941
# Asymptotic LL (peak = 1.98298, asc = -9.35776) = 957.192 
# 843.839
# sex-specific selex = 859.164
# autogen 3 spline = 813.001
# autogen 4 spline = 809.64 
# autogen 4 spline (sexes equal) = 812.671
# autogen 3 spline (sexes equal) = 814.478
# LL with 2023 parameterization = 842.92

model_mod <- wcgbt
model_mod$agedbase <- model_mod$ghostagedbase |> 
  dplyr::filter(Sexes == 3, Fleet == 10) |>
  dplyr::mutate(Pearson = 0, effN =  1, Used = "yes")
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 10
)

SS_plots(wcgbt, plot = c(2, 16:21, 26))

#===============================================================================
# 2.1 Selex
#===============================================================================

model_name <- "2.1_selex_trawl_double_normal_sex_w_block_split_fixed"
split_hkl_pot <- SS_output(here::here("model", model_name))
SS_plots(split_hkl_pot)

plot_fleet_selectivity(
  model_path = here::here("model", model_name), 
  model_out = split_hkl_pot, 
  fleet_num = 3)

p1 <- plot_year_selex(
  replist = split_hkl_pot,
  fleets = 1,
  year = 2024)
p2 <- plot_year_selex(
  replist = split_hkl_pot,
  fleets = 1,
  year = 1890)
cowplot::plot_grid(p1, p2, nrow = 2)

p1 <- plot_year_selex(
  replist = split_hkl_pot,
  fleets = 2,
  year = 2024)
p3 <- plot_year_selex(
  replist = split_hkl_pot,
  fleets = 2,
  year = 1890)
cowplot::plot_grid(p1, p2, nrow = 2)

#===============================================================================
# 3.0 Rec Devs
#===============================================================================

# Turn off pre-model deves
# Push back main year dev period
# Update bias adjustment
#_no timevary SR parameters
# 1 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
# 1970 # first year of main recr_devs; early devs can preceed this era
# 2023 # last year of main recr_devs; forecast devs start in following year
# 3 #_recdev phase
# 1 # (0/1) to read 13 advanced options
# 1890 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
# 3 #_recdev_early_phase
# 3 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
# 1 #_lambda for Fcast_recr_like occurring before endyr+1
# 1976 #_last_yr_nobias_adj_in_MPD; begin of ramp
# 1980 #_first_yr_fullbias_adj_in_MPD; begin of plateau
# 2021 #_last_yr_fullbias_adj_in_MPD
# 2023 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
# 0.729 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)

model_name <- "3.0_recdevs_adj_early_years_bias_adj"
dev_period <- SS_output(here::here("model", model_name))
SS_plots(dev_period, plot = 1:7)

# Moving the early dev period to include that early large deviation, resulted
# in that dev being estimated to be much lower due to the sum to 0 constraint
# during the early period of the model.

# Total NLL = 2427.44 vs (2.2_selex_trawl_double = 2433.52) (3.2: 2428.08)
# Recruitment LL = 47.5  (51.5)
# Eliminate estimating early deviations so the likelihood should change
# Age NLL = 2339.75 (2.2: 2342.23) (3.2: 2338.91)
# Length NLL = 86.1403 (85.8674) 

# The sum of the early period devs = -22.6
# The sum of main period devs = -4.21e-06

#===============================================================================
# 3.1 Rec Devs - option 2 - remove sum to zero constraint
#===============================================================================

model_name <- "3.1_recdevs_adj_early_years_bias_adj_opt=2"
dev_opt_2 <- SS_output(here::here("model", model_name))
SS_plots(dev_opt_2, plot = 3:4)

# The sum of the early period devs = -18.2
# The sum of main period devs = 25.5

#===============================================================================
# 3.2 Rec Devs - move main devs to start in 1960
#===============================================================================

model_name <- "3.2_recdevs_adj_main_dev_start"
dev <- SS_output(here::here("model", model_name))
SS_plots(dev, plot = 3:4)

#===============================================================================
# 4.0 discard fleet length selex
#===============================================================================

# Look at the implied aggregated fits to the discard length data
model_name <- "4.0_discard_data"
discard_selex <- r4ss::SS_output(here::here("model", model_name))
model_mod <- discard_selex
model_mod$lendbase <- model_mod$ghostlendbase |> 
  dplyr::filter(Sexes == 0) |>
  dplyr::mutate(Pearson = 0, effN =  0, Used = "yes")
SSplotComps(
  replist = model_mod, 
  print = TRUE,
  plotdir = here::here("model", "4.0_discard_length_selex", "unsexed_ghostfleet_fits")
)
model_mod$agedbase <- model_mod$ghostagedbase |> 
  dplyr::filter(Sexes == 3, Fleet == 10) |>
  dplyr::mutate(Pearson = 0, effN =  0, Used = "yes")
SSplotComps(
  replist = model_mod,
  #subplots = 21,
  kind = "AGE", 
  fleets = 10,
  print = TRUE,
  maxrows = 3,
  maxcols = 3,
  plotdir = here::here("model", "4.0_discard_length_selex", "sexed_ghostfleet_fits")
)

#===============================================================================
# 4.1 discard fleet length selex
#===============================================================================

# Look at the implied aggregated fits to the discard length data
model_name <- "4.1_discard_length_selex"

dat <- SS_readdat(
  file = here::here("model", model_name, "2025_sablefish_dat.ss"))

lencomp <- dplyr::bind_rows(
  dat$lencomp |> dplyr::filter(fleet == 10),
  dat$lencomp |> dplyr::filter(fleet != 10) |> dplyr::mutate(fleet = -1 * fleet)
)
dat$lencomp <- lencomp

agecomp <- dplyr::bind_rows(
  dat$agecomp |> dplyr::filter(!fleet %in% 4:6),
  dat$agecomp |> dplyr::filter(fleet %in% 4:6) |> dplyr::mutate(fleet = -1 * fleet)
)
dat$agecomp <- agecomp

SS_writedat(
  datlist = dat, 
  outfile = here::here("model",  model_name, "2025_sablefish_dat.ss"),
  overwrite = TRUE)

discard_len_selex <- SS_output(here::here("model", model_name))
# selectivity plots are plot = 2
# time series are plot = 3
SS_plots(discard_len_selex, plot = c(2, 16:26))
model_mod <- discard_len_selex
model_mod$agedbase <- model_mod$ghostagedbase |> 
  dplyr::filter(Sexes == 0) |>
  dplyr::mutate(Pearson = 0, effN =  0, Used = "yes")
SSplotComps(
  replist = model_mod,
  kind = "AGE", 
  print = TRUE,
  maxrows = 3,
  maxcols = 3,
  plotdir = here::here("model", model_name, "unsexed_ghostfleet_fits")
)

#===============================================================================
# 5.0 fix discard catches
#===============================================================================

model_name <- "5.0_fix_discard_catch"
fix_catch <- SS_output(here::here("model", model_name))
# Run the model

# Update the data weights 
dw <- r4ss::tune_comps(
  replist = fix_catch, 
  dir = here::here("model", model_name),
  option = "Francis")
#ctl <- SS_readctl(file = here::here("model", model_name, ctl_file))
#ctl$Variance_adjustment_list <- dw
#SS_writectl(
#  ctllist = ctl, 
#  outfile = here::here("model", model_name, ctl_file),
#  overwrite = TRUE)
# Rerun the model with the new data weights
fix_catch <- SS_output(here::here("model", model_name))
SS_plots(fix_catch)

#===============================================================================
# 5.1 fix discard catches
#===============================================================================

model_name <- "5.1_weight_at_age"
wtatage <- SS_output(here::here("model", model_name))
dw <- r4ss::tune_comps(
  replist = wtatage, 
  fleet = 10,
  dir = here::here("model", model_name),
  option = "Francis")

#===============================================================================
# Compare Models
#===============================================================================
modelnames <- c(
  "2023 Base", 
  "13. M Prior",
  "14. Age-Based Retention",
  "5-Fixed Discard Catch",
  "5-Weight-at-Age")
mysummary <- SSsummarize(list(model_2023, m_prior, age_based_ret, fix_catch, wtatage))
SSplotComparisons(mysummary,
                  filenameprefix = "5.1_",
                  legendlabels = modelnames, 	
                  ylimAdj = 1.25,
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_plots"),
                  pdf = TRUE)

#===============================================================================
# 5.2 Change CAAL to marginals WCGBT
#===============================================================================

model_name <- "5.2_wcgbt_marginals"

dat <- SS_readdat(file = here::here("model", model_name, dat_file))
agecomp <- dplyr::bind_rows(
  dat$agecomp |>
    dplyr::filter(fleet %in% 1:9) |>
    dplyr::mutate(part = 0), 
  dat$agecomp |>
    dplyr::filter(fleet == -10) |>
    dplyr::mutate(fleet = 10)
)
dat$agecomp <- agecomp
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", model_name, dat_file),
  overwrite = TRUE)

remove_caal <- SS_output(here::here("model", model_name))
dw <- r4ss::tune_comps(
  replist = remove_caal, 
  fleet = 1:9,
  dir = here::here("model", model_name),
  option = "Francis")

#===============================================================================
# 5.3 Remove WCGBT len
#===============================================================================

model_name <- "5.3_remove_wcgbt_len"

dat <- SS_readdat(file = here::here("model", model_name, dat_file))
lencomp <- dplyr::bind_rows(
  dat$lencomp |>
    dplyr::filter(fleet != 10) |>
    dplyr::mutate(part = 0), 
  dat$lencomp |>
    dplyr::filter(fleet == 10) |>
    dplyr::mutate(fleet = -10)
)
dat$lencomp <- lencomp
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", model_name, dat_file),
  overwrite = TRUE)

remove_len <- SS_output(here::here("model", model_name))
dw <- r4ss::tune_comps(
  replist = remove_len, 
  dir = here::here("model", model_name),
  option = "Francis")
# Rerun the model with updated data weights
#factor fleet New_Var_adj hash Old_Var_adj New_Francis   New_MI Francis_mult Francis_lo Francis_hi  MI_mult Type         Name Note
# 1        5     1    0.148241    #    0.169524    0.148241 0.042451     0.874455   0.565148   1.704292 0.250411  age          TWL     
# 2        5     2    0.618678    #    0.624502    0.618678 0.069626     0.990674   0.725995   1.511450 0.111491  age          HKL     
# 3        5     3    0.128228    #    0.119210    0.128228 0.390071     1.075646   0.708009   2.373488 3.272131  age          Pot     
# 4        5     4    0.323733    #    0.354678    0.323733 0.041691     0.912753   0.578172   1.895335 0.117546  age TWL_Discards     
# 5        5     5    0.079562    #    0.086883    0.079562 0.103032     0.915737   0.638043   1.844330 1.185867  age HKL_Discards     
# 6        5     6    0.108909    #    0.119801    0.108909 0.115177     0.909082   0.614074   1.786469 0.961401  age  Pot_Discard     
# 7        5     7    2.596135    #    2.270030    2.596135 0.023560     1.143657   0.628777   5.570683 0.010379  age    Triennial     
# 8        5     8    0.087685    #    0.110421    0.087685 0.013067     0.794099   0.511560   3.199631 0.118334  age  AKFSC_SLOPE     
# 9        5     9    0.103209    #    0.121479    0.103209 0.026092     0.849601   0.544707   2.885157 0.214783  age  NWFSC_Slope     
# 10       5    10    0.059075    #    0.069307    0.059075 0.005760     0.852367   0.589623   1.413672 0.083107  age        WCGBT
remove_len <- SS_output(here::here("model", model_name))
SS_plots(remove_len)

#===============================================================================
# Compare Models
#===============================================================================
modelnames <- c(
  "2023 Base", 
  "13. M Prior",
  "14. Age-Based Retention",
  "5-Fixed Discard Catch",
  "5-Weight-at-Age",
  "- Remove WCGBT CAAL",
  "- Remove WCGBT Lengths + DW"
  )
mysummary <- SSsummarize(list(model_2023, m_prior, age_based_ret, fix_catch, wtatage, remove_caal, remove_len))
SSplotComparisons(mysummary,
                  filenameprefix = "5.3_",
                  legendlabels = modelnames, 	
                  ylimAdj = 1.5,
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_plots"),
                  pdf = TRUE)

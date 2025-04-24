library(r4ss)

dat_file <- "2025_sablefish_dat.ss"
ctl_file <- "2025_sablefish_ctl.ss"

model_2023 <- SS_output(here::here("model", "_bridging", "0_2023_model"))
m_prior <- SS_output(here::here("model", "_bridging", "13_m_prior"))

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
  "0-Init Model")
mysummary <- SSsummarize(list(model_2023, m_prior, model_0))
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
# Data Weight
#===============================================================================

model_name <- ""

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

wcgbt_dw <- SS_output(here::here("model", model_name))

modelnames <- c(
  "2023 Base", 
  "13. M Prior",
  "0-Init Model",
  "1.5-Fishery Selectivity", 
  "1.7-WCGBT Spline")
mysummary <- SSsummarize(
  list(model_2023, m_prior, model_0, data_weight, wcgbt_dw))
SSplotComparisons(mysummary,
                  filenameprefix = "1.7_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_plots"),
                  pdf = TRUE)




SSplotComps(
  replist = wcgbt,
  kind = "AGE", 
  subplots = 1,
  fleets = 1,
  datonly = TRUE
)

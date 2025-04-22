library(r4ss)

dat_name <- "2019_sablefish_dat.ss"
ctl_name <- "2019_sablefish_ctl.ss"

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
# LL trawl ages model 0 = 219.917

model_name <- "1.0_selex_trawl"
dir.create(here::here("model", model_name))
files <- list.files(here::here("model", old_dir))
copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = model_name)

selex_twl <- SS_output(here::here("model", model_name))
# add male offset
# LL trawl ages = 214.99

plot_year_selex(
    replist = selex_twl,
    fleets = 1,
    year = 2024)
  
model_mod <- selex_twl
model_mod$agedbase <- model_mod$agedbase |> dplyr::filter(Sexes == 3)
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
# LL trawl ages = 214.99

plot_year_selex(
  replist = spline_twl,
  fleets = 1,
  year = 2024)

model_mod <- spline_twl
model_mod$agedbase <- model_mod$agedbase |> dplyr::filter(Sexes == 3)
SS_plots(model_mod, plot = c(17))

SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  fleets = 1
)

old_dir <- model__name
library(nwfscDiag)

devtools::load_all("C:/Users/chantel.wetzel/Documents/github/nwfscDiag")
path <- here::here("model", "_discard_fleets", "watage")
get <- get_settings_profile(
  parameters = c("NatM_uniform_Fem_GP_1",  "SR_BH_steep", "SR_LN(R0)"),
  low = c(0.06, 0.50, -0.35),
  high = c(0.11, 0.95, 0.35),
  step_size = c(0.005, 0.05, 0.05),
  param_space = c("real", "real", "relative")
)
model_settings <- get_settings(
  mydir = path,
  settings = list(
    base_name = "8.33_enviro_index",
    profile_details = get,
    Njitter = 100,
    jitter_fraction = 0.03,
    retro_yrs = -1:-10
  )
)
run_diagnostics(mydir = path, model_settings = model_settings)


path <- here::here("model", "_discard_fleets", "growth")
get <- get_settings_profile(
  parameters = c("NatM_uniform_Fem_GP_1",  "SR_BH_steep", "SR_LN(R0)"),
  low = c(0.06, 0.50, -0.35),
  high = c(0.11, 0.95, 0.35),
  step_size = c(0.005, 0.05, 0.05),
  param_space = c("real", "real", "relative")
)
model_settings <- get_settings(
  mydir = path,
  settings = list(
    base_name = "9.22_enviro_index",
    profile_details = get,
    Njitter = 100,
    jitter_fraction = 0.03,
    retro_yrs = -1:-10
  )
)
run_diagnostics(mydir = path, model_settings = model_settings)

run_mcmc_diagnostics(
  dir_wd = here::here("model", "_discard_fleets",  "watage", "8.33_data_weight_rec_dev_2"),
  model = "ss3",
  extension = ".exe",
  hour = 8,
  iter = 10000,
  chains = 2,
  interactive = FALSE,
  verbose = FALSE
)

run_mcmc_diagnostics(
  dir_wd = here::here("model", "_discard_fleets", "growth", "9.21_data_weight_rec_dev_2"),
  model = "ss3",
  extension = ".exe",
  hour = 8,
  thin = 2,
  iter = 10000,
  chains = 2,
  interactive = FALSE,
  verbose = FALSE
)


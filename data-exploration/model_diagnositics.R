library(nwfscDiag)

path <- here::here("model", "_discard_fleets", "watage")
get <- nwfscDiag::get_settings_profile(
  parameters = c("NatM_uniform_Fem_GP_1",  "SR_BH_steep", "SR_LN(R0)"),
  low = c(0.06, 0.50, -0.75),
  high = c(0.11, 0.95, 0.75),
  step_size = c(0.005, 0.05, 0.10),
  param_space = c("real", "real", "relative"))
model_settings <- nwfscDiag::get_settings(
  mydir = path,
  settings = list(
    base_name = "8.36_base_model",
    profile_details = get,
    Njitter = 100,
    jitter_fraction = 0.05,
    retro_yrs = -1:-10
  )
)
nwfscDiag::run_diagnostics(mydir = path, model_settings = model_settings)

# MCMC Diagnostic
nwfscDiag::run_mcmc_diagnostics(
  dir_wd = here::here("model", "_discard_fleets",  "watage", "8.36_base_model"),
  model = "ss3",
  extension = ".exe",
  hour = 12,
  iter = 10000,
  thin = 2,
  chains = 2,
  interactive = FALSE,
  verbose = FALSE
)


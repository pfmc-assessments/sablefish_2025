library(nwfscDiag)
devtools::load_all("C:/Users/chantel.wetzel/Documents/github/nwfscDiag")
path <- here::here("model", "_discard_fleets", "growth")

get <- get_settings_profile(
  parameters = c("NatM_uniform_Fem_GP_1",  "SR_BH_steep", "SR_LN(R0)"),
  low = c(0.06, 0.50, -0.25),
  high = c(0.10, 0.95, 0.25),
  step_size = c(0.005, 0.05, 0.05),
  param_space = c("real", "real", "relative")
)

model_settings <- get_settings(
  mydir = path,
  settings = list(
    base_name = "6.3_slope_selex_2011_dw",
    profile_details = get,
    Njitter = 50
  )
)
Sys.sleep(60*90)
run_diagnostics(mydir = path, model_settings = model_settings)


run_mcmc_diagnostics(
  dir_wd = here::here("model", "_discard_fleets", "growth", "6.1_no_early_devs_rec_dev_2"),
  model = "ss3",
  extension = ".exe",
  hour = 4,
  iter = 4000,
  chains = 2,
  interactive = FALSE,
  verbose = FALSE
)
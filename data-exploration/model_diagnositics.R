library(nwfscDiag)
Sys.sleep(60*10)
devtools::load_all("C:/Users/chantel.wetzel/Documents/github/nwfscDiag")
path <- here::here("model", "_discard_fleets", "growth")
#path <- here::here("model", "_discard_fleets", "watage")

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
    base_name = "9.11_fix_beta",
    profile_details = get,
    Njitter = 50
  )
)
run_diagnostics(mydir = path, model_settings = model_settings)


run_mcmc_diagnostics(
  dir_wd = here::here("model", "_discard_fleets", "growth", "7.9_triennial_block_width_final_extra_sd_dw_mle_rec_dev_2"),
  model = "ss3",
  extension = ".exe",
  hour = 1,
  iter = 10000,
  chains = 2,
  interactive = FALSE,
  verbose = FALSE
)

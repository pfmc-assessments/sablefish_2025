library(nwfscDiag)
path <- here::here("model")
get <- get_settings_profile(
  parameters = c("NatM_uniform_Fem_GP_1", "NatM_uniform_Mal_GP_1", "SR_BH_steep", "SR_LN(R0)"),
  low = c(0.05, 0.05, 0.50, -0.25),
  high = c(0.10, 0.10, 0.95, 0.25),
  step_size = c(0.01, 0.01, 0.05, 0.10),
  param_space = c("real", "real", "real", "relative")
)

model_settings <- get_settings(
  mydir = path,
  settings = list(
    base_name = "2.1_selex_trawl_double_normal_sex_w_block_split_fixed",
    profile_details = get,
    Njitter = 20
  )
)
run_diagnostics(mydir = path, model_settings = model_settings)

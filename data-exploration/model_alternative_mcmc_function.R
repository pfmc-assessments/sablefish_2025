
m = "ss3"
p = here::here("model", "_discard_fleets", "growth", "7.9_triennial_block_width_final_extra_sd_dw_mle_rec_dev_2")
input <- list()
iter <- 2000
chains = 2
fit_model <- adnuts::sample_nuts(
  model = m, 
  path = p,  
  iter = iter,
  warmup = 0.25*iter, 
  chains = chains, 
  cores = 4,
  control = list(metric='mle', max_treedepth=5),
  mceval = TRUE)

library(r4ss)
discard_growth <- SS_output(here::here("model", "_discard_fleets", "growth", "7.8_growth_sd_log"))
discard_watage <- SS_output(here::here("model", "_discard_fleets", "watage", "6.1_rec_devs"))

ret_growth <- SS_output(here::here("model", "_retention_model", "_growth", "21.1_no_early_devs"))
ret_watage <- SS_output(here::here("model", "_retention_model", "_weight_at_age", "20.0_data_at_sea_ages _dw"))

modelnames <- c(
  "Discard Fleets - Growth (7.8)",
  "Discard Fleets - Weight-at-Age (6.1)",
  "Retention Fleets - Growth (21.1)",
  "Retention Fleets = Weight-at-Age (20.0)")
mysummary <- SSsummarize(list(
  discard_growth,
  discard_watage,
  ret_growth,
  ret_watage))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_and_ret_05152025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.35,
  pdf = TRUE)


discard_growth <- SS_output(here::here("model", "_discard_fleets", "growth", "7.9_triennial_block_width_final_extra_sd_dw"))
ret_growth <- SS_output(here::here("model", "_retention_model", "_growth", "21.6_growth_sd_log_dw"))

modelnames <- c(
  "Discard Fleets - Growth (7.9)",
  "Retention Fleets - Growth (21.6)")
mysummary <- SSsummarize(list(
  discard_growth,
  ret_growth))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_and_ret_05162025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.35,
  pdf = TRUE)


discard_growth <- SS_output(here::here("model", "_discard_fleets", "growth", "7.9_triennial_block_width_final_extra_sd_dw_mle"))
ret_growth <- SS_output(here::here("model", "_retention_model", "_growth", "21.6_growth_sd_log_dw"))
ret_growth_early_devs <- SS_output(here::here("model", "_retention_model", "_growth", "21.9_hkl_pot_ret_blocks"))

modelnames <- c(
  "Discard Fleets - Growth (7.9)",
  "Retention Fleets - Growth (21.6)",
  "Retention Fleets - Growth w/ Early Devs. (21.9)")
mysummary <- SSsummarize(list(
  discard_growth,
  ret_growth))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_and_ret_05162025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.35,
  pdf = TRUE)


#===============================================================================
# Reorganized fleets: split triennial, similar blocks, and no early devs
#===============================================================================

modelnames <- c(
  "Discard Fleets - Growth (8.0)",
  "Retention Fleets - Growth (22.0)")
mysummary <- SSsummarize(list(
  fleet_structure,
  fleet_structure_ret))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_and_ret_05192025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.35,
  pdf = TRUE)

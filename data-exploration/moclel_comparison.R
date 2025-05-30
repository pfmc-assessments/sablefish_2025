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
fleet_structure <- SS_output(here::here("model", "_discard_fleets", "growth", "8.0_revised_fleet_structure_dw"))
fleet_structure_ret <- SS_output(here::here("model", "_retention_model", "_growth", "22.0_revised_fleet_structure_dw"))
watage_discard <- SS_output(here::here("model", "_discard_fleets", "watage", "8.0_revised_fleet_structure_dw"))
modelnames <- c(
  "Discard Fleets - Growth (8.0)",
  "Retention Fleets - Growth (22.0)",
  "Discard Fleets - Weight-at-Age (8.00)")
mysummary <- SSsummarize(list(
  fleet_structure,
  fleet_structure_ret,
  watage_discard))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_and_ret_05202025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.35,
  pdf = TRUE)

#===============================================================================
# Reorganized fleets: split triennial, similar blocks, and no early devs
#===============================================================================
discard_growth_devs <- SS_output(here::here("model", "_discard_fleets", "growth", "8.2_add_early_rec_devs_dw"))
discard_watage_devs <- SS_output(here::here("model", "_discard_fleets", "watage", "8.2_early_devs_dw"))
modelnames <- c(
  "Discard Fleets - Growth (8.2)",
  "Discard Fleets - Weight-at-Age (8.2)")
mysummary <- SSsummarize(list(
  discard_growth_devs,
  discard_watage_devs))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_growth_watage_05202025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.35,
  pdf = TRUE)

#===============================================================================
# Reorganized fleets: split triennial, similar blocks, no early devs, steepness
#===============================================================================
discard_growth <- SS_output(here::here("model", "_discard_fleets", "growth", "9.10_linear_ae_bias_adj_steepness_dw"))
discard_watage <- SS_output(here::here("model", "_discard_fleets", "watage", "8.7_bias_adjustment"))

modelnames <- c(
  "Discard Fleets - Growth (9.10)",
  "Discard Fleets - Weight-at-Age (8.7)")
mysummary <- SSsummarize(list(
  discard_growth,
  discard_watage))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_growth_watage_05222025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.25,
  pdf = TRUE)

model_2019 <- SS_output(here::here("model", "_bridging",  "0_2019_base_model"))
modelnames <- c(
  "2019 Base Model",
  "Discard Fleets - Growth (9.10)",
  "Discard Fleets - Weight-at-Age (8.7)")
mysummary <- SSsummarize(list(
  model_2019,
  discard_growth,
  discard_watage))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_growth_watage_05222025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.25,
  pdf = TRUE)

model_2019 <- SS_output(here::here("model", "_bridging",  "0_2019_base_model"))
model_2023 <- SS_output(here::here("model", "_bridging",  "0_2023_model"))
discard_watage <- SS_output(here::here("model", "_discard_fleets", "watage", "8.25_data_weight"))
discard_growth <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.18_match_watage_8.25"))
modelnames <- c(
  "2019 Base Model",
  "2023 Base Model",
  "Discard Fleets - Growth (9.18)",
  "Discard Fleets - Weight-at-Age (8.25)")
mysummary <- SSsummarize(list(
  model_2019,
  model_2023,
  discard_growth,
  discard_watage))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_growth_watage_05282025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.25,
  pdf = TRUE)
modelnames <- c(
  "Discard Fleets - Growth (9.18)",
  "Discard Fleets - Weight-at-Age (8.25)")
mysummary <- SSsummarize(list(
  discard_growth,
  discard_watage))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_growth_watage_05282025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.25,
  pdf = TRUE)

data_weight_growth <- SS_output(here::here("model", "_discard_fleets", "growth", "9.20_remove_fixed_disc_desc_blocks"))
data_weight_wtatage <- SS_output(here::here("model", "_discard_fleets", "watage", "8.32_remove_disc_desc_block_hkl_pot"))
data_weight_growth_dev <- SS_output(here::here("model", "_discard_fleets", "growth", "9.20_remove_fixed_disc_desc_blocks_rec_dev_2"))
data_weight_wtatage_dev <- SS_output(here::here("model", "_discard_fleets", "watage", "8.32_remove_disc_desc_block_hkl_pot_rec_dev_2"))
modelnames <- c(
  "Discard Fleets - Growth (9.20) Rec. Dev = 1",
  "Discard Fleets - Growth (9.20) Rec. Dev = 2",
  "Discard Fleets - Weight-at-Age (8.33) Rec. Dev. = 1",
  "Discard Fleets - Weight-at-Age (8.33) Rec. Dev. = 2")
mysummary <- SSsummarize(list(
  data_weight_growth,
  data_weight_growth_dev,
  data_weight_wtatage,
  data_weight_wtatage_dev))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_growth_watage_05302025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.25,
  pdf = TRUE)


data_weight_growth <- SS_output(here::here("model", "_discard_fleets", "growth", "9.21_data_weight"))
data_weight_wtatage <- SS_output(here::here("model", "_discard_fleets", "watage", "8.33_data_weight"))
modelnames <- c(
  "Discard Fleets - Growth (9.21)",
  "Discard Fleets - Weight-at-Age (8.33)")
mysummary <- SSsummarize(list(
  data_weight_growth,
  data_weight_wtatage))
SSplotComparisons(
  mysummary,
  filenameprefix = "compare_discard_growth_watage_05302025_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_plots"),
  ylimAdj = 1.25,
  pdf = TRUE)

library(r4ss)

model_2023 <- SS_output(here::here("model", "_bridging", "0_2023_model"))
exe <- SS_output(here::here("model", "_bridging", "0_exe"))
fleet_numbering <- SS_output(here::here("model", "_bridging", "1_fleet_numbering"))
rm_enviro <- SS_output(here::here("model", "_bridging", "2_rm_enviro"))
add_landings <- SS_output(here::here("model", "_bridging", "3_landings"))
add_fishery_ages_all <- SS_output(here::here("model", "_bridging", "4.3_fishery_ages_all_years"))
add_discard_rates <- SS_output(here::here("model", "_bridging", "5.1_discard_rate_data"))
add_discard_len <- SS_output(here::here("model", "_bridging", "5.3_discard_lengths"))
split_fleets <- SS_output(here::here("model", "_bridging", "6_split_fixed_gears"))
blocks <- SS_output(here::here("model", "_bridging", "7_fix_blocks"))
triennial <- SS_output(here::here("model", "_bridging", "8_triennial"))
akfsc_slope <- SS_output(here::here("model", "_bridging", "9_akfsc_slope"))
nwfsc_slope <- SS_output(here::here("model", "_bridging", "10_nwfsc_slope"))
wcgbt <- SS_output(here::here("model", "_bridging", "11_wcgbt"))
maturity <- SS_output(here::here("model", "_bridging", "12_maturity"))
m_prior <- SS_output(here::here("model", "_bridging", "13_m_prior_mle"))
ageing_error <- SS_output(here::here("model", "_bridging", "14_ageing_error"))

# Structure
main_rec_devs <- SS_output(here::here("model", "_bridging", "15_rec_devs_main"))
early_rec_devs <- SS_output(here::here("model", "_bridging", "16_no_early_devs"))
remove_afsc_slope <- SS_output(here::here("model", "_bridging", "17_afsc_slope"))
single_m <- SS_output(here::here("model", "_bridging", "18_single_m"))
foreign_fleets_age_ret <- SS_output(here::here("model", "_retention_model", "_growth", "21.3_input_n_mirror_selex_only"))
split_triennial_age_ret <- SS_output(here::here("model", "_retention_model", "_growth", "21.4_split_tri"))
fix_wcgbt_selex <- SS_output(here::here("model", "_retention_model", "_growth", "21.5_wcgbt_selex"))
adjust_selex_blocks <- SS_output(here::here("model", "_retention_model", "_growth", "22.0_revised_fleet_structure"))
discard_fleets <- SS_output(here::here("model", "_discard_fleets", "growth", "9.19_no_extra_sd"))
steepness <- SS_output(here::here("model", "_discard_fleets", "growth", "9.19_no_extra_sd_steepness=0.70")) # the steepness run has sex-specific M values
watage <- SS_output(here::here("model", "_discard_fleets", "watage", "8.33_data_weight")) 
add_enviro <- SS_output(here::here("model", "_discard_fleets", "watage", "8.34_fix_additional_selex_param")) 
rec_option_2 <- SS_output(here::here("model", "_discard_fleets", "watage", "8.36_base_model")) 

# Executable
modelnames <- c(
  "2023 Base", 
  "Update SS3 Version")
mysummary <- SSsummarize(list(
  model_2023, 
  exe))
SSplotComparisons(mysummary,
                  subplots = c(2, 4),
                  filenameprefix = "0_bridging_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  print = TRUE,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.0)

# Data
modelnames <- c(
  "2023 Base", 
  "- Remove Enviro. Index",
  "+ Landings",
  "+ Fishery Ages",
  "+ Discard Rates",
  "+ Discard Weights & Lengths",
  "+ Split Fixed Gear")
mysummary <- SSsummarize(list(
  model_2023, 
  rm_enviro, 
  add_landings, 
  add_fishery_ages_all, 
  add_discard_rates, 
  add_discard_len,
  split_fleets))
SSplotComparisons(mysummary,
                  filenameprefix = "1_data_bridging_",
                  subplots = 19,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)

SSplotComparisons(mysummary,
                  filenameprefix = "1_data_bridging_",
                  subplots = 4,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)

modelnames <- c(
  "Split Fixed Gear",
  "+ Revise Blocks",
  "+ Update Historical Surveys",
  "+ WCGBT Survey",
  "+ Maturity",
  "+ M Prior",
  "+ Ageing Error")
mysummary <- SSsummarize(list(
  split_fleets,
  blocks,
  nwfsc_slope,
  wcgbt,
  maturity,
  m_prior,
  ageing_error))
SSplotComparisons(mysummary,
                  filenameprefix = "2_data_bridging_",
                  subplots = 18,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)

SSplotComparisons(mysummary,
                  filenameprefix = "2_data_bridging_",
                  subplots = 3,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)


modelnames <- c(
  "Ageing Error",
  "Adj. Main Rec. Dev. Period",
  "Adj. Main Period and No Early Rec. Devs.")
mysummary <- SSsummarize(list(
  ageing_error,
  main_rec_devs,
  early_rec_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "3_structure_",
                  subplots = 18,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)

SSplotComparisons(mysummary,
                  filenameprefix = "3_structure_",
                  subplots = 3,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)


modelnames <- c(
  "Adj. Main Period and No Early Rec. Devs.", 
  "Remove AFSC Slope Survey",
  "Female & Male Natural Mortality Equal",
  "Add Foreign Fleets with Age-Based Retention")
mysummary <- SSsummarize(list(
  early_rec_devs,
  remove_afsc_slope,
  single_m,
  foreign_fleets_age_ret))
SSplotComparisons(mysummary,
                  filenameprefix = "4_structure_",
                  subplots = 18,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)

SSplotComparisons(mysummary,
                  filenameprefix = "4_structure_",
                  subplots = 3,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)

modelnames <- c(
  "Foreign Fleets with Age-Based Retention",
  "Split Triennial Survey",
  "Revise WCGBT Selectivity",
  "Adjust Fishery Selectivity Blocks")
mysummary <- SSsummarize(list(
  foreign_fleets_age_ret,
  split_triennial_age_ret,
  fix_wcgbt_selex,
  adjust_selex_blocks))
SSplotComparisons(mysummary,
                  filenameprefix = "5_structure_",
                  subplots = 18,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)

SSplotComparisons(mysummary,
                  filenameprefix = "5_structure_",
                  subplots = 3,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)

modelnames <- c(
  "Adjust Selectivity Blocks",
  "Add Discard Fleets",
  "Steepness = 0.75",
  "Add Weight-at-Age",
  "Add Environmental Index",
  "Simple Rec. Deviations")
mysummary <- SSsummarize(list(
  adjust_selex_blocks,
  discard_fleets,
  steepness,
  watage,
  add_enviro,
  rec_option_2))
SSplotComparisons(mysummary,
                  filenameprefix = "6_structure_",
                  subplots = 19,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)

SSplotComparisons(mysummary,
                  filenameprefix = "6_structure_",
                  subplots = 4,
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("report", "figures"),
                  ylimAdj = 1.25,
                  print = TRUE)


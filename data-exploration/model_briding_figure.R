library(r4ss)

model_2023 <- SS_output(here::here("model", "_bridging", "0_2023_model"))
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
m_prior <- SS_output(here::here("model", "_bridging", "13_m_prior"))
age_based <- SS_output(here::here("model", "_bridging", "14m_fix_survey_selex_params"))
remove_discard_weights <- SS_output(here::here("model", "_bridging", "15_remove_discard_weights"))
add_catch_fleets_alt <- SS_output(here::here("model", "_bridging", "16_catch_fleets_w_wcgbt_len"))
selex_bounds <- SS_output(here::here("model", "_bridging", "17_fix_selex_bounds_fix_cv"))
male_selex <- SS_output(here::here("model", "_bridging", "18_fix_male_selex_w_wcgbt_len"))
rec_devs <- SS_output(here::here("model", "_bridging", "19.1_rec_devs_early_main_wcgbt_len"))
ageing_error <- SS_output(here::here("model", "_bridging", "20.0_data_ageing_error_atsea_catch"))
single_m <- SS_output(here::here("model", "_bridging", "20.3_param_single_m"))
data_weight <- SS_output(here::here("model", "_bridging", "20.4_data_weight_hessian"))
discard_input_n <- SS_output(here::here("model", "_bridging", "20.6_reduce_discard_input_n"))


modelnames <- c(
  "2023 Base", 
  "- Remove Enviro. Index",
  "+ Landings",
  "+ Fishery Ages",
  "+ Discard Rates",
  "+ Discard Weights & Lengths")
mysummary <- SSsummarize(list(
  model_2023, 
  rm_enviro, 
  add_landings, 
  add_fishery_ages_all, 
  add_discard_rates, 
  add_discard_len))
SSplotComparisons(mysummary,
                  filenameprefix = "1_bridging_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

modelnames <- c(
  "Discard Data",
  "+ Split Fixed Gear",
  "+ Revise Blocks", 
  "+ Update Historical Surveys",
  "+ WCGBT Survey")
mysummary <- SSsummarize(list(
  add_discard_len,
  split_fleets, 
  blocks,
  nwfsc_slope,
  wcgbt))
SSplotComparisons(mysummary,
                  filenameprefix = "2_bridging_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

modelnames <- c(
  "WCGBT Survey",
  "+ Age-Based Maturity",
  "+ M Prior", 
  "+ Age-Based Retention",
  "+ Add Catch Fleets")
mysummary <- SSsummarize(list(
  wcgbt,
  maturity,
  m_prior, 
  remove_discard_weights,
  add_catch_fleets_alt))
SSplotComparisons(mysummary,
                  filenameprefix = "3_bridging_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

modelnames <- c(
  "Add Catch Fleets",
  "+ Male Selectivity", 
  "+ Recruitment Deviations", 
  "+ Ageing Error",
  "+ Single M",
  "+ Reduce Input N Discards")
mysummary <- SSsummarize(list(
  add_catch_fleets_alt,
  male_selex,
  rec_devs, 
  ageing_error,
  single_m,
  discard_input_n))
SSplotComparisons(mysummary,
                  filenameprefix = "4_bridging_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
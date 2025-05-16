library(r4ss)
#===============================================================================
# Retention Model - Growth
#===============================================================================

ages <- SS_output(here::here("model", "_retention_model", "_growth", "20.5_data_add_ages"))
remove_afsc_slope <- SS_output(here::here("model", "_retention_model", "_growth", "21.0_remove_afsc_slope"))
rec_devs <- SS_output(here::here("model", "_retention_model", "_growth", "21.1_no_early_devs"))
# gradient =  0.402543
# NLL = 1990.55
# R0 = 10.1246
# Q = Triennial 0.476063 and 1.43738, NWFSC Slope = 0.378389, WCGBT =  0.774101
SS_plots(rec_devs)

modelnames <- c(
  "20.5 Add Ages",
  "21.0 Remove AFSC Slope",
  "21.1 No Early Rec. Devs.")
mysummary <- SSsummarize(list(
  ages, 
  remove_afsc_slope,
  rec_devs ))
SSplotComparisons(mysummary,
                  filenameprefix = "20.0-21.1_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_retention_model", "_growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

r4ss::tune_comps(
  replist = rec_devs , 
  dir = here::here("model", "_retention_model", "_growth", "21.1_no_early_devs"),
  option = "Francis")


# Mirror HKL and Pot
mirror_fixed_gear <- SS_output(here::here("model", "_retention_model", "_growth", "21.2_mirror_hkl_pot"))
# gradient =  0.0327414 (vs. 0.402543)
# NLL = 2020.36  (vs. 1990.55)
# R0 = 10.2187 (vs. 10.1246)
SS_plots(mirror_fixed_gear)


modelnames <- c(
  "20.5 Add Ages",
  "21.0 Remove AFSC Slope",
  "21.1 No Early Rec. Devs.",
  "22.2 Mirror HKL & Pot")
mysummary <- SSsummarize(list(
  ages, 
  remove_afsc_slope,
  rec_devs,
  mirror_fixed_gear))
SSplotComparisons(mysummary,
                  filenameprefix = "20.0-21.2_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_retention_model", "_growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

r4ss::tune_comps(
  replist = mirror_fixed_gear, 
  dir = here::here("model", "_retention_model", "_growth", "21.2_mirror_hkl_pot"),
  option = "Francis")

#===============================================================================
# Reduce discard input sample size for hkl and pot
#===============================================================================
discard_input_n <- SS_output(here::here("model", "_retention_model", "_growth", "21.3_input_n"))
# gradient = 0.0287488
# NLL = 1767.06
# R0 = 10.2698 
SS_plots(discard_input_n)

mirror_selex_only_input_n <- SS_output(here::here("model", "_retention_model", "_growth", "21.3_input_n_mirror_selex_only"))
# gradient = 0.0395352
# NLL = 1731.37
# R0 = 10.2413
plot_fleet_selectivity(model_out = mirror_selex_only_input_n, fleet_num = 2)
plot_fleet_selectivity(model_out = mirror_selex_only_input_n, fleet_num = 3)
plot_fleet_retention(model_out = mirror_selex_only_input_n, fleet_num = 2)
plot_fleet_retention(model_out = mirror_selex_only_input_n, fleet_num = 3)

#===============================================================================
# Hard split the triennial survey
#===============================================================================
split_tri <- SS_output(here::here("model", "_retention_model", "_growth", "21.4_split_tri"))
plot_ghostfleets(replist = split_tri)
plot_age_fits_sexed_only(replist = split_tri)
SS_plots(split_tri)

# gradient = 0.788073
# NLL = 1704.48
# R0 = 10.2325
# Triennial Q = 0.925387 and 1.0684
# NWFSC Slope Q = 0.306682
# WCGBT Q = 0.815012

#===============================================================================
# Adjust WCGBT selectivitity based on discard fleets growth 7.6_wcgbt_selex_double_normal
#===============================================================================
wcgbt_selex <- SS_output(here::here("model", "_retention_model", "_growth", "21.5_wcgbt_selex"))
# gradient = 0.0183245
# NLL = 1711.06
# R0 = 10.0969
# Triennial Q = 1.14185 and 1.38971
# NWFSC Slope Q = 0.487124
# WCGBT Q = 0.92973
# While this results in a slightly worse fit to the data the Lmax for females 
# increases to 60.993
# did not invert hessian: param 85 (asc selex hkl)

#===============================================================================
# log(sd) for the growth CV
#===============================================================================
growth_sd_log <- SS_output(here::here("model", "_retention_model", "_growth", "21.6_growth_sd_log"))
# gradient = 0.000913842
# NLL = 1712.86
# R0 = 10.0924
SS_plots(growth_sd_log)
plot_ghostfleets(replist = growth_sd_log)
plot_age_fits_sexed_only(replist = growth_sd_log)


r4ss::tune_comps(
  replist = growth_sd_log, 
  dir = here::here("model", "_retention_model", "_growth", "21.6_growth_sd_log"),
  option = "Francis")

modelnames <- c(
  "21.2 Mirror HKL & Pot",
  "21.3 Reduce Discard Input N",
  "21.4 Split Triennial", 
  "21.5 WCGBT Selectivity",
  "21.6 Growth Log SD")
mysummary <- SSsummarize(list(
  mirror_fixed_gear,
  discard_input_n,
  split_tri,
  wcgbt_selex,
  growth_sd_log))
SSplotComparisons(mysummary,
                  filenameprefix = "21.2-6_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_retention_model", "_growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# log(sd) for the growth CV - data weighted w/ hessian
#===============================================================================
growth_sd_log_dw <- SS_output(here::here("model", "_retention_model", "_growth", "21.6_growth_sd_log_dw"))
# did not invert hessian: param 84 (peak selex hkl)
plot_ghostfleets(replist = growth_sd_log_dw)
plot_age_fits_sexed_only(replist = growth_sd_log_dw)
SS_plots(growth_sd_log_dw)
# gradient = 0.00076822
# NLL = 1601.34
# R0 = 10.0616

#===============================================================================
# Turn on early rec. devs
#===============================================================================
early_devs <- SS_output(here::here("model", "_retention_model", "_growth", "21.7_early_devs"))
# gradient = 0.000684109
# NLL = 1596.97
# R0 = 10.0271

modelnames <- c(
  "21.2 Mirror HKL & Pot",
  "21.3 Reduce Discard Input N",
  "21.4 Split Triennial", 
  "21.5 WCGBT Selectivity",
  "21.6 Growth Log SD - Data Weighted",
  "21.7 Add Early Rec. Devs.")
mysummary <- SSsummarize(list(
  mirror_fixed_gear,
  discard_input_n,
  split_tri,
  wcgbt_selex,
  growth_sd_log_dw,
  early_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "21.2-7_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_retention_model", "_growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# Mirror only selectivity between hkl and pot
#===============================================================================
early_devs <- SS_output(here::here("model", "_retention_model", "_growth", "21.7_early_devs"))


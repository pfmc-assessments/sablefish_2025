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
# gradient =  0.0554209
# NLL = 1.61698e+06  <----- ZOINK
# R0 = 10.6457
SS_plots(mirror_fixed_gear)
# The negative fleet mirroring for discard and retention does not appear to be working
# Just mirroring the selectivity and not the retention results in a 
# NLL = 2019.29 vs. 1990.55 (rec devs model)

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
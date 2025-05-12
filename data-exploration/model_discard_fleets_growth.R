

#===============================================================================
# Discard Fleet Growth Models
#===============================================================================
growth <- SS_output(here::here("model", "_discard_fleets", "growth", "5.0_fix_discard_catch"))
# gradiant = 0.00344382
# NLL = 2884.02
# R0 = 9.35185
# M f = 0.0605897; M m = 0.0643177
# Linf f = 59.6528
# This model has the WCGBT selectivity nearly asymptotic  with high init selex

data_update <- SS_output(here::here("model", "_discard_fleets", "growth", "5.1_at_sea_ages"))
# gradient = 0.0503511
# NLL = 3087.78
# R0 = 9.27672

ageing_error <- SS_output(here::here("model", "_discard_fleets", "growth", "5.2_at_sea_ages_ageing_error"))
# gradient = 0.0134919
# NLL = 2937.13
# R0 = 9.57716

single_m <- SS_output(here::here("model", "_discard_fleets", "growth", "5.3_single_m"))
# gradient = 0.0379836
# NLL = 2938.66 LESS THAN 2 LIKELIHOOD UNIT CHANGE FROM PREVIOUS MODEL 
# R0 = 9.60297

wcgbt_sd <- SS_output(here::here("model", "_discard_fleets", "growth", "5.4_add_wcgbt_sd"))
# gradient = 0.0372931
# NLL = 2907.21 (11 LL improvement in the age data, similar LL for the WCGBT survey but large improvement in the NWFSC Slope)
# R0 = 9.56958

main_dev_start <- SS_output(here::here("model", "_discard_fleets", "growth", "5.5_main_dev_1975"))
# Compare with the single M model above
# gradient =  0.00141939
# NLL = 2945.01
# R0 = 9.9654

main_dev_start_1980 <- SS_output(here::here("model", "_discard_fleets", "growth", "5.5_main_dev_1980"))
main_dev_start_1970 <- SS_output(here::here("model", "_discard_fleets", "growth", "5.5_main_dev_1970"))

no_early_devs <- SS_output(here::here("model", "_discard_fleets", "growth", "5.5_no_early_devs"))
# Compare with the single M model above
# gradient =  0.00177944
# NLL = 2960
# R0 = 9.9439
plot_ghostfleets(replist = no_early_devs)
SS_plots(no_early_devs)

modelnames <- c(
  "5.0 Discard Fleet",
  "5.1 At-Sea Catch & New Ages",
  "5.2 Ageing Error", 
  "5.3 Single M",
  "5.4 Add SD WCGBT",
  "5.3 + Main Devs. 1976",
  "5.3 + No Early Devs. & Main 1976")
mysummary <- SSsummarize(list(
  growth, 
  data_update,
  ageing_error,
  single_m,
  wcgbt_sd,
  main_dev_start, 
  no_early_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "5.1-4_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)


modelnames <- c(
  "5.3 Single M",
  "5.3 + Main Devs. 1970",
  "5.3 + Main Devs. 1976",
  "5.3 + Main Devs. 1980",
  "5.3 + No Early Devs. & Main 1976")
mysummary <- SSsummarize(list(
  single_m,
  main_dev_start_1970,
  main_dev_start, 
  main_dev_start_1980,
  no_early_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "5.3_dev_options_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# Selex explorations based off of 5.5 no early devs w/ single M
#===============================================================================
no_early_devs <- SS_output(here::here("model", "_discard_fleets", "growth", "6.0_no_early_devs"))
SS_plots(no_early_devs)

# gradient =  0.00177944
# NLL = 2960.36
# NLL WCGBT Ages = 833.304 
# R0 = 9.9439

r4ss::tune_comps(
  replist = no_early_devs , 
  dir = here::here("model", "_discard_fleets", "growth", "6.0_no_early_devs_dw"),
  option = "Francis")
no_early_devs_dw <- SS_output(here::here("model", "_discard_fleets", "growth", "6.0_no_early_devs_dw"))

no_early_devs <- SS_output(here::here("model", "_discard_fleets", "growth", "6.0_no_early_devs_mle_dw"))
SS_plots(no_early_devs, plot = 26)
# gradient =  0.00747457
# NLL = 2724.48
# NLL Ages = 2662.42
# NLL WCGBT Ages = 790.197
# R0 = 9.98118

modelnames <- c(
  "6.0 No Early Devs. Single M",
  "+ Data Weighting",
  "6.0 Jitter MLE")
mysummary <- SSsummarize(list(
  no_early_devs,
  no_early_devs_dw,
  no_early_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "6.0_dw_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

rec_option_2 <- SS_output(here::here("model", "_discard_fleets", "growth", "6.1_no_early_devs_rec_dev_2"))
# NLL = 2956.86
# gradient = 0.00901451

wcgbt_selex <- SS_output(here::here("model", "_discard_fleets", "growth", "6.1_wcgbt_selex"))
SS_plots(wcgbt_selex, plot = 2)
plot_ghostfleets(replist = wcgbt_selex)

wcgbt_selex_2011 <- SS_output(here::here("model", "_discard_fleets", "growth", "6.1_wcgbt_selex_2011"))
SS_plots(wcgbt_selex_2011, plot = c(2, 26))
plot_ghostfleets(replist = wcgbt_selex_2011)
# gradient = 0.416494
# NLL = 3050.66
# DOES NOT FIT THE COMPOSITION DATA AT ALL!!!!!

# Add block on trawl discard for peak and descending limbs
trawl_discard_block <- SS_output(here::here("model", "_discard_fleets", "growth", "6.2_trawl_discard_blocks"))
# NLL = 2670.65
# gradient = 0.0118757
# NLL Age = 2610.39
# R0 = 10.0338

SS_plots(trawl_discard_block, plot = c(2, 26))
plot_year_selex(
  replist = trawl_discard_block,
  fleets = 7,
  year = 1890)

slope_selex_2011 <- SS_output(here::here("model", "_discard_fleets", "growth", "6.3_slope_selex_2011"))
SS_plots(slope_selex_2011)
plot_ghostfleets(replist = slope_selex_2011)
plot_year_selex(
  replist = slope_selex_2011,
  fleets = 7,
  year = 1890)
# NLL = 2536.64
# gradient = 0.277868
# NLL Age = 2474.39
# R0 = 10.0255
r4ss::tune_comps(
  replist = slope_selex_2011, 
  dir = here::here("model", "_discard_fleets", "growth", "6.3_slope_selex_2011"),
  option = "Francis")

# This model is just the data weighted model from 6.3 slope survey selectivity
trawl_slope <- SS_output(here::here("model", "_discard_fleets", "growth", "6.4_trawl_discad_blocks_slope"))


modelnames <- c(
  "6.0 No Early Devs. Single M",
  "6.2 Trawl Discard",
  "6.3 Slope Surveys Selex",
  "6.4 Trawl Discard & Slope Selex")
mysummary <- SSsummarize(list(
  no_early_devs,
  trawl_discard_block,
  slope_selex_2011,
  trawl_slope))
SSplotComparisons(mysummary,
                  filenameprefix = "6.0-4_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

# This model is just the data weighted model from 6.3 slope survey selectivity
trawl_slope_2011_mle <- SS_output(here::here("model", "_discard_fleets", "growth", "6.3_slope_selex_2011_mle_dw"))
SS_plots(trawl_slope_2011_mle)
# 5 parameters on the bounds
# Age_DblN_ascend_se_Pot(3)	-10.0000000	
# Age_DblN_peak_HKL_Discards(5)  19.8746000	<- this can't be correct
# Age_DblN_ascend_se_HKL_Discards(5)	9.9995700	
# Age_DblN_peak_TWL_Discards(4)_BLK2repl_2011	0.0752342	
# Age_DblN_descend_se_TWL_Discards(4)_BLK2repl_1890	-10.0000000	

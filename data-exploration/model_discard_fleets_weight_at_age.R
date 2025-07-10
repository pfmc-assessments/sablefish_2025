#===============================================================================
# Discard Fleet - Weight-at-Age
#===============================================================================
watage_discard <- SS_output(here::here("model", "_discard_fleets", "watage", "8.0_revised_fleet_structure"))
# gradiant = 0.0195772
# NLL = 1905.78
# R0 = 9.9296
# M = 0.0859982
# Triennial Q = 1.16822 and 1.29164 
# NWFSC Slope Q =  0.518269
# WCGBT Q = 1.29164

r4ss::tune_comps(
  replist = watage_discard, 
  dir = here::here("model", "_discard_fleets", "watage", "8.0_revised_fleet_structure"),
  option = "Francis")
watage_discard <- SS_output(here::here("model", "_discard_fleets", "watage", "8.0_revised_fleet_structure_dw"))
# gradiant = 0.000634133
# NLL = 1782.14
# R0 = 9.94013
# M = 0.0871005
# Parameters with high SD
# Age_DblN_descend_se_Triennial_Early(7)	-9.1745900	4	-10.000	10.00	-9.433310	OK	5.00489e+03	-3.62251e-07
# Age_DblN_descend_se_Triennial_Late(8)	-9.9987800	4	-10.000	10.00	-9.135740	LO	5.51258e+00	-5.09508e-07
# Age_DblN_ascend_se_WCGBT(10)	-7.8571900	4	-10.000	10.00	-8.065950	OK	1.21003e+04	-1.99521e-07
# Age_DblN_descend_se_WCGBT(10)	-6.6047900	4	-10.000	10.00	-5.448950	OK	1.78279e+04	-1.38175e-07
# Age_DblN_descend_se_TWL_Discards(4)_BLK2repl_2011	-8.9811900	7	-10.000	10.00	-9.228970	OK	6.11530e+03	-3.20808e-07
# Age_DblN_descend_se_Pot_Discard(6)_BLK2repl_2011	1.8885000	7	-10.000	10.00	1.881720	OK	4.63390e+00	3.94147e-05

watage_discard <- SS_output(here::here("model", "_discard_fleets", "watage", "8.0_revised_fleet_structure_dw_mle"))
SS_plots(watage_discard)
# gradient = 0.000920817
# NLL = 1727.2
# R0 = 9.91403
# Age_DblN_descend_se_Triennial_Early(7)             -8.6473800     4 -10.000 10.00 -9.433310     OK  7976.13000000
# Age_DblN_ascend_se_WCGBT(10)                        4.3405300     4 -10.000 10.00 -8.065950     OK    69.07150000
# Age_DblN_descend_se_WCGBT(10)                      -7.3461400     4 -10.000 10.00 -5.448950     OK 14557.30000000
# Age_DblN_ascend_se_TWL(1)_BLK3repl_1890            -7.2728700     4 -10.000 10.00 -1.247530     OK  4248.98000000
# Age_DblN_top_logit_Triennial_Late(8)              -10.0000000     4 -10.000 10.00 -3.164970     LO     0.01303970
# Age_DblN_end_logit_NWFSC_Slope(9)                  -9.9939100     4 -10.000 10.00  2.526350     LO    28.23440000
# Age_DblN_top_logit_Triennial_Early(7)	Age_DblN_peak_Triennial_Early(7)	-0.999241


SS_plots(watage_discard)
plot_year_selex(
  replist = watage_discard,
  fleets = 1:3,
  year = 2024)
plot_year_selex(
  replist = watage_discard,
  fleets = 1:3,
  year = 1890)
plot_year_selex(
  replist = watage_discard,
  fleets = 4:6,
  year = 2024)
plot_year_selex(
  replist = watage_discard,
  fleets = 4:6,
  year = 2011)
plot_year_selex(
  replist = watage_discard,
  fleets = 4:6,
  year = 1890)

#===============================================================================
# Discard Fleet - Weight-at-Age - Early Devs.
#===============================================================================
watage_discard_devs <- SS_output(here::here("model", "_discard_fleets", "watage", "8.2_early_devs_dw"))
# gradiant = 6.49936e-05
# NLL = 1776.68
# R0 = 9.88195
# M = 0.0840556
# Triennial Q = 1.2088 and 1.39118 
# NWFSC Slope Q =  0.532344
# WCGBT Q = 1.31036
r4ss::tune_comps(
  replist = watage_discard_devs, 
  dir = here::here("model", "_discard_fleets", "watage", "8.2_early_devs"),
  option = "Francis")

modelnames <- c(
  "8.0 Split Triennial",
  "8.1 Estimate Early Rec. Devs.")
mysummary <- SSsummarize(list(
  watage_discard ,
  watage_discard_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "8.0-1_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

watage_discard_devs <- SS_output(here::here("model", "_discard_fleets", "watage", "8.2_early_devs_dw_mle"))
SS_plots(watage_discard_devs)

# gradient = 0.000806
# NLL = 1756
# R0 = 9.841
# Age_DblN_descend_se_Triennial_Early(7)             -7.8903900     4 -10.000 10.00 -9.433310     OK 11935.00000000
# Age_DblN_top_logit_Triennial_Late(8)              -10.0000000     4 -10.000 10.00 -3.164970     LO     0.01106340
# Age_DblN_ascend_se_WCGBT(10)                       -9.8019600     4 -10.000 10.00 -8.065950     LO  1240.13000000
# Age_DblN_descend_se_WCGBT(10)                      -5.5931300     4 -10.000 10.00 -5.448950     OK  1996.12000000
# # Age_DblN_descend_se_TWL_Discards(4)_BLK2repl_2011  -6.3845800     7 -10.000 10.00 -9.228970     OK 18732.40000000
modelnames <- c(
  "8.0 No Early Rec. Devs.",
  "8.1 Estimate Early Rec. Devs.")
mysummary <- SSsummarize(list(
  watage_discard ,
  watage_discard_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "8.0-1_mle_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)


#===============================================================================
# Discard Fleet - No Early Devs, Fix Poorly Estimated Parameters
#===============================================================================
watage_discard<- SS_output(here::here("model", "_discard_fleets", "watage", "8.3_no_early_devs_fix_params"))
# gradient = 0.000961105
# NLL = 1726
# R0 = 9.91549
SS_plots(watage_discard)
# May need to fix the NWFSC Slope final to asymptotic and fix the top logit WCGBT

# Remove triennial years of data
remove_tri_ages <- SS_output(here::here("model", "_discard_fleets", "watage", "8.4_triennial_data"))
# gradient = 0.000122995
# NLL = 1433.579
# R0 = 10.0132
SS_plots(remove_tri_ages)
# Remove the select years in the late period reduces the selectivity for older ages for this survey
# Triennial Early Q = 1.09681, Late Q = 2.31061
# WCGBT = 1.18013

# Remove select unfished aged form the landings fleets
remove_unsexed_fishery <- SS_output(here::here("model", "_discard_fleets", "watage", "8.5_remove_unsexed"))
# gradient =  0.00085685
# NLL = 1397.49
# R0 = 10.0153
SS_plots(remove_unsexed_fishery)
r4ss::tune_comps(
  replist = remove_unsexed_fishery, 
  dir = here::here("model", "_discard_fleets", "watage", "8.5_remove_unsexed"),
  option = "Francis")
remove_unsexed_fishery <- SS_output(here::here("model", "_discard_fleets", "watage", "8.5_remove_unsexed_dw"))
# NLL = 1396.27

# Set steepness to 0.75
steepness <- SS_output(here::here("model", "_discard_fleets", "watage", "8.6_steepness"))
# gradient = 0.000186449
# NLL = 1396.31
# R0 = 9.9555
SS_plots(steepness)
modelnames <- c(
  "8.0 No Early Rec. Devs.",
  "8.4 Remove Select Triennial Ages",
  "8.5 Remove Unsexed Ages HKL/Pot", 
  "8.6 Steepness = 0.75")
mysummary <- SSsummarize(list(
  watage_discard ,
  remove_tri_ages,
  remove_unsexed_fishery,
  steepness))
SSplotComparisons(mysummary,
                  filenameprefix = "8.4-8.6_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

# Adjust the bias adjustment
bias_adj <- SS_output(here::here("model", "_discard_fleets", "watage", "8.7_bias_adjustment"))
# gradient = 0.000173573
# NLL = 1399.15
# R0 = 10.0713
SS_plots(bias_adj)

# The new MLE after jitter
base <- SS_output(here::here("model", "_discard_fleets", "watage", "8.8_fix_beta"))
SS_plots(base)
# gradient = 0.000816109
# NLL = 1395.83 
# R0 = 10.0834000 
# Triennial Q = 1.04797 and 2.63568
# NWFSC Slope = 0.414383 (asymptotic selectivity)
# WCGBT Q = 1.17875 (peak at age 1 and constant ~ 0.80 for older ages)

# Tasks: Need to fix final male selectivity for HKL and Pot
# Parameters on bounds
# None

# Parameters with high stdev
# Age_DblN_top_logit_NWFSC_Slope(9)                 -8.1086000     4 -10.000 10.00  1.484160     OK 3313.12000000
# Age_DblN_descend_se_WCGBT(10)                     -5.5927400     4 -10.000 10.00 -7.346140     OK 1637.95000000

rec_dev2 <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.8_fix_beta_rec_dev_2"))
# gradient = 0.000781141
# NLL = 1398.59
# R0 = 10.0259
# Age_DblN_top_logit_NWFSC_Slope(9)                  1.4642700     4 -10.000 10.00  1.484160     OK   223.12400000
# Age_DblN_descend_se_WCGBT(10)                     -7.3460000     4 -10.000 10.00 -7.346140     OK 14557.90000000
modelnames <- c(
  "8.8 Rec Dev Option = 1",
  "8.8 Rec Dev Option = 2")
mysummary <- SSsummarize(list(
  base,
  rec_dev2))
SSplotComparisons(mysummary,
                  filenameprefix = "8.8_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

# Explore using option 3 for male selectivity
male_selex <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.9_fishery_selex"))
# gradient = 8.51217e-07
# NLL = 1393.83
# R0 = 10.0855
SS_plots(male_selex, plot = 2)

# Estimate all the parameters (except init) for the NWFSC Slope survey
slope_selex <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.10_nwfsc_slope_selex_hessian"))
SS_plots(slope_selex)
# gradient = 0.000100399
# NLL = 1391.15
# R0 = 10.0863
# Slope Q ~ 0.50
# Age_DblN_descend_se_NWFSC_Slope(9)                -7.6848300     4 -10.000 10.00 -7.685450     OK 48.97550000


# Estimate all the parameters (except init) for the WCGBT while keeping the same NWFSC Slope parameters active
wcgbt_selex <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.11_wcgbt_selex"))
SS_plots(wcgbt_selex)
# gradient = 6.14422e-05
# NLL = 1388.12
# R0 = 10.088
# Estimated variance of parameter 108 is -20630.5, failed to invert Hessian

# Use WCGBT comps with sex ratio applied
wcgbt_comps <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.12_wcgbt_selex_sexed"))
# gradient = 0.00077529
# NLL = 1336.55
# R0 = 10.083
# WCGBT Q = 1.09494
r4ss::tune_comps(
  replist = wcgbt_comps, 
  dir = here::here("model", "_discard_fleets", "watage", "8.12_wcgbt_selex_sexed"),
  option = "Francis")
wcgbt_comps_dw <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.12_wcgbt_selex_sexed_dw"))

modelnames <- c(
  "8.8 Rec Dev Option = 1",
  "8.9 Fishery Male Selex",
  "8.10 NWFSC Slope Selex",
  "8.11 WCGBT Selex",
  "8.12 WCGBT Sexed Comps.",
  "8.12 Update Data Weight")
mysummary <- SSsummarize(list(
  base,
  male_selex,
  slope_selex,
  wcgbt_selex,
  wcgbt_comps,
  wcgbt_comps_dw))
SSplotComparisons(mysummary,
                  filenameprefix = "8.8-12_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

blocks_selex <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.13_blocks"))
# gradient = 2.94688e-05
# NLL = 1389.51
# R0 = 10.0784
# 106 parameters
SS_plots(blocks_selex)

blocks_survey_selex <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.14_blocks_survey"))
# gradient = 0.000329736
# NLL = 1392.45
# R0 = 10.0766
# 104 parameters
r4ss::tune_comps(
  replist = blocks_survey_selex, 
  dir = here::here("model", "_discard_fleets", "watage", "8.14_blocks_survey"),
  option = "Francis")
blocks_survey_selex <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.14_blocks_survey_dw"))
SS_plots(blocks_survey_selex)
# gradient = 0.000669563
# NLL = 1430.38
# R0 = 10.0713
# Age_DblN_descend_se_NWFSC_Slope(9)                -6.9421400     4 -10.000 10.00 -7.174190     OK 10.91150000
modelnames <- c(
  "8.8 Rec Dev Option = 1",
  "8.14 Selectivity Refinements")
mysummary <- SSsummarize(list(
  base,
  blocks_survey_selex))
SSplotComparisons(mysummary,
                  filenameprefix = "8.8-8.14_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)


# WCGBT all data (sexed and unsexed)
# NLL = 1520.89
# 102 parameters

retain_asc_block <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.16_retain_asc_block"))
SS_plots(retain_asc_block)
# gradient = 2.33859e-05
# NLL = 1518.68
# R0 = 10.0765
# 103 parameters

retain_add_block <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.17_retain_add_block"))
# gradient = 0.000772292
# NLL = 1517.82
# R0 = 10.0737
# 106 parameters
SS_plots(retain_add_block)

all_blocks <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.18_all_blocks"))
SS_plots(all_blocks, plot = c(2, 17))
# gradient = 8.4481e-05
# NLL = 1515.72
# R0 = 10.0498
# 117 parameters

# Add blocks for the male parameters
male_selex_asc <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.19_male_asc_selex"))
SS_plots(all_blocks, plot = c(2, 17))
# gradient = 0.000443977
# NLL = 1513.89
# R0 = 10.0785
# 109 parameters
# Opting to not add additional blocks since the improved fit is minor given the additional parameters and
# that none of these changes improved the female residuals in recent years (observed > expected)

base_watage <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.15_wcgbt_all_data"))
SS_plots(base_watage)
plot_age_fits_sexed_only(replist = base_watage, years = 1890:2001)
plot_age_fits_sexed_only(replist = base_watage, years = 2002:2024)
modelnames <- c(
  "8.15",
  "8.16 Add HKL/Pot Block to Asc.",
  "8.17 Add HKL/Pot 2019-2024 Block",
  "8.19 Add HKL/Pot 2019-2024 Block and Male Param")
mysummary <- SSsummarize(list(
  base_watage,
  retain_asc_block,
  retain_add_block,
  male_selex_asc))
SSplotComparisons(mysummary,
                  filenameprefix = "8.15-19_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

# Move to F-type option 4
f_type <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.20_f_type"))
# This results in a large increase in the number of parameters with many of the early year parameters on the lower bound

# WCGBT all data (sexed and unsexed)
# NLL = 1520.89
# NWFSC Slope Age NLL = 34.9079
# WCGBT Age NLL = 328.358
# Survey NLL = -9.58574 -5.00984 -4.8089 -11.3201
# 102 parameters
# NWFSC Slope Q = 0.491442
# WCGBT Q = 1.16648
asym_survey_selex <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.21_slope_wcgbt_asym_selex"))
SS_plots(asym_survey_selex)
# gradient = 0.000623913
# NLL = 1530.66
# NWFSC Slope Age NLL = 35.3877
# WCGBT Age NLL = 340.656
# Survey NLL = -9.50006 -4.88126 -4.79483 -13.8312
# R0 = 10.1035
# NWFSC Slope Q = 0.393668
# WCGBT Q = 0.878335
# 100 parameters
plot_age_fits_sexed_only(replist = asym_survey_selex)

asym_survey_selex <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.21_slope_wcgbt_asym_selex_dw"))
r4ss::tune_comps(
  replist = asym_survey_selex, 
  dir = here::here("model", "_discard_fleets", "watage", "8.21_slope_wcgbt_asym_selex_dw"),
  option = "Francis")
SS_plots(asym_survey_selex)

modelnames <- c(
  "8.15 Slight Dome Selectivity",
  "8.21 Asym. Selectivity NWFSC Slope & WCGBT")
mysummary <- SSsummarize(list(
  base_watage,
  asym_survey_selex))
SSplotComparisons(mysummary,
                  filenameprefix = "8.21_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

trawl_block <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.22_trawl_blocks"))
SS_plots(trawl_block, plot = c(2, 17))
# gradient = 0.000554751
# NLL = 1518.72
# R0 = 10.0719
# 103 parameters
plot_age_fits_sexed_only(replist = trawl_block, years = 1890:2001)
plot_age_fits_sexed_only(replist = trawl_block, years = 2002:2024)

base_watage <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.23_preliminary_base"))
r4ss::tune_comps(
  replist = base_watage, 
  dir = here::here("model", "_discard_fleets", "watage", "8.23_preliminary_base"),
  option = "Francis")
SS_plots(base_watage)

# Revisit turning on extra SD for two models
extra_sd_domed_survey <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.24_wcgbt_extra_sd"))
# Q_extraSD_WCGBT(10) 0.0999995
# R0 = 10.1101
# NLL = 1402.92 (vs. 1434.13)
# Survey NLL = -48.46 (vs. -30.8828)
extra_sd_asym_survey <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.21_slope_wcgbt_asym_selex_dw_est_extra_sd"))
# Q_extraSD_WCGBT(10) 0.0902852
# R0 = 10.137
# NLL = 1506.47 (vs. 1530.66)
# Survey NLL = -45.96 (vs. -33.0073)
modelnames <- c(
  "8.23 Slight Dome Selctivity NWFSC Slope & WCGBT",
  "8.23 Slight Dome Selctivity NWFSC Slope & WCGBT w/ Extra SD",
  "8.21 Asym. Selectivity NWFSC Slope & WCGBT",
  "8.21 Asym. Selectivity NWFSC Slope & WCGBT w/ Extra SD")
mysummary <- SSsummarize(list(
  base_watage,
  extra_sd_domed_survey,
  asym_survey_selex, 
  extra_sd_asym_survey))
SSplotComparisons(mysummary,
                  filenameprefix = "8.23_vs_8.21_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

extra_sd_domed_survey <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.24_wcgbt_extra_sd_dw"))
r4ss::tune_comps(
  replist = extra_sd_domed_survey, 
  dir = here::here("model", "_discard_fleets", "watage", "8.24_wcgbt_extra_sd_dw"),
  option = "Francis")
ageing_error <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.25_ageing_error"))
modelnames <- c(
  "8.24 Slight Dome Selctivity NWFSC Slope & WCGBT w/ Extra SD",
  "8.25 2019 Ageing Error")
mysummary <- SSsummarize(list(
  extra_sd_domed_survey,
  ageing_error))
SSplotComparisons(mysummary,
                  filenameprefix = "8.24-5_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

extra_sd_domed_survey <-  SS_output(here::here("model", "_discard_fleets", "watage", "8.24_wcgbt_extra_sd_dw_fix_bound"))
r4ss::tune_comps(
  replist = extra_sd_domed_survey, 
  dir = here::here("model", "_discard_fleets", "watage", "8.24_wcgbt_extra_sd_dw_fix_bound"),
  option = "Francis")
modelnames <- c(
  "8.23",
  "8.24 WCGBT w/ Extra SD")
mysummary <- SSsummarize(list(
  base_watage,
  extra_sd_domed_survey))
SSplotComparisons(mysummary,
                  filenameprefix = "8.24_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)


francis <- SS_output(here::here("model", "_discard_fleets", "watage", "8.25_data_weight"))
mi <- SS_output(here::here("model", "_discard_fleets", "watage", "8.26_mi_data_weight"))
SS_plots(mi)
modelnames <- c(
  "8.25 Francis Data Weight",
  "8.26 McAllister-Ianelli Data Weight")
mysummary <- SSsummarize(list(
  francis,
  mi))
SSplotComparisons(mysummary,
                  filenameprefix = "8.25-6_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

est_steepness <- SS_output(here::here("model", "_discard_fleets", "watage", "8.27_est_steepness"))
modelnames <- c(
  "8.25 Original Weight-at-Age w/ Extra SD",
  "8.27 Estimate Steepness w/ Extra SD")
mysummary <- SSsummarize(list(
  francis,
  est_steepness))
SSplotComparisons(mysummary,
                  filenameprefix = "8.25-8.27_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

fix_watage <- SS_output(here::here("model", "_discard_fleets", "watage", "8.28_fixed_watage"))
modelnames <- c(
  "8.25 Original Weight-at-Age",
  "8.28 Revised Weight-at-Age")
mysummary <- SSsummarize(list(
  francis,
  fix_watage))
SSplotComparisons(mysummary,
                  filenameprefix = "8.25-8.28_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)


watage_rec_dev_2 <- SS_output(here::here("model", "_discard_fleets", "watage", "8.29_fixed_watage_no_extra_sd_rec_dev_2"))
SS_plots(watage_rec_dev_2)

#===============================================================================
# Turn off extra sd for the WCGBT survey with the fixed weight-at-age
#===============================================================================
watage <- SS_output(here::here("model", "_discard_fleets", "watage", "8.29_fixed_watage_no_extra_sd"))
# gradient = 5.02957e-07
# NLL = 1523.65
# R0 = 10.1582
# 102 parameters
SS_plots(watage)

remove_hkl_disc_desc <- SS_output(here::here("model", "_discard_fleets", "watage", "8.29_remove_hkl_disc_desc_block"))
# gradient = 0.000762993
# NLL = 1524.78
# R0 = 10.1582
# 101 parameters
SS_plots(remove_hkl_disc_desc)
plot_fleet_selectivity(model_out = remove_hkl_disc_desc, fleet_num = 1)
plot_year_selex(
    replist = remove_hkl_disc_desc,
    fleets = 1:3,
    year = 2002)

remove_disc_desc <- SS_output(here::here("model", "_discard_fleets", "watage", "8.31_remove_disc_desc"))
# gradient = 0.000266743
# NLL = 1529.86
# R0 = 10.1569
# 101 parameters
SS_plots(remove_disc_desc, plot = 17)

remove_fixed_disc_desc <- SS_output(here::here("model", "_discard_fleets", "watage", "8.32_remove_disc_desc_block_hkl_pot"))
# gradient = 0.000206439
# NLL = 1524.94
# R0 = 10.1587
# 101 parameters
r4ss::tune_comps(
  replist = remove_fixed_disc_desc, 
  dir = here::here("model", "_discard_fleets", "watage", "8.32_remove_disc_desc_block_hkl_pot"),
  option = "Francis")


modelnames <- c(
  "8.29 Rec Dev Option = 1",
  "8.29 Rec Dev Option = 2")
mysummary <- SSsummarize(list(
  watage,
  watage_rec_dev_2))
SSplotComparisons(mysummary,
                  filenameprefix = "8.29_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

data_weight_wtatage <- SS_output(here::here("model", "_discard_fleets", "watage", "8.33_data_weight"))
SS_plots(data_weight_wtatage)
# gradient = 5.01888e-06
# NLL = 1341.02
# R0 = 10.1751
# 101 parameters

# Apply the same blocks to all fishery fleets 
same_blocks <- SS_output(here::here("model", "_discard_fleets", "watage", "8.33_same_blocks"))
# gradient = 0.000309307
# NLL = 1369.87
# R0 = 10.166
# 114 parameters
SS_plots(same_blocks)

modelnames <- c(
  "8.33",
  "8.33 + Apply the Same Blocks")
mysummary <- SSsummarize(list(
  data_weight_wtatage ,
  same_blocks))
SSplotComparisons(mysummary,
                  filenameprefix = "8.33_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
data_weight_rec_option_2 <- SS_output(here::here("model", "_discard_fleets", "watage", "8.33_data_weight_rec_dev_2"))
SS_plots(data_weight_rec_option_2)

modelnames <- c(
  "8.33 No Enviro Index & Rec Dev Option = 1",
  "8.33 No Enviro Index & Rec Dev Option = 2")
mysummary <- SSsummarize(list(
  data_weight_wtatage,
  data_weight_rec_option_2 ))
SSplotComparisons(mysummary,
                  filenameprefix = "8.33_no_enviro_index_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.15,
                  pdf = TRUE)

add_enviro <- SS_output(here::here("model", "_discard_fleets", "watage", "8.33_enviro_index"))
SS_plots(add_enviro)
# gradient = 9.40391e-05
# NLL = 1342.2
# R0 = 10.1733
# active parameters = 102

# Parameters that hit the bounds in the jitter the most frequently
# Age_DblN_peak_HKL_Discards(5)                4.4824800     4   0.010 20.00  4.553810     OK 0.40348400
# Age_DblN_ascend_se_Pot_Discard(6)            0.0183923     5 -10.000 10.00  0.114341     OK 0.59197900
# Age_DblN_end_logit_Pot_Discard(6)            2.1574200     4 -10.000 10.00  1.484330     OK 1.50029000
# Age_DblN_peak_Pot_Discard(6)_BLK1repl_1890   2.2713600     6   1.000 20.00  2.285200     OK 0.34297900
#
# Parameters with the highest correlations
# Age_DblN_peak_Pot_Discard(6)_BLK1repl_2011	Age_DblN_ascend_se_Pot_Discard(6)	0.919106
# Age_DblN_ascend_se_HKL(2)	Age_DblN_peak_HKL(2)	0.91807
# Age_DblN_ascend_se_TWL(1)	Age_DblN_peak_TWL(1)	0.915924
# Age_DblN_ascend_se_Pot_Discard(6)	Age_DblN_peak_Pot_Discard(6)	0.892136

additional_simplications <- SS_output(here::here("model", "_discard_fleets", "watage", "8.34_fix_additional_selex_param"))
# gradient = 9.63052e-05
# NLL = 1344.18
# R0 = 10.1738
# active parameters = 99
# Age_DblN_ascend_se_Pot_Discard(6)           -0.6313840     4 -10.000 10.00 -0.567118     OK 1.00428000
# Age_DblN_end_logit_Pot_Discard(6)            2.5790600     4 -10.000 10.00  2.676390     OK 1.97631000
# Age_DblN_end_logit_Triennial_Early(7)       -5.3227600     4 -10.000 10.00 -5.324070     OK 1.58515000
# Age_DblN_end_logit_NWFSC_Slope(9)            1.2732600     4 -10.000 10.00  1.281270     OK 1.10952000

modelnames <- c(
  "8.33",
  "8.34")
mysummary <- SSsummarize(list(
  add_enviro,
  additional_simplications))
SSplotComparisons(mysummary,
                  filenameprefix = "8.33-.34_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

additional_simplications <- SS_output(here::here("model", "_discard_fleets", "watage", "8.34_fix_additional_selex_param"))
additional_simplications_rec_option_2 <- SS_output(here::here("model", "_discard_fleets", "watage", "8.34_fix_additional_selex_param_rec_dev_2"))
modelnames <- c(
  "8.34 Rec Dev Option = 1",
  "8.34 Rec Dev Option = 2")
mysummary <- SSsummarize(list(
  additional_simplications,
  additional_simplications_rec_option_2))
SSplotComparisons(mysummary,
                  filenameprefix = "8.34_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

base_model <- SS_output(here::here("model", "_discard_fleets", "watage", "8.34_fix_additional_selex_param_rec_dev_2"))
r4ss::tune_comps(
  replist = base_model, 
  dir = here::here("model", "_discard_fleets", "watage", "8.34_fix_additional_selex_param_rec_dev_2"),
  option = "Francis")

base_model <- SS_output(here::here("model", "_discard_fleets", "watage", "8.36_base_model"))
modelnames <- c(
  "8.34 Rec Dev Option = 1",
  "8.34 Rec Dev Option = 2", 
  "+ Updated Data Weights")
mysummary <- SSsummarize(list(
  additional_simplications,
  additional_simplications_rec_option_2,
  base_model))
SSplotComparisons(mysummary,
                  filenameprefix = "8.34_data_weight_rec_dev_2_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.25,
                  pdf = TRUE)

base_model <- SS_output(here::here("model", "_discard_fleets", "watage", "8.36_base_model"))
fix_twl_discard_init <- SS_output(here::here("model", "_discard_fleets", "watage", "8.37_trawl_discard"))
modelnames <- c(
  "8.36 Base Model",
  "8.37 Fix Trawl Discard Init")
mysummary <- SSsummarize(list(
  base_model ,
  fix_twl_discard_init))
SSplotComparisons(mysummary,
                  filenameprefix = "8.37_trawl_discard_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.25,
                  pdf = TRUE)
SS_plots(fix_twl_discard_init)

#===============================================================================
# Blocks
#===============================================================================

base_model <- r4ss::SS_output(here::here("model", "_discard_fleets", "watage", "8.36_base_model"))
no_blocks <- r4ss::SS_output(here::here("model", "_discard_fleets", "watage", "8.38_no_selex_blocks"))

modelnames <- c(
  "8.36 Base Model",
  "8.38 No Selectivity Blocks")
mysummary <- r4ss::SSsummarize(list(
  base_model ,
  no_blocks ))
r4ss::SSplotComparisons(mysummary,
                  filenameprefix = "8.38_trawl_discard_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "watage"),
                  ylimAdj = 1.10,
                  pdf = TRUE)
r4ss::SS_plots(no_blocks )

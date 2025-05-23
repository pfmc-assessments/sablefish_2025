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

# Explore HKL/Pot Selectivity



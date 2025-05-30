

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
slope_selex_2011 <- SS_output(here::here("model", "_discard_fleets", "growth", "6.3_slope_selex_2011_dw"))

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

#===============================================================================
# Remove the AFSC Slope survey
#===============================================================================
# built from the 6.3_slope_selex_2011_dw
# gradient =  0.0194295
# NLL =  2450.68
# R0 = 10.0515

no_afsc_slope <- SS_output(here::here("model", "_discard_fleets", "growth", "7.0_remove_afsc_survey"))
SS_plots(no_afsc_slope )

# gradient =  0.0137538
# NLL =  2350.26
# NLL Triennial Ages = 273.843
# R0 = 10.004
# Triennal Q = 0.73062 and 2.16868

r4ss::tune_comps(
  replist = no_afsc_slope, 
  dir = here::here("model", "_discard_fleets", "growth", "7.0_remove_afsc_survey"),
  option = "Francis")
# The suggested data weights are nearly identical, so I am not going to data weight yet.
# no_afsc_slope <- SS_output(here::here("model", "_discard_fleets", "growth", "7.0_remove_afsc_survey_dw"))


tri_selex <- SS_output(here::here("model", "_discard_fleets", "growth", "7.1_triennial"))
# gradient =  0.000144795 
# NLL = 2448.9
# NLL Triennial Ages = 362.975
# R0 = 10.0468 
# Triennial Q = 0.696297 and 2.1393

modelnames <- c(
  "6.3",
  "7.0 Remove AFSC Slope Survey",
  "7.1 Remove Triennial Sex-Selex")
mysummary <- SSsummarize(list(
  slope_selex_2011,
  no_afsc_slope ,
  tri_selex))
SSplotComparisons(mysummary,
                  filenameprefix = "7.0-7.1_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

r4ss::tune_comps(
  replist = tri_selex, 
  dir = here::here("model", "_discard_fleets", "growth", "7.1_triennial"),
  option = "Francis")
# The new suggested data weights for the triennial ages is much lower than before:
# 0.824494 vs. 1.738770
tri_selex <- SS_output(here::here("model", "_discard_fleets", "growth", "7.1_triennial_dw"))
plot_ghostfleets(replist = tri_selex)
plot_age_fits_sexed_only(replist = tri_selex)

modelnames <- c(
  "6.3",
  "7.0 Remove AFSC Slope Survey",
  "7.1 Remove Triennial Sex-Selex")
mysummary <- SSsummarize(list(
  slope_selex_2011,
  no_afsc_slope ,
  tri_selex))
SSplotComparisons(mysummary,
                  filenameprefix = "7.0-7.1_dw_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)


trawl_selex <- SS_output(here::here("model", "_discard_fleets", "growth", "7.2_remove_sex_trawl_selex"))
r4ss::tune_comps(
  replist = trawl_selex, 
  dir = here::here("model", "_discard_fleets", "growth", "7.2_remove_sex_trawl_selex"),
  option = "Francis")

# The new suggested data weights for the triennial ages is much lower than before:
# 0.824494 vs. 1.738770
trawl_selex <- SS_output(here::here("model", "_discard_fleets", "growth", "7.2_remove_sex_trawl_selex"))

modelnames <- c(
  "6.3",
  "7.0 Remove AFSC Slope Survey",
  "7.1 Remove Triennial Sex-Selex",
  "7.2 Remove Trawl Sex-Selex")
mysummary <- SSsummarize(list(
  slope_selex_2011,
  no_afsc_slope ,
  tri_selex,
  trawl_selex))
SSplotComparisons(mysummary,
                  filenameprefix = "7.0-7.2_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# Mirror the HKL and Pot Landings fleets
#===============================================================================
mirror_fixed_gears <- SS_output(here::here("model", "_discard_fleets", "growth", "7.5_mirror_pot_hkl"))
SS_plots(mirror_fixed_gears)
# gradient =  0.0151213
# NLL = 2172.42
# R0 = 10.0942 


#===============================================================================
# Try to improve the fit to the WCGBT age data
#===============================================================================
wcgbt_db <- SS_output(here::here("model", "_discard_fleets", "growth", "7.6_wcgbt_selex_double_normal"))
plot_ghostfleets(replist = wcgbt_db)
plot_age_fits_sexed_only(replist = wcgbt_db)
SS_plots(wcgbt_db)
# gradient =  0.0370341
# NLL = 2174.19
# WCGBT Age NLL = 656.419 (656.274)
# R0 = 10.0921
# Q = 1.09601
# The selectivity is no longer fully asymptotic but very close to ~ 0.90
# This results in a slight underfit to the oldest ages, but better fits the peak a the
# youngest ages
r4ss::tune_comps(
  replist = wcgbt_db, 
  dir = here::here("model", "_discard_fleets", "growth", "7.6_wcgbt_selex_double_normal"),
  option = "Francis")
# The data weight suggested for this fleet does not change with the new selectivity shape

modelnames <- c(
  "7.0 Remove AFSC Slope Survey",
  "7.1 Remove Triennial Sex-Selex",
  "7.2 Remove Trawl Sex-Selex",
  "7.5 Mirror HKL & Pot Retention Fleets",
  "7.6 Adjust WCGBT Selex")
mysummary <- SSsummarize(list(
  no_afsc_slope ,
  tri_selex,
  trawl_selex,
  mirror_fixed_gears,
  wcgbt_db))
SSplotComparisons(mysummary,
                  filenameprefix = "7.0-7.6_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

# Try applying a spline to the NWFSC Slope survey
nwfsc_slope_spline <- SS_output(here::here("model", "_discard_fleets", "growth", "7.7_nwfsc_slope_peak"))
# gradient =  0.0016718
# NLL = 2176.89
# NWFSC Slope Age NLL = 38.8217 (36.5359)
# R0 = 10.0844
# Q = 1.24046 (0.515876)
# The selectivity is a strange S shape that and results in an unrealistic Q
plot_ghostfleets(replist = nwfsc_slope_spline)
plot_age_fits_sexed_only(replist = nwfsc_slope_spline)
SS_plots(nwfsc_slope_spline, plot = 2)

# Try adjusting the double normal peak and other parameters
# The peak of the distribution occurs at age 4 but generally does not align with the
# aggregated comps due to the other parameters
nwfsc_slope_peak <- SS_output(here::here("model", "_discard_fleets", "growth", "7.7_nwfsc_slope_peak_double_normal"))
# gradient = 0.0290165
# NLL = 2174.01
# NWFSC Slope Age NLL = 36.1292 (36.5359)
# R0 = 10.0922
# Q = 0.626377 (0.515876)

plot_ghostfleets(replist = nwfsc_slope_peak)
plot_age_fits_sexed_only(replist = nwfsc_slope_peak)
SS_plots(nwfsc_slope_peak)


r4ss::tune_comps(
  replist = nwfsc_slope_spline, 
  dir = here::here("model", "_discard_fleets", "growth", "7.7_nwfsc_slope_peak"),
  option = "Francis")
# The data weight suggested for this fleet does not change with the new selectivity shape

modelnames <- c(
  "7.0 Remove AFSC Slope Survey",
  "7.1 Remove Triennial Sex-Selex",
  "7.2 Remove Trawl Sex-Selex",
  "7.5 Mirror HKL & Pot Retention Fleets",
  "7.6 Adjust WCGBT Selex",
  "7.7 NWFSC Slope Change Peak")
mysummary <- SSsummarize(list(
  no_afsc_slope ,
  tri_selex,
  trawl_selex,
  mirror_fixed_gears,
  wcgbt_db,
  nwfsc_slope_peak))
SSplotComparisons(mysummary,
                  filenameprefix = "7.0-7.7_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

# Explore the CV or SD options around growth
cv_fa <- SS_output(here::here("model", "_discard_fleets", "growth", "7.8_growth_cv_f(a)"))
# gradient = 0.014328
# NLL = 2176.94
# R0 = 10.0994
# Lmax = 60.3097 and 55.6329 (60.5149 and 55.4477)
# -----------sd(laa)-----------------------------------
sd_laa <- SS_output(here::here("model", "_discard_fleets", "growth", "7.8_growth_sd_f(laa)"))
SS_plots(sd_laa , plot = 26)
# gradient = 0.0362911
# NLL = 2174.55
# R0 = 10.0994
# Lmax = 60.5562 and 55.4564 (60.5149 and 55.4477)
# -----------sd(a)-----------------------------------
# gradient = 783.989
# NLL = 2240.4
# -----------log-sd-----------------------------------
# gradient = 0.000418152
# NLL = 2174.49
# R0 = 10.0934
# Lmax = 60.6601 and 56.0224 (60.5149 and 55.4477)
log_sd <- SS_output(here::here("model", "_discard_fleets", "growth", "7.8_growth_sd_log"))
SS_plots(log_sd)

# Jitter found a better fit for this model 
# gradient = 0.000767795
# NLL = 2160.81
# R0 = 10.0786
# Lmax = 60.7417 and 56.0595 (60.5149 and 55.4477)

# Parameters on the bounds
# Age_DblN_descend_se_NWFSC_Slope(8)	-9.9999900	4	-10.000	10.00	-10.0000000	LO	2.53969e-0
# Age_DblN_peak_TWL_Discards(4)_BLK2repl_2011	0.0751665	6	0.010	20.00	0.0908572	LO	1.11627e-02

# Parameters with high stdev
# Age_DblN_top_logit_NWFSC_Slope(8)	-8.4695500	4	-10.000	10.00	-8.4698100	OK	2.79418e+00	
# Age_DblN_ascend_se_TWL(1)_BLK3repl_1890	-9.4794700	4	-10.000	10.00	-1.5308400	OK	3.20646e+03
# Age_DblN_ascend_se_WCGBT(9)	-7.5258800	4	-10.000	10.00	-8.0662300	OK	1.37120e+04
# Age_DblN_descend_se_WCGBT(9)	-8.3353500	4	-10.000	10.00	-5.4483900	OK	9.65190e+03

#===============================================================================
# Revisit triennial selectivity
#===============================================================================

# Block width, descending, and final
# NLL = 2126.1
# Tri Ages = 148.75
# Q = 1.22368 and 1.5126

# Block width and final
# NLL = 2123.02
# Tri Age = 145.524
# Q = 1.08517 and 1.48642
triennial_width_final <- SS_output(here::here("model", "_discard_fleets", "growth", "7.9_triennial_block_width_final"))

# Block final
# NLL = 2159.95
# Tri Ages = 170.862
# Q = 1.20858 and 1.78026

# Hard Split the Triennial
# NLL = 2119.88
# Tri Ages = 11.3899 + 133.648
# Q = 1.12868 and 1.47007
# SD = 0.00100006 and 0.165875

# Each of these increase the Q from the sd(log) model for the early period of 0.668905 
# but decreases the late period Q from 2.06023

split_triennial <- SS_output(here::here("model", "_discard_fleets", "growth", "7.9_triennial_block_width_final_extra_sd_dw"))
r4ss::tune_comps(
  replist = split_triennial, 
  dir = here::here("model", "_discard_fleets", "growth", "7.9_triennial_block_width_final_extra_sd_dw"),
  option = "Francis")
# Data weighting suggests a bonker upweight to early triennial ages of 79!!! and late ages of 1.60.  There is 
# a slight increase to the suggested NWFSC Slope ages as well of 0.159 from 0.093.
# Changed the input sample size to the number of age samples and set the data weight to 1.0 for both periods.
# After data weighting the Qs are 1.17 and 1.40

modelnames <- c(
  "7.8 log(SD) Growth",
  "7.9 Triennial Select Blocks (width, final)",
  "7.9 Hard Split Triennial with input N change")
mysummary <- SSsummarize(list(
  log_sd,
  triennial_width_final,
  split_triennial))
SSplotComparisons(mysummary,
                  filenameprefix = "7.8_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

split_triennial <- SS_output(here::here("model", "_discard_fleets", "growth", "7.9_triennial_block_width_final_extra_sd_dw_mle"))
SS_plots(split_triennial)


plot_year_selex(
 replist = split_triennial,
 fleets = 1:3,
 year = 1890)
plot_year_selex(
  replist = split_triennial,
  fleets = 4:6,
  year = 1890)
plot_year_selex(
  replist = split_triennial,
  fleets = 4:6,
  year = 2011)
plot_year_selex(
  replist = split_triennial,
  fleets = 4:6,
  year = 2019)
#===============================================================================
# Explore deviations in k
#===============================================================================
k <- SS_output(here::here("model", "_discard_fleets", "growth", "7.10_growth_sd_log_k"))
SS_plots(k)
# gradient = 0.000376514
# NLL = 2117.08  (2160.81)
# R0 = 10.105 (10.0786)

# gradient = 0.000767795
# NLL = 2160.81
# R0 = 10.0786
# historical k = 0.45055 and 0.450196 (0.344845 and 0.35354)

k_short <- SS_output(here::here("model", "_discard_fleets", "growth", "7.10_growth_sd_log_k_shorter"))
SS_plots(k_short)

#===============================================================================
# Split the triennial and reorganize the fleets
#===============================================================================
init <- SS_output(here::here("model", "_discard_fleets", "growth", "7.9_triennial_block_width_final_extra_sd_dw_mle"))
fleet_structure <- SS_output(here::here("model", "_discard_fleets", "growth", "8.0_revised_fleet_structure_dw"))
# Parameters with high SD
# Age_DblN_descend_se_Triennial_Early(7)	-9.4333100	4	-10.000	10.00	-9.4333100	OK	3.48252e+03	-4.46516e-07
# Age_DblN_descend_se_Triennial_Late(8)	-9.1357400	4	-10.000	10.00	-9.1357400	OK	5.20723e+03	-3.52776e-07
# Age_DblN_ascend_se_WCGBT(10)	-8.0659500	4	-10.000	10.00	-8.0659500	OK	1.10492e+04	-2.14344e-07
# Age_DblN_descend_se_WCGBT(10)	-5.4489500	4	-10.000	10.00	-5.4489500	OK	1.52485e+03	3.01472e-07
# Age_DblN_descend_se_TWL_Discards(4)_BLK2repl_2011	-9.2289700	7	-10.000	10.00	-9.2289700	OK	4.68843e+03	-3.76495e-07

modelnames <- c(
  "7.9 Split",
  "8.0 Reorganize fleets")
mysummary <- SSsummarize(list(
  init,
  fleet_structure))
SSplotComparisons(mysummary,
                  filenameprefix = "8.0_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

r4ss::tune_comps(
  replist = fleet_structure, 
  dir = here::here("model", "_discard_fleets", "growth", "8.0_revised_fleet_structure"),
  option = "Francis")

#===============================================================================
# Change initial maturity age to 1
#===============================================================================
min_mat_age <- SS_output(here::here("model", "_discard_fleets", "growth", "8.1_min_maturity"))
# gradiant = 0.000371443
# NLL = 2369.24
# R0 = 10.0189
# M = 0.0842056
# Triennial Q = 1.18333 and 1.40101 
# NWFSC Slope Q =  0.525045
# WCGBT Q = 1.0741

modelnames <- c(
  "8.0 Reorganize fleets",
  "8.1 Min Maturity Age = 1")
mysummary <- SSsummarize(list(
  fleet_structure,
  min_mat_age))
SSplotComparisons(mysummary,
                  filenameprefix = "8.0-1_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
#===============================================================================
# Turn on early devs
#===============================================================================
early_devs <- SS_output(here::here("model", "_discard_fleets", "growth", "8.2_add_early_rec_devs_dw"))
# gradiant = 0.0009636
# NLL = 2360.6
# R0 = 9.94548
# M = 0.0787157
# Triennial Q = 1.26221 and 1.48155
# NWFSC Slope Q =  0.55361 
# WCGBT Q = 1.12716
SS_plots(early_devs)
r4ss::tune_comps(
  replist = early_devs, 
  dir = here::here("model", "_discard_fleets", "growth", "8.2_add_early_rec_devs"),
  option = "Francis")
modelnames <- c(
  "8.0 Reorganize fleets",
  "8.1 Min Maturity Age = 1",
  "8.1 Turn on Early Rec. Devs.")
mysummary <- SSsummarize(list(
  fleet_structure,
  min_mat_age,
  early_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "8.0-2_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# Remove the triennial index and age
#===============================================================================
remove_tri <- SS_output(here::here("model", "_discard_fleets", "growth", "8.3_early_devs_no_triennial"))
SS_plots(remove_tri)
r4ss::tune_comps(
  replist = remove_tri, 
  dir = here::here("model", "_discard_fleets", "growth", "8.3_early_devs_no_triennial"),
  option = "Francis")
modelnames <- c(
  "8.1 Turn on Early Rec. Devs.",
  "8.3 Remove the Triennial")
mysummary <- SSsummarize(list(
  early_devs, remove_tri))
SSplotComparisons(mysummary,
                  filenameprefix = "8.1-3_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# Triennial Explorations
#===============================================================================
triennial_all <- SS_output(here::here("model", "_discard_fleets", "growth", "9.0_min_maturity_fix_uncertain_selex_pars"))
SS_plots(triennial_all)
r4ss::tune_comps(
  replist = triennial_all, 
  dir = here::here("model", "_discard_fleets", "growth", "9.0_min_maturity_fix_uncertain_selex_pars"),
  option = "Francis")
# R0 = 10.1091 
# Survey NLL = -9.85654 -3.34772
# Age NLL = 14.4943 178.16
# Q = 1.10736 and 1.44768

# Remove 1986 samples 
triennial_remove_1986 <- SS_output(here::here("model", "_discard_fleets", "growth", "9.1_triennial_remove_1986"))
# R0 = 10.1087
# Survey NLL = -9.85779 -3.34694
# Age NLL = 13.7526 178.054
# Q = 1.13712 and 1.44913

# Remove 1986 and 1992 samples 
triennial_remove_1986_1992 <- SS_output(here::here("model", "_discard_fleets", "growth", "9.2_triennial_remove_1986_1992"))
# R0 = 10.1064
# Survey NLL = -9.6626 -3.05
# Age NLL = 9.96398 175.216
# Q = 1.10462 and 1.44887

# Remove all issue years
triennial_remove_mult_years <- SS_output(here::here("model", "_discard_fleets", "growth", "9.3_triennial_remove_1986_1992_1998_2001"))
# R0 = 10.1142 
# Survey NLL = 9.23474 -5.02791
# Age NLL = 10.0614 80.4806
# Q = 1.14623 and 1.81153

# Mirror the selectivity to the trawl fishery and remove all triennial age data
triennial_mirror_selex <- SS_output(here::here("model", "_discard_fleets", "growth", "9.4_triennial_mirror_trawl_selex"))
SS_plots(triennial_mirror_selex)
# R0 = 10.1511
# Survey NLL = -2.01469 -0.276122
# Q = 0.153293 and 0.511559

# Length based selectivity for the Triennial survey and remove the ages
triennial_len_selex <- SS_output(here::here("model", "_discard_fleets", "growth", "9.5_triennial_length_selex"))
# R0 = 10.1249
# Survey NLL = -5.13752 -1.3133
# Length NLL = 17.0901 10.7331
# Q = 1.12159 and 1.29998
r4ss::tune_comps(
  replist = triennial_len_selex, 
  dir = here::here("model", "_discard_fleets", "growth", "9.5_triennial_length_selex"),
  option = "Francis")

remove_triennial <- SS_output(here::here("model", "_discard_fleets", "growth", "9.6_remove_triennial"))
SS_plots(remove_triennial)
# R0 = 10.1945

r4ss::tune_comps(
  replist = remove_triennial, 
  dir = here::here("model", "_discard_fleets", "growth", "9.6_remove_triennial"),
  option = "Francis")

modelnames <- c(
  "9.0 All Triennial Data",
  "9.1 Remove 1986 Triennial Ages",
  "9.2 Remove 1986 and 1992 Triennial Ages",
  "9.3 Remove 1986, 1992, 1998, 2001 Triennial Ages",
  "9.4 Mirror Triennial Selex to Trawl Fishery",
  "9.5 Length-based selectivity for Triennial (no ages)",
  "9.6 Remove the Triennial Survey"
  )
mysummary <- SSsummarize(list(
  triennial_all, 
  triennial_remove_1986,
  triennial_remove_1986_1992,
  triennial_remove_mult_years,
  triennial_mirror_selex,
  triennial_len_selex,
  remove_triennial))
SSplotComparisons(mysummary,
                  filenameprefix = "9.0-9.6_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.65,
                  pdf = TRUE)


# Compare to the early dev model
early_devs_tri_mirror_selex <- SS_output(here::here("model", "_discard_fleets", "growth", "9.7_early_devs_triennial_mirror_trawl_selex"))
early_devs_tri_len_selex <- SS_output(here::here("model", "_discard_fleets", "growth", "9.8_early_devs_triennial_length_selex"))

modelnames <- c(
  "9.0 No Early Rec. Devs",
  "8.1 Turn on Early Rec. Devs.",
  "8.3 Remove the Triennial",
  "9.8 Length-based selectivity for Triennial (no ages)",
  "9.7 Mirror Triennial Selex to Trawl Fishery"
  )
mysummary <- SSsummarize(list(
  triennial_all,
  early_devs, 
  remove_tri,
  early_devs_tri_mirror_selex,
  early_devs_tri_len_selex))
SSplotComparisons(mysummary,
                  filenameprefix = "9.7-9.8_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# Selectivity checks for the varying block periods
#===============================================================================
full_block_model <- SS_output(here::here("model", "_discard_fleets", "growth", "9.0_min_maturity_fix_uncertain_selex_pars"))
# assumed inital blocks
# Discard Fleets = 1890 2010 2011 2018
# Landings Fleets = 1890 2001
plot_fleet_selectivity(model_out = triennial_all, fleet_num = 2)
plot_fleet_selectivity(model_out = triennial_all, fleet_num = 6)

plot_year_selex(
  replist = full_block_model,
  fleets = 1:3,
  year = 1890)
plot_year_selex(
  replist = full_block_model,
  fleets = 1:3,
  year = 2024)

plot_year_selex(
  replist = sfull_block_model,
  fleets = 4:6,
  year = 1890)
plot_year_selex(
  replist = full_block_model,
  fleets = 4:6,
  year = 2011)
plot_year_selex(
  replist = full_block_model,
  fleets = 4:6,
  year = 2019)

#===============================================================================
# Curvlinear Ageing Error
#===============================================================================
curv_age_error <- SS_output(here::here("model", "_discard_fleets", "growth", "9.9_curvlinear_ageing_errror"))
SS_plots(curv_age_error)
modelnames <- c(
  "9.0 Linear Ageing Error",
  "9.9 Curvlinear Ageing Error"
)
mysummary <- SSsummarize(list(
  triennial_all,
  curv_age_error))
SSplotComparisons(mysummary,
                  filenameprefix = "9.0-9.9_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# Remove unsexed HKL & Pot, Triennial select year, bias adj, and steepness
#===============================================================================
steep_bias_adj <- SS_output(here::here("model", "_discard_fleets", "growth", "9.10_linear_ae_bias_adj_steepness"))
r4ss::tune_comps(
  replist = steep_bias_adj, 
  dir = here::here("model", "_discard_fleets", "growth", "9.10_linear_ae_bias_adj_steepness"),
  option = "Francis")
steep_bias_adj <- SS_output(here::here("model", "_discard_fleets", "growth", "9.10_linear_ae_bias_adj_steepness_dw"))
SS_plots(steep_bias_adj)
# gradient = 0.000678344
# NLL = 1860.69
# R0 = 10.2518

# The new MLE after jitter
base_growth <- SS_output(here::here("model", "_discard_fleets", "growth", "9.11_fix_beta"))
SS_plots(base)
# gradient = 0.000885819
# NLL = 1879.60 <-- the jitter lowest NLL (37) was 1877.77 but had a gradient of 0.182771
# so I used the par from the next best fit (43) which had a NLL of 1897.60
# R0 = 10.1353 (37 = 10.1594; 43 = 10.1353) 37 2025 depl ~ 30% and 43 2025 depl ~ 31%
# Triennial Q = 1.11177 and 1.85021
# NWFSC Slope =  0.510465 selectivity looks similar to the WCGBT but peak at age 7
# WCGBT Q = 1.04503 selectivity peak at age 1 and ~ 0.90 for older ages

# Tasks: Need to fix final male selectivity for HKL and Pot
# Parameters on bounds
# Age_DblN_peak_TWL_Discards(4)	0.2074520	4	0.010	20.00	0.2073330	LO

# Parameters with high stdev
# Age_DblN_descend_se_WCGBT(10)                     -9.7867700     4 -10.000 10.00 -5.4489600     OK 1334.18000000

rec_dev2 <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.11_fix_beta_rec_dev_2"))
modelnames <- c(
  "9.11 Rec Dev Option = 1",
  "9.11 Rec Dev Option = 2")
mysummary <- SSsummarize(list(
  base,
  rec_dev2))
SSplotComparisons(mysummary,
                  filenameprefix = "9.11_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

# Explore using option 3 for male selectivity
male_selex_growth <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.13_fishery_selex"))
# gradient = 0.000294189
# NLL = 1864.82
# R0 = 10.1576
SS_plots(male_selex_growth, plot = 2)

slope_selex_growth <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.14_nwfsc_slope_selex"))
# gradient = 0.000464244
# NLL = 1868.4
# R0 = 10.1605

wcgbt_selex_growth <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.15_wcgbt_selex"))
# gradient = 0.000499167
# NLL = 1868.4
# R0 = 10.1605

blocks_growth <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.16_blocks"))
# gradient = 0.000552739
# NLL = 1868.93
# R0 = 10.1628

modelnames <- c(
  "9.11 ",
  "9.13 Fishery Male Selex",
  "9.14 NWFSC Slope Selex",
  "9.15 WCGBT Selex",
  "9.16 Fishery Blocks")
mysummary <- SSsummarize(list(
  base_growth,
  male_selex_growth,
  slope_selex_growth,
  wcgbt_selex_growth,
  blocks_growth))
SSplotComparisons(mysummary,
                  filenameprefix = "9.11-16_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

blocks_survey_growth <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.17_blocks_survey"))
# gradient = 0.000442573
# NLL = 1869.59
# R0 = 10.162
# 114 parameters
r4ss::tune_comps(
  replist = blocks_survey_growth, 
  dir = here::here("model", "_discard_fleets", "growth", "9.17_blocks_survey"),
  option = "Francis")
modelnames <- c(
  "9.11 ",
  "9.17 Block Adj.")
mysummary <- SSsummarize(list(
  base_growth,
  blocks_survey_growth))
SSplotComparisons(mysummary,
                  filenameprefix = "9.11-9.17_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

match_watage <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.18_match_watage_8.25"))
r4ss::tune_comps(
  replist = match_watage, 
  dir = here::here("model", "_discard_fleets", "growth", "9.18_match_watage_8.25"),
  option = "Francis")
match_watage <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.18_match_watage_8.25"))
# gradient = 0.00224541
# NLL = 2101.84
# R0 = 10.13
# 113 parameters
SS_plots(match_watage)

growth_fix_sd <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.19_no_extra_sd"))
growth_fix_sd_rec_dev_2 <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.19_no_extra_sd_rec_dev_2"))
modelnames <- c(
  "9.19 Rec. Dev. Option = 1",
  "9.19 Rec. Dev. Option = 2")
mysummary <- SSsummarize(list(
  growth_fix_sd ,
  growth_fix_sd_rec_dev_2))
SSplotComparisons(mysummary,
                  filenameprefix = "9.19_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets", "growth"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(growth_fix_sd)

fix_disc_desc_blocks <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.20_remove_fixed_disc_desc_blocks"))
fix_disc_desc_blocks_rec_dev_2 <-  SS_output(here::here("model", "_discard_fleets", "growth", "9.20_remove_fixed_disc_desc_blocks_rec_dev_2"))

r4ss::tune_comps(
  replist = remove_fixed_disc_desc, 
  dir = here::here("model", "_discard_fleets", "growth", "9.20_remove_fixed_disc_desc_blocks"),
  option = "Francis")

data_weight_growth <- SS_output(here::here("model", "_discard_fleets", "growth", "9.21_data_weight"))
# gradient = 0.000340719
# NLL = 1861.03
# R0 = 10.1642
# 101 parameters
SS_plots(data_weight_growth)

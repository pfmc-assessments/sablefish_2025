
#===============================================================================
# Retention Growth Models
#===============================================================================
age_ret_growth <- SS_output(here::here("model", "_retention_model", "0.1_age_ret_growth"))
len_ret_growth <- SS_output(here::here("model", "_retention_model", "0.1_len_ret_growth"))

modelnames <- c(
  "+ 0.1 Age Ret. Growth",
  "+ 0.1 Length Ret. Growth")
mysummary <- SSsummarize(list(
  age_ret_growth, 
  len_ret_growth))
SSplotComparisons(mysummary,
                  filenameprefix = "0.1_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_retention_model"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

r4ss::tune_comps(
  replist = len_ret_growth , 
  dir = here::here("model", "_retention_model", "0.1_len_ret_growth"),
  option = "Francis")

r4ss::tune_comps(
  replist = age_ret_growth , 
  dir = here::here("model", "_retention_model", "0.1_age_ret_growth"),
  option = "Francis")

age_ret_growth <- SS_output(here::here("model", "_retention_model", "0.1_age_ret_growth_dw"))
len_ret_growth <- SS_output(here::here("model", "_retention_model", "0.1_len_ret_growth_dw"))
 
# Both of these model have poor gradients:
# len ret 0.263
# age ret 1.090

modelnames <- c(
  "+ 0.1 Age Ret. Growth Data Weighted",
  "+ 0.1 Length Ret. Growth Data Weighted")
mysummary <- SSsummarize(list(
  age_ret_growth, 
  len_ret_growth))
SSplotComparisons(mysummary,
                  filenameprefix = "0.1_dw_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_retention_model"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(age_ret_growth)
SS_plots(len_ret_growth)

#===============================================================================
# Retention WatAge Models
#===============================================================================
age_ret_watage <- SS_output(here::here("model", "_retention_model", "0.2_age_ret_watage"))
SS_plots(age_ret_watage)


#===============================================================================
# Retention Growth Models w/ Rec. Dev. Option 2
#===============================================================================
age_ret_growth_dev <- SS_output(here::here("model", "_retention_model", "0.3_age_ret_growth_dev_2"))
len_ret_growth_dev <- SS_output(here::here("model", "_retention_model", "0.3_len_ret_growth_dev_2"))
age_ret_watage_dev <- SS_output(here::here("model", "_retention_model", "0.3_age_ret_watage_dev_2"))

modelnames <- c(
  "+ 0.1 Age Ret. Growth",
  "+ 0.1 Length Ret. Growth", 
  "+ 0.3 Age Ret. Growth Dev. Option 2",
  "+ 0.3 Length Ret. Growth Dev. Option 2")
mysummary <- SSsummarize(list(
  age_ret_growth, 
  len_ret_growth, 
  age_ret_growth_dev,
  len_ret_growth_dev))
SSplotComparisons(mysummary,
                  filenameprefix = "0.3_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_retention_model"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

r4ss::tune_comps(
  replist = len_ret_growth , 
  dir = here::here("model", "_retention_model", "0.1_len_ret_growth"),
  option = "Francis")

r4ss::tune_comps(
  replist = age_ret_growth , 
  dir = here::here("model", "_retention_model", "0.1_age_ret_growth"),
  option = "Francis")

age_ret_growth <- SS_output(here::here("model", "_retention_model", "0.1_age_ret_growth_dw"))
len_ret_growth <- SS_output(here::here("model", "_retention_model", "0.1_len_ret_growth_dw"))

modelnames <- c(
  "+ 0.1 Age Ret. Growth Data Weighted",
  "+ 0.1 Length Ret. Growth Data Weighted")
mysummary <- SSsummarize(list(
  age_ret_growth, 
  len_ret_growth))
SSplotComparisons(mysummary,
                  filenameprefix = "0.1_dw_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_retention_model"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(age_ret_growth)
SS_plots(len_ret_growth)


#===============================================================================
# Weight-at-age models
#===============================================================================
# this model is pulled from the bridging sandbox with 1975 main dev start
watage <- SS_output(here::here("model", "_retention_model", "19.0_weight_at_age"))
# CANNOT CALCULATE THE HESSIAN FOR THIS MODEL: parameter 1 is -0.00109949
# gradient = 0.000588279
# M f = 0.096, M m = 0.797
# NLL = 1130.19
# R0 = 10.1294

# reduce the discard ages input N by a factor of 10 to try to improve the selectivity fits
# input_n <- SS_output(here::here("model", "_retention_model", "19.1_discard_input_n"))
# this is causing the model to crash for some strange reason

# add at-sea catches for 2024 and new fishery ages
ages <- SS_output(here::here("model", "_retention_model", "19.1_data_at_sea_ages"))
# gradient = 0.000856551
# R0 = 10.218
# NLL = 1168.51
plot_ghostfleets(replist = ages)
plot_age_fits_sexed_only(replist = ages)
r4ss::tune_comps(
  replist = ages , 
  dir = here::here("model", "_retention_model", "19.1_data_at_sea_ages"),
  option = "Francis")


# add the initial ageing error provided by Cheryl on Monday
ageing_error <- SS_output(here::here("model", "_retention_model", "19.2_ageing_error"))
# gradient = 0.000986225
# NLL = 9504.19
# R0 = 14.9996000

# set male and female M to be equal and estimated
single_m <- SS_output(here::here("model", "_retention_model", "19.3_single_m_old_ageing_error"))
# this crashes the model

modelnames <- c(
  "19.0 Weight-at-Age",
  "19.1 + new age data and at-sea catch")
mysummary <- SSsummarize(list(
  watage, 
  ages))
SSplotComparisons(mysummary,
                  filenameprefix = "19.0_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_retention_model"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

# 19.6_single_m_old_ageing_error_rec_dev_2 - bad model
# 19.7_survey_sd - bad model

#===============================================================================
# 20.0 Weight-at-Age building from 19.1 at-sea and ages - selectivity changes
#===============================================================================

ages <- SS_output(here::here("model", "_retention_model", "20.0_data_at_sea_ages _dw"))

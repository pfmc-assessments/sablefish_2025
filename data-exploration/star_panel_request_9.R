
base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
low <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "ageing_bias_minus_25"))
high <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "ageing_bias_plus_25"))

modelnames <- c(
  "Base Model",
  "Low State of Nature",
  "High State of Nature")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  low,
  high))
r4ss::SSplotComparisons(
  mysummary,
  subplots = c(1, 3),
  filenameprefix = "request_9_ae_fixed_m_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("model", "_star_panel_requests", "request_9"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)



base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
low <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "ageing_bias_minus_25_dt"))
high <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "ageing_bias_plus_25_dt"))

modelnames <- c(
  "Base Model",
  "Low State of Nature",
  "High State of Nature")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  low,
  high))
r4ss::SSplotComparisons(
  mysummary,
  subplots = c(1, 3),
  filenameprefix = "request_9_ae_fixed_m_dt_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("model", "_star_panel_requests", "request_9"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)

#===============================================================================
# Fix M at low and high values
#===============================================================================

base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
low <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "ageing_bias_minus_25_m_0.70"))
high <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "ageing_bias_plus_25_m_0.104"))

modelnames <- c(
  "Base Model",
  "Low State of Nature",
  "High State of Nature")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  low,
  high))
r4ss::SSplotComparisons(
  mysummary,
  subplots = c(1, 3),
  filenameprefix = "request_9_ae_fixed_lowhigh_m_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("model", "_star_panel_requests", "request_9"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)

#===============================================================================
# Sigma R and M
#===============================================================================

base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
low <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "sigma_r_m", "sigma_r_low_m_high"))
high <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "sigma_r_m", "sigma_r_high_m_low"))

modelnames <- c(
  "Base Model",
  "Low State of Nature",
  "High State of Nature")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  low,
  high))
r4ss::SSplotComparisons(
  mysummary,
  subplots = c(1, 3),
  filenameprefix = "request_9_sigma_r_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("model", "_star_panel_requests", "request_9", "sigma_r_m"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)

#===============================================================================
# Fix Rec. Devs.
#===============================================================================

base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
percent_10 <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "rec_devs", "low_devs_2016_10_percent"))
percent_20 <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "rec_devs", "low_devs_2016_20_percent"))
percent_10_high <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "rec_devs", "high_devs_2016_10_percent"))
percent_20_high <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_9", "rec_devs", "high_devs_2016_20_percent"))

modelnames <- c(
  "Base Model",
  "Rec. Devs. 2016-2023 Positive 10% Less/Negative 10% Greater",
  "Rec. Devs. 2016-2023 Positive 10% Greater/Negative 10% Less")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  percent_10,
  percent_10_high))
r4ss::SSplotComparisons(
  mysummary,
  subplots = 1:4,
  filenameprefix = "request_9_rec_dev_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("model", "_star_panel_requests", "request_9", "rec_devs"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)


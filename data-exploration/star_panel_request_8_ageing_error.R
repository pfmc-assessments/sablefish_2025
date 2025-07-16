
base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
plus_25 <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_8", "ageing_bias_plus_25"))
plus_20 <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_8", "ageing_bias_plus_20"))
plus_15 <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_8", "ageing_bias_plus_15"))
plus_10 <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_8", "ageing_bias_plus_10"))
minus_25 <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_8", "ageing_bias_minus_25"))
minus_20 <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_8", "ageing_bias_minus_20"))
minus_15 <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_8", "ageing_bias_minus_15"))
minus_10 <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_8", "ageing_bias_minus_10"))

modelnames <- c(
  "Base Model",
  "+25% Ageing Bias",
  #"+20% Ageing Bias",
  #"+15% Ageing Bias",
  #"+10% Ageing Bias",
  #"-10% Ageing Bias",
  #"-15% Ageing Bias",
  #"-20% Ageing Bias",
  "-25% Ageing Bias")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  plus_25,
  #plus_20,
  #plus_15,
  #plus_10,
  #minus_10,
  #minus_15,
  #minus_20,
  minus_25))
r4ss::SSplotComparisons(
  mysummary,
  filenameprefix = "request_8_subset_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("model", "_star_panel_requests", "request_8"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)

r4ss::SSplotComparisons(
  mysummary,
  subplots = 5:12,
  filenameprefix = "request_8_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("model", "_star_panel_requests", "request_8"),
  ylimAdj = 1.25,
  legendloc = "topleft",
  pdf = FALSE,
  print = TRUE)

create_comparison_table(
    dir = here::here("model", "_star_panel_requests", "request_8"),
    model_summary = mysummary,
    model_names = modelnames,
    add_name = "ageing_error")


base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
low <- r4ss::SS_output(dir = here::here("model", "base_model", "decision_table", "low_pstar_45"))
high <- r4ss::SS_output(dir = here::here("model", "base_model", "decision_table", "high_pstar_45"))

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
  filenameprefix = "request_8_decision_table",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("model", "_star_panel_requests", "request_8"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)

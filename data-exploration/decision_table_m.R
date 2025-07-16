
base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
low <- r4ss::SS_output(dir = here::here("model", "base_model", "decision_table", "m", "low_pstar_45"))
high <- r4ss::SS_output(dir = here::here("model", "base_model", "decision_table", "m", "high_pstar_45"))

modelnames <- c(
  "Base Model",
  "Low State of Nature (2019 M)",
  "High State of Nature (Emigration)")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  low,
  high))
r4ss::SSplotComparisons(
  mysummary,
  filenameprefix = "m_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("model", "base_model", "decision_table", "m"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)



base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
low <- r4ss::SS_output(dir = here::here("model", "base_model", "_sensitivities", "14-M-Fix-2019Assess"))
high <- r4ss::SS_output(dir = here::here("model", "base_model", "_sensitivities", "13-M-adjust-migration"))

modelnames <- c(
  "Base Model",
  "2019 M",
  "Emigration")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  low,
  high))
r4ss::SSplotComparisons(
  mysummary,
  subplots = c(2, 4),
  filenameprefix = "m_sensitivites_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("model", "base_model", "decision_table", "m"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)

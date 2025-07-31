# Plot the time series from the states of nature

folder <- "m"
catch_name <- "pstar_45"
base <- r4ss::SS_output(here::here("model", "base_model", "decision_table", folder, paste0("base_", catch_name)))
low <- r4ss::SS_output(here::here("model", "base_model", "decision_table", folder, paste0("low_", catch_name)))
high <- r4ss::SS_output(here::here("model", "base_model", "decision_table", folder, paste0("high_", catch_name)))

modelnames <- c(
  "Low State of Nature",
  "Base Model",
  "High State of Nature")
mysummary <- r4ss::SSsummarize(list(
  low, 
  base,
  high))
r4ss::SSplotComparisons(
  mysummary,
  filenameprefix = "states_of_nature_",
  subplots = c(2, 4),
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  endyr = 2036,
  plotdir = here::here("presentation", "gfsc_ssc", "plots"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)



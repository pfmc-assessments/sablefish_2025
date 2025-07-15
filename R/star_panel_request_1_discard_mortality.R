
base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
full_mortality <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_1", "full_discard"))
mortality_increas <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_1", "intermediate_discard"))
r4ss::SS_plots(full_mortality)
r4ss::SS_plots(mortality_increas)

modelnames <- c(
  "Pre-STAR Base Model",
  "100% Discard Mortality",
  "75% TWL and 60% HKL/Pot Discard Mortality")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  full_mortality,
  mortality_increas))
r4ss::SSplotComparisons(
  mysummary,
  filenameprefix = "request_1_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_star_panel_requests", "request_1"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)

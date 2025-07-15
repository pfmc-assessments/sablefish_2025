base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
no_blocks <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_3", "8.36_no_selex_blocks"))
r4ss::SS_plots(no_blocks)

consistent_blocks <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_3", "8.36_consistent_blocks"))
r4ss::tune_comps(
  replist = consistent_blocks, 
  dir = here::here("model", "_star_panel_requests", "request_3", "8.36_consistent_blocks"),
  option = "Francis")
consistent_blocks <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_3", "8.36_consistent_blocks"))
r4ss::SS_plots(consistent_blocks)

modelnames <- c(
  "Pre-STAR Base Model",
  "No Blocks",
  "Consistent Blocks")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  no_blocks,
  consistent_blocks))
r4ss::SSplotComparisons(
  mysummary,
  filenameprefix = "request_3_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_star_panel_requests", "request_3"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)

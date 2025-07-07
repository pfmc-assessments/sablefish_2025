

#===============================================================================
# r4ss plots
#===============================================================================

model_output <- r4ss::SS_output(here::here("model", "base_model", "8.36_base_model"))
save(model_output, file = here::here("presentation", "model_and_diag", "tables", "model_output.rda"))
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 4,
  forecastplot = FALSE,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 7,
  forecastplot = FALSE,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 9,
  forecastplot = FALSE,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 11,
  forecastplot = FALSE,
  print = TRUE,
  minyr = 1950,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotSPR(
  replist = model_output,
  subplot = 3:4,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotRecdevs(
  replist = model_output,
  subplots = 1:2,
  print = TRUE,
  minyr = 1950,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotIndices(
  replist = model_output,
  subplots = 2,
  print = TRUE,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotComps(
  replist = model_output,
  subplots = 21,
  kind = "AGE", 
  print = TRUE,
  plotdir = here::here("presentation", "model_and_diag",  "plots"),
  fleets = 1:6,
  maxrows = 2,
  pwidth = 6,
  pheight = 5
)
file.rename(here::here("presentation", "model_and_diag", "plots", "comp_agefit__aggregated_across_time.png"),
            here::here("presentation", "model_and_diag", "plots", "comp_agedat__aggregated_across_time_1-6.png"))
r4ss::SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  print = TRUE,
  plotdir = here::here("presentation", "model_and_diag", "plots"),
  fleets = 7:10,
  maxrows = 2,
  pwidth = 6,
  pheight = 5
)
file.rename(here::here("presentation", "model_and_diag", "plots", "comp_agefit__aggregated_across_time.png"),
            here::here("presentation", "model_and_diag", "plots", "comp_agedat__aggregated_across_time_7-10.png"))
r4ss::SSplotComps(
  replist = model_output,
  subplots = 9,
  kind = "AGE", 
  print = TRUE,
  plotdir = here::here("presentation", "model_and_diag",  "plots"),
  maxrows = 1,
  pwidth = 6,
  pheight = 5
)

#===============================================================================
# Bridging
#===============================================================================

pwidth = 6.5,
pheight = 5.0,


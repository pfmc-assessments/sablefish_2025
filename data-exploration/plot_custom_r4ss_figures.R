library(r4ss)

model <- SS_output(here::here("model", "base_model", "8.36_base_model"))
dir.create(here::here("model", "base_model", "8.36_base_model", "custom_plots"))
SS_fitbiasramp(
  replist = model, 
  shownew = FALSE,
  twoplots = FALSE,
  print = TRUE,
  plotdir = here::here("model", "base_model", "8.36_base_model", "custom_plots")
)
file.copy(
  from = here::here("model", "base_model", "8.36_base_model", "custom_plots", "recruit_fit_bias_adjust.png"),
  to = here::here("report", "figures")
)
SSplotPars(
  replist = model, 
  nrows = 1,
  ncols = 1,
  print = TRUE,
  plotdir = here::here("model", "base_model", "8.36_base_model", "custom_plots")
)
file.copy(
  from = here::here("model", "base_model", "8.36_base_model", "custom_plots", "parameter_distributions_pageNaN.png"),
  to = here::here("report", "figures")
)
SSplotComps(
  replist = model,
  kind = "AGE",
  fleets = 7,
  maxrows = 3,
  maxcols = 1,
  print = TRUE,
  plotdir = here::here("model", "base_model", "8.36_base_model", "custom_plots")
)
SSplotComps(
  replist = model,
  kind = "AGE",
  fleets = 8,
  maxrows = 2,
  maxcols = 1,
  print = TRUE,
  plotdir = here::here("model", "base_model", "8.36_base_model", "custom_plots")
)

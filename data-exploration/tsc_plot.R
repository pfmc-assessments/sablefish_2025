library(r4ss)
model <- SS_output(here::here("model", "base_model", "8.36_base_model"))
TSCplot(
  model,
  makePNG = file.path(model$inputs$dir, "plots", 'TSC_plot_sablefish_2025.png')
)

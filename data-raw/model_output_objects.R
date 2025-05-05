# 2023 Model for Comparisons
model_dir <- here::here("model", "_bridging", "0_2023_model")
model_output <- r4ss::SS_output(model_dir)
save(model_output, file = file.path(model_dir, "model_2023_output.rda"))

# Base Model for the report
model_name <- "0_init_hessian"
model_dir <- here::here("model", model_name)
model_output <- r4ss::SS_output(model_dir)
save(model_output, file = file.path(model_dir, "model_output.rda"))
r4ss::SS_plots(model_output)

r4ss::table_all(
  replist = model_output,
  fleetnames = c(
    "Trawl", "Hook-and-Line", "Pot",
    "Triennial", "AKFSC Slope", "NWFSC Slope", "WCGBT"
  )
)
r4ss::table_config(replist = model_output)

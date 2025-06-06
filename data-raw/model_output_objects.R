# 2023 Model for Comparisons
model_dir <- here::here("model", "_bridging", "0_2023_model")
model_output_2023 <- r4ss::SS_output(model_dir)
save(model_output_2023, file = here::here("data", "model_output_2023.rda"))

# Base Model for the report
model_name <- "8.34_fix_additional_selex_param_rec_dev_2"
model_dir <-  here::here("model", "base_model", model_name)
model_output <- r4ss::SS_output(model_dir)
save(model_output, file = here::here("data", "model_output.rda"))
r4ss::SS_plots(model_output)

r4ss::table_all(replist = model_output)
r4ss::table_config(replist = model_output)

library(r4ss)

model_2023 <- SS_output(
  dir = here::here("model", "_bridging", "0_2023_model"),
  verbose = FALSE,
  printstats = FALSE)

find <- grep("RecrDev", model_2023$parameters$Label)

years <- tidyr::extract_numeric(model_2023$parameters$Label[find])

recdevs <- model_2023$parameters[find, "Value"]

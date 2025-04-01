model_name <- "13_m_prior"
model_dir <- here::here("model", "_bridging", model_name)
model_output <- r4ss::SS_output(model_dir)
save(model_output, file = file.path(model_dir, "model_output.rda"))

write_r4ss_tables(
  replist = model_output,
  fleetnames = c(
    "Trawl", "Hook-and-Line", "Pot",
    "Triennial", "AKFSC Slope", "NWFSC Slope", "WCGBT"
  )
)

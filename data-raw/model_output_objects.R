
model_name <- "13_m_prior"
model_dir <- here::here("model", "_bridging", model_name)
model_output <- r4ss::SS_output(model_dir)
save(model_output, file = file.path(model_dir, "model_output.rda"))
r4ss::SS_plots(model_output)

# Alternative install the table branch for r4ss, should be merged soon into
# the main branch and at which point everyone will need to update the 
# package version.
devtools::load_all("C:/Users/chantel.wetzel/Documents/github/r4ss")
table_all(
  replist = model_output,
  fleetnames = c(
    "Trawl", "Hook-and-Line", "Pot",
    "Triennial", "AKFSC Slope", "NWFSC Slope", "WCGBT"
  )
)
table_config(replist = model_output)

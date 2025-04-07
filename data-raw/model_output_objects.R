
model_name <- "13_m_prior"
model_dir <- here::here("model", "_bridging", model_name)
model_output <- r4ss::SS_output(model_dir)
save(model_output, file = file.path(model_dir, "model_output.rda"))
r4ss::SS_plots(model_output)

devtools::load_all("C:/Users/chantel.wetzel/Documents/github/r4ss")
table_exec_summary(
  replist = model_output,
  fleetnames = c(
    "Trawl", "Hook-and-Line", "Pot",
    "Triennial", "AKFSC Slope", "NWFSC Slope", "WCGBT"
  )
)
pars <- table_pars(
  output = model_output
)
rownames(pars) <- NULL
save(pars, file = file.path(model_output$inputs$dir, "tables", "parameters.rda"))

n_pars <- table_parcounts(
  output = model_output
)
save(n_pars, file = file.path(model_output$inputs$dir, "tables", "n_parameters.rda"))

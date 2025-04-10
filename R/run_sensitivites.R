#' Run the following sensitivities:
#'
#' @param dir The directory where the base_model should be located
#' @param base_model The base name that sensitivities will be run for.
#' @param sens_dir The directory to save sensitivity runs 
#' @param data_file The name of the data file.
#' @param ctl_file The name of the control file.
#' @param verbose TRUE/FALSE to print messages to the screen.
#' 
#' @export
#' 
run_sensitivities <- function(
  dir,
  base_model,
  sens_dir = NULL,
  data_file = "data.ss",
  ctl_file = "control.ss",
  verbose = TRUE
) {
  nwfscSurvey::check_dir(dir = dir, verbose)
  check <- file.path(dir, base_model)
  if (!check) {
    cli::cli_inform("The base model is not found in the directory.")
  }
  if (is.null(sens_dir)) {
    sens_dir <- file.path(dir, "_sensitivities")
    cli::cli_inform("Sensitivies being run and saved in the _sensitivities folder in the {dir}.")
  }
  
  # Run data removal sensitivities
  data <- r4ss::SS_readdat(file = file.path(dir, base_model, data_file))
  fleetinfo <- data$fleetinfo
  num <-  data.frame(fleet = 1:dim(fleetinfo)[1])
  mod_fleetinfo <- dplyr::bind_cols(
    fleetinfo, num
  )
  surveys <- mod_fleetinfo |>
    dplyr::filter(type == 3) |>
    dplyr::select(fleet) 
  for(s in surveys) {
    add_name <- mod_fleetinfo |> dplyr::filter(fleet == s) |> dplyr::select(fleetname)
    run_name <- paste0(base_model, "_remove_", add_name)
    
    run_data_sensitivies(
      dir = dir,
      base_model = base_model,
      sens_dir = sens_dir,
      fleet = s,
      run_name = run_name,
      data_file = data_file,
      ctl_file = ctl_file,
      verbose = TRUE
    )
  }
  
  # Mirror M
  run_name <- paste0(base_model, "_mirror_m")
  sens_mirror_m(
    dir = dir,
    base_model = base_model,
    sens_dir = sens_dir,
    run_name = run_name,
    ctl_file = ctl_file,
    verbose = TRUE
  )
  
  # Alaska M
  cli::cli_inform("Setting M equal to the values used in the Alaska assessment.")
  run_name <- paste0(base_model, "_fix_m_alaska")
  sens_mirror_m(
    dir = dir,
    base_model = base_model,
    sens_dir = sens_dir,
    run_name = run_name,
    m_values = c(0.114, 0.114),
    ctl_file = ctl_file,
    verbose = TRUE
  )
  
  # Increase M by 20%
  cli::cli_inform("Increasing M by sex relative to the base value by 20%.")
  ctl <- r4ss::SS_readctl(file = file.path(dir, base_model, "control.ss_new"))
  row <- which(rownames(ctl$MG_parms) %in% c("NatM_p_1_Fem_GP_1", "NatM_p_1_Mal_GP_1"))
  model_m <- ctl$MG_parms[row, "INIT"] + 0.20 * ctl$MG_parms[row, "INIT"]
  run_name <- paste0(base_model, "_fix_m_higher_20")
  sens_mirror_m(
    dir = dir,
    base_model = base_model,
    sens_dir = sens_dir,
    run_name = run_name,
    m_values = model_m,
    ctl_file = ctl_file,
    verbose = TRUE
  )
  
  # Fix M by sex at the prior value
  cli::cli_inform("Fixing M by sex to the prior.")
  ctl <- r4ss::SS_readctl(file = file.path(dir, base_model, "control.ss_new"))
  row <- which(rownames(ctl$MG_parms) %in% c("NatM_p_1_Fem_GP_1", "NatM_p_1_Mal_GP_1"))
  model_m <- exp(ctl$MG_parms[row, "PRIOR"]) 
  run_name <- paste0(base_model, "_fix_m_prior")
  sens_mirror_m(
    dir = dir,
    base_model = base_model,
    sens_dir = sens_dir,
    run_name = run_name,
    m_values = model_m,
    ctl_file = ctl_file,
    verbose = TRUE
  )

  
  
}
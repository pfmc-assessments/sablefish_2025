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

  
  
}
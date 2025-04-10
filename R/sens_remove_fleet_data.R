#' Remove data sensitivies
#'
#' @param dir The directory where the base_model should be located
#' @param base_model The base name that sensitivities will be run for.
#' @param sens_dir The directory to save sensitivity runs 
#' @param fleet The fleet number that the index, length, and age data will be
#'   removed for.
#' @param run_name  The run folder name. 
#' @param data_file The name of the data file.
#' @param ctl_file The name of the control file.
#' @param verbose TRUE/FALSE to print messages to the screen.
#' 
#' @export
#' 
sens_remove_fleet_data <- function(
    dir,
    base_model,
    sens_dir,
    fleet,
    run_name,
    data_file,
    ctl_file,
    verbose = TRUE
) {
  copy_files <- function(x, from_name, to_name){
    file.copy(
      from = file.path(from_name,  x),
      to = file.path(to_name,  x)
    )
  }
  cli::cli_inform("Running data removal sensitivity for fleet {fleet}.")
  to_name <- file.path(sens_dir, run_name)
  dir.create(to_name)
  files <- list.files(file.path(dir, base_model))
  copy_files(
    x = files, 
    from_name = file.path(dir, base_model), 
    to_name = to_name)
  
  ctl <- r4ss::SS_readctl(file = file.path(to_name, ctl_file))
  remove_data <- data.frame(
    like_comp = c(1, 4, 5),
    fleet = fleet,
    phase = 1,
    value = 0,
    sizefreq_method = 1
  )
  colnames(remove_data) <- colnames(ctl$lambdas)
  ctl$lambdas <- dplyr::bind_rows(
    remove_data, ctl$lambdas
  )
  ctl$N_lambdas <- dim(ctl$lambdas)[1]
  r4ss::SS_writectl(
    ctllist = ctl, 
    outfile = file.path(to_name, ctl_file),
    overwrite = TRUE)
  setwd(file.path(to_name))
  shell("ss3 -nohess")
  
}
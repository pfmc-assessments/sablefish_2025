#' Mirror M between sexes sensitivies
#'
#' @param dir The directory where the base_model should be located
#' @param base_model The base name that sensitivities will be run for.
#' @param sens_dir The directory to save sensitivity runs 
#' @param run_name  The run folder name. 
#' @param ctl_file The name of the control file.
#' @param verbose TRUE/FALSE to print messages to the screen.
#' 
#' @export
#' 
sens_mirror_m <- function(
    dir,
    base_model,
    sens_dir,
    run_name,
    ctl_file,
    verbose = TRUE
) {
  copy_files <- function(x, from_name, to_name){
    file.copy(
      from = file.path(from_name,  x),
      to = file.path(to_name,  x)
    )
  }
  cli::cli_inform("Mirroring M between sexes.")
  to_name <- file.path(sens_dir, run_name)
  dir.create(to_name)
  files <- list.files(file.path(dir, base_model))
  copy_files(
    x = files, 
    from_name = file.path(dir, base_model), 
    to_name = to_name)
  
  ctl <- r4ss::SS_readctl(file = file.path(to_name, ctl_file))
  row <- which(rownames(ctl$MG_parms) == "NatM_p_1_Mal_GP_1")
  ctl$MG_parms[row, "INIT"] <- 0
  ctl$MG_parms[row, "PHASE"] <- -1
  r4ss::SS_writectl(
    ctllist = ctl, 
    outfile = file.path(to_name, ctl_file),
    overwrite = TRUE)
  setwd(file.path(to_name))
  shell("ss3 -nohess")
  
}
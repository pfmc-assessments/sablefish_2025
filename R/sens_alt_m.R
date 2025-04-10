#' Mirror M between sexes sensitivies
#'
#' @param dir The directory where the base_model should be located
#' @param base_model The base name that sensitivities will be run for.
#' @param sens_dir The directory to save sensitivity runs 
#' @param run_name  The run folder name. 
#' @param ctl_file The name of the control file.
#' @param m_values List of alternative m values for females and then males.
#' @param verbose TRUE/FALSE to print messages to the screen.
#' 
#' @export
#' 
sens_alt_m <- function(
    dir,
    base_model,
    sens_dir,
    run_name,
    ctl_file,
    m_values,
    verbose = TRUE
) {
  copy_files <- function(x, from_name, to_name){
    file.copy(
      from = file.path(from_name,  x),
      to = file.path(to_name,  x)
    )
  }
  cli::cli_inform("Fixing M for female and males to {m_values}.")
  to_name <- file.path(sens_dir, run_name)
  dir.create(to_name)
  files <- list.files(file.path(dir, base_model))
  copy_files(
    x = files, 
    from_name = file.path(dir, base_model), 
    to_name = to_name)
  
  ctl <- r4ss::SS_readctl(file = file.path(to_name, ctl_file))
  row <- which(rownames(ctl$MG_parms) %in% c("NatM_p_1_Fem_GP_1", "NatM_p_1_Mal_GP_1"))
  ctl$MG_parms[row, "INIT"] <- m_values
  ctl$MG_parms[row, "PHASE"] <- -1
  r4ss::SS_writectl(
    ctllist = ctl, 
    outfile = file.path(to_name, ctl_file),
    overwrite = TRUE)
  setwd(file.path(to_name))
  shell("ss3 -nohess")
  
}
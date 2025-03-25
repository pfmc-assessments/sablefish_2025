#' Create model rda
#'
#' @param base_model Base model folder name 
#' @param dir Directory where the base model is located
#' 
#' @export
create_rda <- function(
  base_model,
  dir) {
  model_dir <- file.path(dir, base_model)
  model_output <- r4ss::SS_output(model_dir, printstats = FALSE, verbose = FALSE) 
  save(
    model_output,
    file = file.path(model_dir, "model_output.rda")
  )
}

create_latex_tables <- function(
  base_model,
  dir,
  fleetnames = NULL,
  ci_value = 0.95
  ) {
  model_dir <- file.path(dir, base_model)
  table_dir <- file.path(dir, base_model, "tables")
  dir.create(table_dir, showWarnings = FALSE)
  model_output <- r4ss::SS_output(model_dir, printstats = FALSE, verbose = FALSE) 
  
  r4ss::SSexecutivesummary(
    replist = model_output,
    ci_value = ci_value,
    fleetnames = fleetnames,
    verbose = FALSE
  )
  sa4ss::es_table_tex(
    dir = model_dir, 
    table_folder = "tables", 
    save_loc = table_dir)
    
}
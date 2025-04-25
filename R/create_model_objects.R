#' Create r4ss plots required for the report document
#'
#' @param  dir Directory to save figures. The full directory is required and if 
#'   provided a folder will be created in this location where figures will be 
#'   saved.  
#' @param  model_name String model folder name.
#' @param  fleetnames List of user specific fleet names to use in the figures or 
#'   tables.
#'
#' @author Chantel Wetzel
#' @export
#'
create_model_plots <- function(
  model_name,
  dir = here::here("model"),
  fleetnames = c(
    "Trawl", 
    "Hook-and-Line", 
    "Pot",
    "Trawl Discards", 
    "Hook-and-Line Discards",
    "Pot Discard",
    "Triennial", 
    "AFSC Slope", 
    "NWFSC Slope", 
    "WCGBT",
    "Recruitment Index"
    )
  ) {

  model_dir <- file.path(dir, model_name)
  model_output <- r4ss::SS_output(model_dir)
  save(model_output, file = file.path(model_dir, "model_output.rda"))
  r4ss::SS_plots(
    replist = model_output,
    fleetnames = fleetnames
  )
}
#'
#' Create r4ss rda files required for the report document
#'
#' @param  dir Directory to save figures. The full directory is required and if 
#'   provided a folder will be created in this location where figures will be 
#'   saved.  
#' @param  model_name String model folder name.
#' @param  fleetnames List of user specific fleet names to use in the figures or 
#'   tables.
#'
#' @author Chantel Wetzel
#' @export
#'
create_model_tables <- function(
    model_name,
    dir = here::here("model"),
    fleetnames = c(
      "Trawl", 
      "Hook-and-Line", 
      "Pot",
      "Trawl Discards", 
      "Hook-and-Line Discards",
      "Pot Discard",
      "Triennial", 
      "AFSC Slope", 
      "NWFSC Slope", 
      "WCGBT",
      "Recruitment Index"
    )
) {
  
  model_dir <- file.path(dir, model_name)
  model_output <- r4ss::SS_output(model_dir)
  r4ss::table_all(
    replist = model_output,
    fleetnames = fleetnames
  )
  r4ss::table_config(replist = model_output)
}
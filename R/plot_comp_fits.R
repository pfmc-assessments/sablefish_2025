#' Create sex-specific implied fits to length and age data in ghost fleets (e.g., negative fleet number)
#'
#' @param  replist List of model objects created by `r4ss::SS_output()`.
#' @param  common_name Common name of the species in the GEMM database.
#' @param  dir Directory to save figures. The full directory is required and if 
#'   provided a folder will be created in this location where figures will be 
#'   saved.  The default is NULL which will create a folder to save special 
#'   figures inside the replist directory.
#'
#' @author Chantel Wetzel
#' @export
#'
plot_ghostfleets <- function(
  replist,
  dir = NULL) {
  
  plot_dir <- file.path(
    ifelse(
      is.null(dir),
      yes = replist[["inputs"]][["dir"]],
      no = dir
    ),
    "unsexed_ghostfleet_fits"
  )
  dir.create(plot_dir)
  
  model_mod <- replist
  model_mod$lendbase <- model_mod$ghostlendbase |> 
    dplyr::filter(Sexes == 0) |>
    dplyr::mutate(Pearson = 0, effN =  0, Used = "yes")
  SSplotComps(
    replist = model_mod, 
    print = TRUE,
    plotdir = plot_dir
  )
  
  model_mod$agedbase <- model_mod$ghostagedbase |> 
    dplyr::filter(Sexes == 0) |>
    dplyr::mutate(Pearson = 0, effN =  0, Used = "yes")
  SSplotComps(
    replist = model_mod,
    kind = "AGE", 
    print = TRUE,
    maxrows = 3,
    maxcols = 3,
    plotdir = plot_dir
  )
  
  plot_dir <- file.path(
    ifelse(
      is.null(dir),
      yes = replist[["inputs"]][["dir"]],
      no = dir
    ),
    "sexed_ghostfleet_fits"
  )
  dir.create(plot_dir)
  
  model_mod <- replist
  model_mod$lendbase <- model_mod$ghostlendbase |> 
    dplyr::filter(Sexes == 3) |>
    dplyr::mutate(Pearson = 0, effN =  0, Used = "yes")
  SSplotComps(
    replist = model_mod, 
    print = TRUE,
    plotdir = plot_dir
  )
  
  model_mod$agedbase <- model_mod$ghostagedbase |> 
    dplyr::filter(Sexes == 3) |>
    dplyr::mutate(Pearson = 0, effN =  0, Used = "yes")
  SSplotComps(
    replist = model_mod,
    kind = "AGE", 
    print = TRUE,
    maxrows = 3,
    maxcols = 3,
    plotdir = plot_dir
  )
}

#' Create sexed aggregated fits to marginal age data (e.g., do not include unsexed data)
#'
#' @param  replist List of model objects created by `r4ss::SS_output()`.
#' @param  dir Directory to save figures. The full directory is required and if 
#'   provided a folder will be created in this location where figures will be 
#'   saved.  The default is NULL which will create a folder to save special 
#'   figures inside the replist directory.
#' @param  years A vector of continous years to plot the aggregated fits for. Providing
#'   a vector of years to plot can be useful to investigate fits for a subset of years
#'   (e.g., block periods). The default is NULL which will plot the aggregated fleets for all years.
#'
#' @author Chantel Wetzel
#' @export
#'
plot_age_fits_sexed_only <- function(
  replist,
  dir = NULL,
  years = NULL) {
  
  if (is.null(years)) {
    years <- replist[["startyr"]]:replist[["endyr"]]
    year_name <- "_all_years"
  } else {
    year_name <- paste0("_", min(years), "-", max(years))
  }
  
  plot_dir <- file.path(
    ifelse(
      is.null(dir),
      yes = replist[["inputs"]][["dir"]],
      no = dir
    ),
    paste0("sexed_age_aggregated_fits", year_name)
  )
  dir.create(plot_dir)
  
  model_mod <- replist
  model_mod$agedbase <- model_mod$agedbase |> 
    dplyr::filter(Sexes == 3, Yr %in% years)
  SSplotComps(
    replist = model_mod,
    subplots = 21,
    kind = "AGE", 
    print = TRUE,
    plotdir = plot_dir
  )
}

#' Create sexed aggregated fits to length data (e.g., do not include unsexed data)
#'
#' @param  replist List of model objects created by `r4ss::SS_output()`.
#' @param  dir Directory to save figures. The full directory is required and if 
#'   provided a folder will be created in this location where figures will be 
#'   saved.  The default is NULL which will create a folder to save special 
#'   figures inside the replist directory.
#' @param  years A vector of continuous years to plot the aggregated fits for. Providing
#'   a vector of years to plot can be useful to investigate fits for a subset of years
#'   (e.g., block periods). The default is NULL which will plot the aggregated fleets for all years.
#'
#' @author Chantel Wetzel
#' @export
#'
plot_len_fits_sexed_only <- function(
    replist,
    dir = NULL,
    years = NULL) {
  
  if (is.null(years)) {
    years <- replist[["startyr"]]:replist[["endyr"]]
    year_name <- "_all_years"
  } else {
    year_name <- paste0("_", min(years), "-", max(years))
  }
  
  plot_dir <- file.path(
    ifelse(
      is.null(dir),
      yes = replist[["inputs"]][["dir"]],
      no = dir
    ),
    paste0("sexed_len_aggregated_fits", year_name)
  )
  dir.create(plot_dir)
  
  model_mod$lendbase <- model_mod$lendbase |> 
    dplyr::filter(Sexes == 3)
  SSplotComps(
    replist = model_mod,
    subplots = 21,
    kind = "LEN", 
    print = TRUE,
    plotdir = plot_dir
  )
}
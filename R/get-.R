#' Retrieve data from web-based resources
#'
#' @name get_data_survey
#' @param name A string providing the relevant population or species name.
#'   The vector must be of length one.
#' @param directory The path to the assessment package of interest. The default
#'   uses [here::here()] to locate the appropriate directory but you can set it
#'   to whatever directory you want as long as that directory contains
#'   `data-raw`.
#' @param remove_old A logical specifying if you want to delete the old data
#'   prior to downloading the new data. The default is `TRUE` because why
#'   would you need old data?
#' @return
#' `TRUE` is invisibly returned if the function is successful. Otherwise, an
#' error message is printed to the screen if the function is not successful.
#' Relevant files will be saved to the disk in the `data-raw` folder.
#' @export
get_data_survey <- function(
  name = "sablefish",
  directory = here::here(),
  remove_old = TRUE) {

  assertthat::assert_that(is.character(name))
  assertthat::assert_that(length(name) == 1)
  directory_raw <- fs::path(directory, "data-raw", "survey", "trawl")
  fs::dir_create(directory_raw)

  unlink(
    x = fs::dir_ls(
      path = directory_raw,
      regex = "catch_|bio_"
    ),
    force = TRUE
  )

  surveys <- c(
    "Triennial",
    "AFSC.Slope",
    "NWFSC.Slope",
    "NWFSC.Combo"
  )
  purrr::map(
    .x = surveys,
    .f = ~ nwfscSurvey::pull_bio(
      common_name = name,
      survey = .x,
      dir = directory_raw
    )
  )
  purrr::map(
    .x = surveys,
    .f = ~ nwfscSurvey::pull_catch(
      common_name = name,
      surve = .x,
      dir = directory_raw
    )
  )
  download_names <- fs::dir_ls(
    path = directory_raw,
    regex = paste0("[bc][ai][a-z]+_.+_", Sys.Date())
  )
  file.rename(
    from = download_names,
    to = gsub(paste0("_", Sys.Date()), "", download_names)
  )
  return(invisible(TRUE))
}

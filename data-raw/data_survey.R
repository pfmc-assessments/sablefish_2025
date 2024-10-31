#get_data_survey()

data_survey_catch <-
  fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "catch_") |>
  purrr::map(
    .f = function(x) {load(x); return(x)}
  )

data_survey_bio <- fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_") |>
  purrr::map(
    .f = function(x) {
      load(x)
      if (inherits(x, "list")) {
        data <- dplyr::bind_rows(x)
      }
      return(data)
    }
  )

usethis::use_data(
  data_survey_catch,
  data_survey_bio,
  overwrite = TRUE
)

#get_data_survey()

data_survey_catch <-
  fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "catch_") |>
  purrr::map(
    .f = function(x) {load(x); return(x)}
  )
#data_survey_bio <- fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[2:3] |>
#  purrr::map(
#    .f = function(x) {
#      load(x)
#      if (inherits(x, "list")) {
#        data <- dplyr::bind_rows(x)
#      }
#      return(data)
#    }
#  )
nwfsc_combo_bio <- fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[2] |>
  purrr::map(
    .f = function(x) {
    load(x)
    return(x)
  })
nwfsc_slope_bio <- fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[3] |>
  purrr::map(
    .f = function(x) {
      load(x)
      return(x)
    })
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[4]) 
tri_bio <- x
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[1]) 
afsc_bio <- x
data_survey_bio <- list()
data_survey_bio$nwfsc_combo <- nwfsc_combo_bio 
data_survey_bio$nwfsc_slope <- nwfsc_slope_bio 
data_survey_bio$afsc_slope <- afsc_bio 
data_survey_bio$triennial <- tri_bio

usethis::use_data(
  data_survey_catch,
  data_survey_bio,
  overwrite = TRUE
)

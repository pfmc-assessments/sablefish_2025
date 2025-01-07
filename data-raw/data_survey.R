#get_data_survey()

load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "catch_")[1])
afsc_slope <- x |>
  dplyr::filter(Year >= 1997)
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "catch_")[2])
nwfsc_combo <- x
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "catch_")[3])
nwfsc_slope <- x
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "catch_")[4])
triennial <- x |>
  dplyr::filter(Year >= 1980)
data_survey_catch <- list()
data_survey_catch$nwfsc_combo <- nwfsc_combo 
data_survey_catch$nwfsc_slope <- nwfsc_slope
data_survey_catch$afsc_slope <- afsc_slope 
data_survey_catch$triennial <- triennial

load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[2])
nwfsc_combo_bio <- x
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[3])
nwfsc_slope_bio <- x
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[4]) 
tri_bio <- x
tri_bio$length_data <- tri_bio$length_data |> dplyr::filter(Year >= 1980) 
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[1]) 
afsc_bio <- x
afsc_bio$length_data <- afsc_bio$length_data |> dplyr::filter(Year >= 1997)
afsc_bio$age_data <- afsc_bio$age_data |> dplyr::filter(Year >= 1997)
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

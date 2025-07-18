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
data_survey_catch$triennial_early <- triennial |> dplyr::filter(Year <= 1992)
data_survey_catch$triennial_late <- triennial |> dplyr::filter(Year > 1992)

load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[2])
nwfsc_combo_bio <- x
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[3])
nwfsc_slope_bio <- x
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[4]) 
tri_bio <- x
tri_bio_early <- tri_bio_late <- list()
tri_bio_early$length_data <- tri_bio$length_data |> dplyr::filter(Year >= 1980, Year <= 1992)
tri_bio_early$age_data <- tri_bio$age_data |> dplyr::filter(Year >= 1980, Year <= 1992)
tri_bio_early$length_data$Project <- "triennial_early"
tri_bio_early$age_data$Project <- "triennial_early"

tri_bio_late <- x
tri_bio_late$length_data <- tri_bio$length_data |> dplyr::filter(Year > 1992)
tri_bio_late$age_data <- tri_bio$age_data |> dplyr::filter(Year > 1992)
tri_bio_late$length_data$Project <- "triennial_late"
tri_bio_late$age_data$Project <- "triennial_late"
load(fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_")[1]) 
afsc_bio <- x
afsc_bio$length_data <- afsc_bio$length_data |> dplyr::filter(Year >= 1997)
afsc_bio$age_data <- afsc_bio$age_data |> dplyr::filter(Year >= 1997)
data_survey_bio <- list()
data_survey_bio$nwfsc_combo <- nwfsc_combo_bio 
data_survey_bio$nwfsc_slope <- nwfsc_slope_bio 
data_survey_bio$afsc_slope <- afsc_bio 
data_survey_bio$triennial_early <- tri_bio_early
data_survey_bio$triennial_late <- tri_bio_late

usethis::use_data(
  data_survey_catch,
  data_survey_bio,
  overwrite = TRUE
)

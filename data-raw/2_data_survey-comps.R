# TODO: Evaluate how to incorporate targets here to evaluate when things 
# need to be rerun.
library(nwfscSurvey)

process_survey(
  catch_data = data_survey_catch$nwfsc_combo,
  bds_data = data_survey_bio$nwfsc_combo,
  strata = strata_combo,
  age_bins = age_bins,
  length_bins = len_bins,
  save_dir = "data-processed",
  fig_table_dir = here::here("data-raw", "survey", "trawl")
)

process_survey(
  catch_data = data_survey_catch$nwfsc_slope,
  bds_data = data_survey_bio$nwfsc_slope |> dplyr::filter(Sex != "U"),
  strata = strata_nwfsc_slope,
  age_bins = age_bins,
  length_bins = len_bins,
  save_dir = "data-processed",
  fig_table_dir = here::here("data-raw", "survey", "trawl")
)

# Removing the AFSC Slope survey due to unrepresentative sampling between lengths and ages
# process_survey(
#   catch_data = data_survey_catch$afsc_slope,
#   bds_data = data_survey_bio$afsc_slope,
#   strata = strata_afsc_slope,
#   age_bins = age_bins,
#   length_bins = len_bins,
#   save_dir = "data-processed",
#   fig_table_dir = here::here("data-raw", "survey", "trawl")
# )

process_survey(
  catch_data = data_survey_catch$triennial_early,
  bds_data = data_survey_bio$triennial_early,
  strata = strata_triennial_early,
  age_bins = age_bins,
  length_bins = len_bins,
  save_dir = "data-processed",
  fig_table_dir = here::here("data-raw", "survey", "trawl")
)

process_survey(
  catch_data = data_survey_catch$triennial_late,
  bds_data = data_survey_bio$triennial_late,
  strata = strata_triennial_late,
  age_bins = age_bins,
  length_bins = len_bins,
  save_dir = "data-processed",
  fig_table_dir = here::here("data-raw", "survey", "trawl")
)





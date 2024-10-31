# Pull NWFSC Survey Data
catch <- nwfscSurvey::pull_catch(
  common_name = "sablefish",
  survey = "NWFSC.Combo",
  dir = here::here("data-raw")
)
catch <- nwfscSurvey::pull_catch(
  common_name = "sablefish",
  survey = "Triennial",
  dir = here::here("data-raw")
)
catch <- nwfscSurvey::pull_catch(
  common_name = "sablefish",
  survey = "NWFSC.Slope",
  dir = here::here("data-raw")
)
catch <- nwfscSurvey::pull_catch(
  common_name = "sablefish",
  survey = "AFSC.Slope",
  dir = here::here("data-raw")
)
bio <- nwfscSurvey::pull_bio(
  common_name = "sablefish",
  survey = "NWFSC.Combo",
  dir = here::here("data-raw")
)
bio <- nwfscSurvey::pull_bio(
  common_name = "sablefish",
  survey = "NWFSC.Slope",
  dir = here::here("data-raw")
)
bio <- nwfscSurvey::pull_bio(
  common_name = "sablefish",
  survey = "AFSC.Slope",
  dir = here::here("data-raw")
)
bio <- nwfscSurvey::pull_bio(
  common_name = "sablefish",
  survey = "Triennial",
  dir = here::here("data-raw")
)

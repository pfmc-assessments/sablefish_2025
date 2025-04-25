col_names <- c(
  "assessment_year",
  "a1", "a2", "first_mature_age",
  "m_female","k_female", "lmin_female", "lmax_female", "cv_young_female", "cv_old_female",
  "m_male","k_male", "lmin_male", "lmax_male", "cv_young_male", "cv_old_male"
)
data_historical_biology <- cbind(
c(2023,2021,2019,2015,2011),
c(0.5,0.5,0.5,0.5,0.5),
c(30,30,30,30,30),
c(3,3,3,3,3),
c(0.0711184,0.0725861,0.0758634,0.0757999,0.0798815),
c(25.2617,25.7207,25.1516,26.149,25.8181),
c(61.1301,62.4569,62.6737,64.2267,63.9794),
c(0.366898,0.343282,0.34379,0.326784,0.334585),
c(0.0580896,0.0572535,0.0606818,0.078497,0.0807421),
c(0.103294,0.109531,0.110045,0.118391,0.121029),
c(0.059237,0.060472,0.0675262,0.0615699,0.0649092),
c(26.6207,26.926,25.5019,26.149,25.8181),
c(56.1106, 56.6228,56.3704,56.2739,56.1653),
c(0.380761,0.371287,0.400115,0.415657,0.418791),
c(0.0704857,0.0749235,0.0663632,0.078497,0.0807421),
c(0.0780941,0.0783725,0.0796672,0.0779401,0.0780549)
)

colnames(data_historical_biology) <- col_names

usethis::use_data(
  data_historical_biology,
  overwrite = TRUE
)
rm(data_historical_biology, col_names)


cols_to_keep <- c(
  "Sex",
  "Length_cm", 
  "Weight_kg"
)

weight_length <- dplyr::bind_rows(
  data_survey_bio$nwfsc_combo |> dplyr::select(dplyr::all_of(cols_to_keep)), 
  data_survey_bio$nwfsc_slope |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$afsc_slope$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$triennial_early$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$triennial_late$age_data |> dplyr::select(dplyr::all_of(cols_to_keep))
)

# Estimate weight-length relationship by sex
weight_length_estimates <- nwfscSurvey::estimate_weight_length(
  data = weight_length,
  verbose = FALSE
)[1:2, c("sex", "A", "B")]

usethis::use_data(
  weight_length_estimates,
  overwrite = TRUE
)

cols_to_keep <- c(
  "Sex",
  "Age_years", 
  "Length_cm"
)

age_length <- dplyr::bind_rows(
  data_survey_bio$nwfsc_combo |> dplyr::select(dplyr::all_of(cols_to_keep)), 
  data_survey_bio$nwfsc_slope |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$afsc_slope$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$triennial_early$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$triennial_late$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_commercial_bds |> dplyr::select(length_cm, age_years, sex) |>
    dplyr::rename(Length_cm = length_cm, Age_years = age_years, Sex = sex)
) 

growth <- nwfscSurvey::est_growth(
  dat = age_length |> 
    dplyr::filter(Sex != "U") |> dplyr::mutate(Age = Age_years),
  return_df = FALSE
)

usethis::use_data(
  growth,
  overwrite = TRUE
)
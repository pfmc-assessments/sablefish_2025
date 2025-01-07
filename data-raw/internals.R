
assessment_dir <- fs::path("report")
common_name <- "sablefish"
latin_name <- "Anoplopoma fimbria"
len_bins <- seq(18, 90, by = 2)
age_bins <- 0:50
gears <- c("HKL", "POT", "TWL")

# Create stratification areas from the SA3 file, which is in {nwfscSurvey}
# same as 2011 assessment, also used for 2019
strata_combo <- nwfscSurvey::CreateStrataDF.fn(
  names = paste(
    sep = "_",
    rep(times = 4, c("55m-183m", "183m-549m", "549m-900m", "900m-1280m")),
    rep(each = 4, c("32-34.5", "34.5-40.5", "40.5-45", "45-49"))
  ),
  depths.shallow = rep(times = 4, x = c(55, 183, 549, 900)),
  depths.deep = rep(times = 4, x = c(183, 549, 900, 1280)),
  lats.south = rep(each = 4, x = c(32, 34.5, 40.5, 45)),
  lats.north = rep(each = 4, x = c(34.5, 40.5, 45, 49))
)
strata_nwfsc_slope <- nwfscSurvey::CreateStrataDF.fn(
  names = paste(
    sep = "_",
    rep(times = 3, c("183m-549m", "549m-900m", "900m-1280m")),
    rep(each = 3, c("34.5-40.5", "40.5-45", "45-49"))
  ),
  depths.shallow = rep(times = 3, x = c(183, 549, 900)),
  depths.deep = rep(times = 3, x = c(549, 900, 1280)),
  lats.south = rep(each = 3, x = c(34.5, 40.5, 45)),
  lats.north = rep(each = 3, x = c(40.5, 45, 49))
)
strata_afsc_slope <- nwfscSurvey::CreateStrataDF.fn(
  names = paste(
    sep = "_",
    rep(times = 3, c("183m-549m", "549m-900m", "900m-1280m")),
    rep(each = 3, c("34.5-40.5", "40.5-45", "45-49"))
  ),
  depths.shallow = rep(times = 3, x = c(183, 549, 900)),
  depths.deep = rep(times = 3, x = c(549, 900, 1280)),
  lats.south = rep(each = 3, x = c(34.5, 40.5, 45)),
  lats.north = rep(each = 3, x = c(40.5, 45, 49))
)
strata_triennial_early <- nwfscSurvey::CreateStrataDF.fn(
  names = paste(
    sep = "_",
    rep(times = 3, c("55-183mm", "183m-350m")),
    rep(each = 2, c("36.0-40.5", "40.5-45", "45-49"))
  ),
  depths.shallow = rep(times = 3, x = c(55, 183)),
  depths.deep = rep(times = 3, x = c(183, 350)),
  lats.south = rep(each = 2, x = c(36, 40.5, 45)),
  lats.north = rep(each = 2, x = c(40.5, 45, 49))
)
strata_triennial_late <- nwfscSurvey::CreateStrataDF.fn(
  names = paste(
    sep = "_",
    rep(times = 3, c("55-183mm", "183m-549m")),
    rep(each = 2, c("34.5-40.5", "40.5-45", "45-49"))
  ),
  depths.shallow = rep(times = 3, x = c(55, 183)),
  depths.deep = rep(times = 3, x = c(183, 549)),
  lats.south = rep(each = 2, x = c(34.5, 40.5, 45)),
  lats.north = rep(each = 2, x = c(40.5, 45, 49))
)

usethis::use_data(
  age_bins,
  assessment_dir,
  common_name,
  latin_name,
  len_bins,
  strata_combo,
  strata_nwfsc_slope,
  strata_afsc_slope,
  strata_triennial_early,
  strata_triennial_late,
  internal = TRUE,
  overwrite = TRUE
)
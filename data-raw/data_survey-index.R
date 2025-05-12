# Determine the best model and the format all indices for SS3

# TODO:
# 1. Create code to pull out diagnostics by survey and error distribution

#=============================================
# Triennial early 
#=============================================
# delta gamma: AIC = -5396, NLL = -2717 - XX
# delta lognormal: AIC = -5534, NLL = -2786
#=============================================
# Triennial late
#=============================================
# delta gamma: AIC = -4380, NLL = -2207 - XX
# delta lognormal: AIC = -4402, NLL = -2218
#=============================================
# AFSC Slope
#=============================================
# tweedie
#=============================================
# NWFSC Slope
#=============================================
# delta gamma: bad sanity
# delta lognormal: XX
#=============================================
# WCGBT
#=============================================
# delta gamma: AIC = -42671, NLL = -21390 - XX
# delta lognormal: AIC = -43444, NLL = -21777

# Read in the estimated index for each survey
index_dir <- here::here("data-raw", "survey", "trawl", "indices")

triennial <- dplyr::bind_rows(
  read.csv(file.path(index_dir, "triennial_early", "delta_gamma", "index", "est_by_area.csv")),
  read.csv(file.path(index_dir, "triennial_late", "delta_gamma", "index", "est_by_area.csv"))) |>
  dplyr::filter(area == "Coastwide") |> 
  dplyr::select(year, est, se) |>
  dplyr::mutate(
    month = 7,
    fleet = recode_fleet_cw("triennial"),
    est = round(est, 0),
    se = round(se, 4)
  ) |>
  dplyr::relocate(
    fleet, .before = est
  ) |>
  dplyr::relocate(
    month, .before = fleet
  )

#afsc_slope <- 
#  read.csv(file.path(index_dir, "afsc_slope", "tweedie", "index", "est_by_area.csv")) |>
#  dplyr::filter(area == "Coastwide") |> 
#  dplyr::select(year, est, se) |>
#  dplyr::mutate(
#    month = 7,
#    fleet = recode_fleet_cw("afsc_slope"),
#    est = round(est, 0),
#    se = round(se, 4)
#  ) |>
#  dplyr::relocate(
#    fleet, .before = est
#  ) |>
#  dplyr::relocate(
#    month, .before = fleet
#  )

nwfsc_slope <- 
  read.csv(file.path(index_dir, "nwfsc_slope", "delta_lognormal", "index", "est_by_area.csv")) |>
  dplyr::filter(area == "Coastwide") |> 
  dplyr::select(year, est, se) |>
  dplyr::mutate(
    month = 7,
    fleet = recode_fleet_cw("nwfsc_slope"),
    est = round(est, 0),
    se = round(se, 4)
  ) |>
  dplyr::relocate(
    fleet, .before = est
  ) |>
  dplyr::relocate(
    month, .before = fleet
  )
  
wcgbt <- 
  read.csv(file.path(index_dir, "wcgbts", "delta_gamma", "index", "est_by_area.csv")) |>
  dplyr::filter(area == "Coastwide") |> 
  dplyr::select(year, est, se) |>
  dplyr::mutate(
    month = 7,
    fleet = recode_fleet_cw("wcgbt"),
    est = round(est, 0),
    se = round(se, 4)
  ) |>
  dplyr::relocate(
    fleet, .before = est
  ) |>
  dplyr::relocate(
    month, .before = fleet
  )

data_survey_indices <- dplyr::bind_rows(
  triennial,
  nwfsc_slope,
  wcgbt
)   

utils::write.csv(
  data_survey_indices,
  file = here::here("data-processed", "data-survey-indices.csv"),
  row.names = FALSE
)
  



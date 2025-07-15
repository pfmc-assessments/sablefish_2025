library(dplyr)
library(ggplot2)
library(pacfintools)

raw_pacfin_bds <-
  fs::dir_ls(here::here("data-raw", "bds"), regex = "PacFIN\\..+bds") |>
  purrr::map_df(
    .f = function(x) {load(x); return(bds.pacfin)}
  ) |>
  tibble::tibble()

good_lengths <- c("U", "A", "F")
# Keep only random (R) samples based on SAMPLE_METHOD_CODE. Only random samples
# should be retained unless specified by a state agency.
good_methods <- "R"
# Keep commercial on-board (C), market (M), and blank samples based on SAMPLE_TYPE
good_samples <- c("", "M", "C", "S")
# Keep data from all 3 states. This is the default.
good_states <- c("WA", "OR", "CA")
# Keep only break and burn (B, BB), unknown (U), and blank (") age reads based 
# on AGE_METHOD. This decision should be species-specific.
# The CAP lab generally has done nearly all sablefish aging, and they seem to 
# be doing surface reading intermittently, so these by default have been kept.
good_age_method <- c("B", "BB", "S", "M", "U", "")
# TODO: evaluate all available age methods types. 
# 1. What is the prevalence of each? state? years?
# 2. There are a large number ages with blank (or NA) age method primarily in 
# Oregon. Make sure these ages look similar to those that are marked BB.  These
# ages should ideally be kept due to the number of samples.
# 3. Determine if any age method types should be kept beyond the default of BB. 
# Often S age methods are tossed, should we do that here?

bds_cleaned <- pacfintools::cleanPacFIN(
  Pdata = raw_pacfin_bds |> dplyr::filter(SAMPLE_YEAR != 2025),
  keep_gears = gears,
  CLEAN = TRUE,
  keep_age_method = c("B", "BB", "S", "M", "U"),
  keep_sample_type = c("", "M", "C"),
  keep_sample_method = good_methods,
  keep_length_type = good_lengths,
  keep_states = good_states,
  spp = "sablefish"
) |>
  dplyr::mutate(
    stratification = paste(state, geargroup, sep = "."),
    weightkg = dplyr::case_when(
      FISH_WEIGHT_UNITS == "P" ~ FISH_WEIGHT * 0.453592, 
      .default = weightkg
    )
  )

port_lats <- PEPtools::pacfin_ports_withlatlong |>
  dplyr::rename(
    PCID = pcid
  ) |>
  dplyr::select(
    c(-name, -agencydesc, -agid)
  ) |>
  dplyr::distinct(PCID, .keep_all = TRUE) |>
  tibble::tibble()

data_commercial_bds_filter <- bds_cleaned |>
  dplyr::mutate(
    pacfin_port_code = dplyr::case_when(is.na(PCID) ~ "UKN", PCID == "XXX" ~ "UKN", .default = PCID)
  ) |>
  dplyr::left_join(
    y = port_lats
  ) |>
  dplyr::mutate(
    area = dplyr::case_when(
      latitude > 36 ~ "North", 
      is.na(latitude) ~ "Unknown", 
      .default = "South"),
    area = dplyr::case_when(
      area == "Unknown" & state %in% c("OR", "WA") ~ "North",
      .default = area
    )
  ) |>
  dplyr::select(year, state, geargroup, area, lengthcm, Age, SEX) |>
  dplyr::rename(
    length_cm = lengthcm,
    age_years = Age,
    sex = SEX
  ) 
usethis::use_data(
  data_commercial_bds_filter,
  overwrite = TRUE
)

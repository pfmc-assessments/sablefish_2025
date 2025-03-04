#' Process PacFIN bds data
#'
#' @param bds_data Data file of th PacFIN bds data. 
#' @param catch_file Name of the catch file to be used to expand biological data.
#'   This file should be located within `data-processed` with the `here` package
#'   used to find the file in the directory.
#' @param age_bins Numerical sequence of age bins to process age data.
#' @param length_bins  Numerical sequence of length bins to process length data.
#' @param gears Gear types to group the data within.  This should align with 
#'   PacFIN gear names.
#' @param common_name Argument input for pacfintools::cleanPacFIN() that defines
#'   the gear-area fleet structure (coastwide HKL, Pot, and TWL).
#' 
#' @export
process_bds_data <- function(
  bds_data,
  catch_file = "data_commercial_catch_expansions.csv",
  age_bins = age_bins,
  length_bins = len_bins,
  gears = gears,
  common_name = common_name) {
  if (!file.exists(here::here("data-raw", "bds"))) {
    dir.create(here::here("data-raw", "bds"))
    dir.create(here::here("data-raw", "bds", "plots"))
  }
  # pre-processed catch data by year, state, and geargroup
  file_catch <- here::here("data-processed",  catch_file)
  
  # The maximum quantile at which to cap all data expansions within getExpansion_1().
  expansion <- 0.95
  
  # Determine what samples to retain or filter out when using cleanPacFIN()
  # Keep all alternate (A), fork (F), and unknown (U) length types based on 
  # FISH_LENGTH_TYPE_CODE. This decision should be species-specific
  good_lengths <- c("U", "A", "F")
  # Keep only random (R) samples based on SAMPLE_METHOD_CODE. Only random samples
  # should be retained unless specified by a state agency.
  good_methods <- "R"
  # Keep commercial on-board (C), market (M), and blank samples based on SAMPLE_TYPE
  good_samples <- c("", "M", "C")
  # Keep data from all 3 states. This is the default.
  good_states <- c("WA", "OR", "CA")
  # Keep only break and burn (B, BB), unknown (U), and blank (") age reads based 
  # on AGE_METHOD. This decision should be species-specific.
  good_age_method <- c("B", "BB", "U", "")
  # TODO: evaluate all available age methods types. 
  # 1. What is the prevalence of each? state? years?
  # 2. There are a large number ages with blank (or NA) age method primarily in 
  # Oregon. Make sure these ages look similar to those that are marked BB.  These
  # ages should ideally be kept due to the number of samples.
  # 3. Determine if any age method types should be kept beyond the default of BB. 
  # Often S age methods are tossed, should we do that here?
  
  # Load the survey data 
  bds_survey <- data_survey_bio$nwfsc_combo
  # Estimate weight-length relationship by sex
  weight_length_estimates <- nwfscSurvey::estimate_weight_length(
    bds_survey,
    verbose = FALSE
  )
  
  catch_formatted <- utils::read.csv(file_catch) |>
    formatCatch(
      strat = c("state", "geargroup"),
      valuename = "catch_mt"
    )
  
  bds_cleaned <- cleanPacFIN(
    Pdata = bds_data |> dplyr::filter(SAMPLE_YEAR != 2025),
    keep_gears = gears,
    CLEAN = TRUE,
    keep_age_method = good_age_method,
    keep_sample_type = good_samples,
    keep_sample_method = good_methods,
    keep_length_type = good_lengths,
    keep_states = good_states,
    spp = common_name
  ) |>
    dplyr::mutate(
      stratification = paste(state, geargroup, sep = ".")
    )
  
  saveRDS(
    bds_cleaned, 
    file =  here::here("data-raw", "bds", paste0("cleaned_pacfin_bds.rds"))
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
  
  data_commercial_bds <- bds_cleaned |>
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
    data_commercial_bds,
    overwrite = TRUE
  )
  
  samples <- bds_cleaned |>
    dplyr::group_by(geargroup, year) |>
    dplyr::summarise(
      `N Lengthed` = sum(!is.na(lengthcm)),
      `N Aged` = sum(!is.na(Age))
    ) |>
    dplyr::rename(
      Year = year,
      Gear = geargroup
    ) |>
    as.data.frame()
  utils::write.csv(
    samples,
    file = here::here("data-processed", paste0("data-fishery-bds-n.csv")),
    row.names = FALSE
  )
  
  raw_samples_by_gear_state <- bds_cleaned |>
    dplyr::group_by(year, state, geargroup) |>
    dplyr::summarise(
      n_length = sum(!is.na(lengthcm)),
      n_age = sum(!is.na(Age))
    ) 
  samples_by_gear_state <- raw_samples_by_gear_state |>
    tidyr::pivot_wider(
      names_from = state,
      values_from = c(n_length, n_age),
      values_fill = 0
    ) |>
    dplyr::relocate(
      n_length_WA,
      .after = n_length_OR
    ) |>
    dplyr::relocate(
      n_age_WA,
      .after = n_age_OR
    ) |>
    dplyr::rename(
      Year = year,
      Gear = geargroup,
      `N Lengthed (WA)` = n_length_WA,
      `N Lengthed (OR)` = n_length_OR,
      `N Lengthed (CA)` = n_length_CA,
      `N Aged (WA)` = n_age_WA,
      `N Aged (OR)` = n_age_OR,
      `N Age (CA)` = n_age_CA,
    ) |>
    dplyr::arrange(Gear, Year)|>
    as.data.frame()
  utils::write.csv(
    samples_by_gear_state,
    file = here::here("data-processed", paste0("data-fishery-bds-n-state-gear.csv")),
    row.names = FALSE
  )
  
  gg1 <- ggplot2::ggplot(raw_samples_by_gear_state, 
    ggplot2::aes(x = year, y = n_length, fill = state)) +
    ggplot2::geom_bar(position="stack", stat="identity") +
    ggplot2::theme_bw() +
    ggplot2::ylab("# of Lengths") + ggplot2::xlab("Year") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::facet_grid(geargroup~.)
  ggplot2::ggsave(
    gg1, 
    filename = here::here("data-raw", "bds", "plots", "bds_lengths_by_year_gear.png"),
    height = 7, width = 7
  )
  gg2 <- ggplot2::ggplot(raw_samples_by_gear_state |> dplyr::filter(year > 1985), 
    ggplot2::aes(x = year, y = n_age, fill = state)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::theme_bw() +
    ggplot2::ylab("# of Ages") + ggplot2::xlab("Year") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::facet_grid(geargroup~.)
  ggplot2::ggsave(
    gg2, 
    filename = here::here("data-raw", "bds", "plots", "bds_ages_by_year_gear.png"),
    height = 7, width = 7
  )
  
  bds_modified <- bds_cleaned |>
    # Set any length with an age to NA to avoid double use of data
    # in the model via marginals.
    dplyr::mutate(
      lengthcm = dplyr::case_when(
        !is.na(Age) ~ NA, is.na(Age) ~ lengthcm
      )
    )
  
  expanded_comps <- get_pacfin_expansions(
    Pdata = bds_modified,
    Catch = catch_formatted,
    weight_length_estimates = weight_length_estimates,
    Units = "MT",
    maxExp = expansion,
    Exp_WA = TRUE,
    verbose = FALSE
  )
  
  length_comps_long <- getComps(
    Pdata = expanded_comps |> dplyr::filter(!is.na(lengthcm)),
    Comps = "LEN",
    weightid = "Final_Sample_Size_L",
    verbose = FALSE
  )
  
  length_composition_data <- writeComps(
    inComps = length_comps_long,
    column_with_input_n = "n_stewart",
    comp_bins = length_bins,
    verbose = FALSE
  ) |>
    dplyr::mutate(
      fleet = recode_fleet_cw(fleet)
    ) |>
    dplyr::arrange(fleet)
  
  age_comps_long <- getComps(
    Pdata =  dplyr::filter(expanded_comps, !is.na(Age)),
    Comps = "AGE",
    weightid = "Final_Sample_Size_A",
    verbose = FALSE
  )
  
  age_composition_data <- writeComps(
    inComps = age_comps_long,
    comp_bins = age_bins,
    column_with_input_n = "n_stewart",
    verbose = FALSE
  ) |>
    dplyr::mutate(
      fleet = recode_fleet_cw(fleet),
      ageerr = 1,
    ) |>
    dplyr::arrange(fleet)

  utils::write.csv(
    age_composition_data,
    file = here::here("data-processed",
        glue::glue("data-commercial-comps_age-{min(age_bins)}-{max(age_bins)}.csv")),
    row.names = FALSE
  )
  
  utils::write.csv(
    length_composition_data,
    file = here::here("data-processed",
        glue::glue("data-commercial-comps-length-{min(length_bins)}-{max(length_bins)}.csv")),
    row.names = FALSE
  )

}



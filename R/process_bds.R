#' Process PacFIN bds data
#'
#' @param save_dir The path to the assessment package of interest. The default
#'   uses [here::here()] to locate the appropriate directory but you can set it
#'   to whatever directory you want as long as that directory contains
#'   `data-raw`.
#' @param bds_file File name of the PacFIN bds file to process. 
#' @param catch_file Name of the catch file to be used to expand biological data.
#'   This file should be located within `data-processed` with the `here` package
#'   used to find the file in the directory.
#' @param age_bins Numerical sequence of age bins to process age data.
#' @param length_bins  Numerical sequence of length bins to process length data.
#' @param gears Gear types to group the data within.  This should align with 
#'   PacFIN gear names.
#' @param species_code String that is used to name the saved length and age 
#'   composition files.
#' @param common_name
#' 
#' @export
process_bds_data <- function(
  save_dir = here::here("data-raw", "bds"),
  bds_file,
  catch_file = "data_commercial_catch_expansions.csv",
  age_bins = age_bins,
  length_bins = len_bins,
  gears = gears,
  species_code = "SABL",
  common_name = common_name) {
  # bds.pacfin
  load(file.path(save_dir, bds_file))
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
  
  # Pull survey data 
  bds_survey <- nwfscSurvey::pull_bio(
    common_name = common_name,
    survey = "NWFSC.Combo"
  )
  # Estimate weight-length relationship by sex
  weight_length_estimates <- nwfscSurvey::estimate_weight_length(
    bds_survey,
    verbose = FALSE
  )
  
  catch_formatted <- utils::read.csv(file_catch) |>
    PacFIN.Utilities::formatCatch(
      strat = c("state", "geargroup"),
      valuename = "catch_mt"
    )
  
  bds_cleaned <- PacFIN.Utilities::cleanPacFIN(
    Pdata = bds.pacfin,
    keep_gears = gears,
    CLEAN = TRUE,
    keep_age_method = good_age_method,
    keep_sample_type = good_samples,
    keep_sample_method = good_methods,
    keep_length_type = good_lengths,
    keep_states = good_states,
    spp = common_name,
    savedir = save_dir
  ) |>
    dplyr::mutate(
      stratification = paste(state, geargroup, sep = ".")
    )
  save(
    bds_cleaned, 
    file = file.path(save_dir, paste0("cleaned_", bds_file))
  )
  # TODO: 
  # 1. determine if any data filtering should be done to remove outliers by plotting
  # age and length comparisons.  The nwfscSurvey::est_growth function can help with this.
  # 2. evaluate whether the correct age methods are returned and whether they 
  # are appropriate.
  
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
    file = here::here(save_dir, paste0("data-fishery-bds-n.csv")),
    row.names = FALSE
  )
  
  samples_by_gear_state <- bds_cleaned |>
    dplyr::group_by(year, state, geargroup) |>
    dplyr::summarise(
      n_length = sum(!is.na(lengthcm)),
      n_age = sum(!is.na(Age))
    ) |>
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
    file = here::here(save_dir, paste0("data-fishery-bds-n-state-gear.csv")),
    row.names = FALSE
  )
  
  bds_modified <- bds_cleaned |>
    # Set any length with an age to NA to avoid double use of data
    # in the model via marginals.
    dplyr::mutate(
      lengthcm = dplyr::case_when(
        !is.na(Age) ~ NA, is.na(Age) ~ lengthcm
      )
    )
  
  expanded_comps <- PacFIN.Utilities::get_pacfin_expansions(
    Pdata = bds_modified,
    Catch = catch_formatted,
    weight_length_estimates = weight_length_estimates,
    Units = "MT",
    maxExp = expansion,
    Exp_WA = TRUE,
    verbose = TRUE,
    savedir = save_dir
  )
  
  length_comps_long <- PacFIN.Utilities::getComps(
    Pdata = dplyr::filter(expanded_comps, !is.na(lengthcm)),
    Comps = "LEN",
    weightid = "Final_Sample_Size_L"
  )
  
  length_composition_data <- PacFIN.Utilities::writeComps(
    inComps = length_comps_long,
    fname = fs::path(
      save_dir,
      glue::glue("{species_code}_lcomps_{min(length_bins)}-{max(length_bins)}.csv")
    ),
    comp_bins = length_bins,
    verbose = TRUE
  ) 
  
  age_comps_long <- PacFIN.Utilities::getComps(
    Pdata =  dplyr::filter(expanded_comps, !is.na(Age)),
    Comps = "AGE",
    weightid = "Final_Sample_Size_A"
  )
  
  age_composition_data <- PacFIN.Utilities::writeComps(
    inComps = age_comps_long,
    fname = fs::path(
      save_dir,
      glue::glue("{species_code}_acomps_{min(age_bins)}-{max(age_bins)}.csv")
    ),
    comp_bins = age_bins,
    verbose = TRUE
  )

}



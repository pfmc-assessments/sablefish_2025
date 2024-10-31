year_analysis <- 2023

# Create data_commercial_discard_composition
# Read in discard length data and combine with sample size information
# For a sensitivity change year_analysis to 2021 to:
# * numbers rather than weighted proportions
# * N_unique_Trips rather than a calculated input sample size
data_commercial_discard_length <- dplyr::full_join(
  # Length frequencies
  utils::read.csv(
    fs::path("data-raw", "wcgop", "sablefish_lfs.csv")
  ),
  # Sample size
  utils::read.csv(fs::path("data-raw", "wcgop", "sablefish_Nsamp.csv")),
  by = c("Year", "Gear", "State")
) |>
  dplyr::mutate(
    Gear = dplyr::case_when(
      Gear == "NONTRAWL" ~ 1,
      Gear == "TRAWL" ~ 2,
      TRUE ~ NA_integer_
    ),
    # Specify levels so when making a wide data frame all levels are included
    # even if there are zero observations
    Lenbin = factor(Lenbin, levels = len_bins)
  ) |>
  dplyr::mutate(
    Nsamp = N_unique_Trips,
    prop = if (year_analysis == 2021) {
      Prop.numbers
    } else {
      Prop.wghtd
    }
  ) |>
  dplyr::select(Yr = Year, FltSvy = Gear, State, Lenbin, Nsamp, prop)
stopifnot(!any(is.na(data_commercial_discard_length[["FltSvy"]])))
stopifnot(length(unique(data_commercial_discard_length[["State"]])) == 1)
# Go from long data to wide as it is formatted in Stock Synthesis
data_commercial_discard_composition <- dplyr::full_join(
  x = dplyr::select(data_commercial_discard_length, -State),
  # Copy the data for females and males because information is repeated in
  # Stock Synthesis with Gender = 0
  y = tidyr::expand(
    data_commercial_discard_length,
    Yr, Seas = 1, FltSvy, Gender = 0, Lenbin, Part = 1, Sex = c("F", "M")
  ),
  by = c("Yr", "FltSvy", "Lenbin")
) |>
  tidyr::pivot_wider(
    values_from = prop,
    names_from = c(Sex, Lenbin),
    names_sep = "",
    names_sort = TRUE
  ) |>
  dplyr::relocate(Seas, .after = Yr) |>
  dplyr::relocate(Nsamp, .after = Part) |>
  dplyr::arrange(FltSvy, Part, Yr, Seas) |>
  dplyr::filter(!is.na(Nsamp))

# Create data_commercial_discard_weight
data_commercial_discard_weight <- utils::read.csv(
  fs::path("data-raw", "wcgop", "sablefish_AvgWt.csv"),
  fileEncoding = "UTF-8-BOM"
) |>
  dplyr::mutate(
    Seas = 1, # 2021 season was 7
    Value = Wghtd.AVG_W * 0.453592, # convert lb to kg
    Std_in = Wghtd.AVG_W.SD / Wghtd.AVG_W, # Std_in needs to be a CV
    # Std_in = Wghtd.AVG_W.SD * 0.453592, # Maybe what was used in 2019
    Fleet = dplyr::case_when(
      Gear == "NONTRAWL" ~ 1,
      Gear == "TRAWL" ~ 2,
      TRUE ~ NA_integer_
    ),
    Partition = 1, # discarded
    Type = 2 # Mean weight, rather than 1 is length
  ) |>
  dplyr::select(Year, Seas, Fleet, Partition, Type, Value, Std_in) |>
  dplyr::arrange(Fleet, Partition, Year, Seas)

# Rate data
data_commercial_discard_rates <- merge(
  utils::read.csv(
    fs::path(
      "data-raw",
      "wcgop",
      "sablefish_cs_wcgop_discard_all_years_coastwide_2023-07-19.csv"
    )
  ) |>
    dplyr::select(
      year,
      "CS_LBS" = ob_retained_mt,
      "CS_RATIO" = ob_ratio,
      gear
    ),
  utils::read.csv(
    fs::path(
      "data-raw",
      "wcgop",
      "sablefish_ncs_wcgop_discard_all_years_coastwide_2023-07-19.csv"
    )
  ) |>
    dplyr::select(
      year,
      "NCS_LBS" = ob_retained_mt, # Median.Boot_RETAINED.MTS no longer exported
      "NCS_RATIO" = ob_ratio,
      NCS_SD = sd_boot_ratio,
      gear
    ),
  by = c("gear", "year")
) |>
  dplyr::mutate(
    gear = ifelse(grepl("t", gear, ignore.case = TRUE), 2, 1),
    tot = CS_LBS + NCS_LBS
  ) |>
  dplyr::filter(year > 2019) |>
  dplyr::group_by(year, gear) |>
  dplyr::summarise(
    cs_prop = CS_LBS / tot,
    ncs_prop = NCS_LBS / tot,
    cs_propxrate  = cs_prop * CS_RATIO,
    ncs_propxrate  = ncs_prop * NCS_RATIO,
    Discard = cs_propxrate + ncs_propxrate,
    Std_in = NCS_SD,
    Seas = 1
  ) |>
  dplyr::ungroup() |>
  dplyr::select(year, Seas, gear, Discard, Std_in)

# Write the three objects to data-processed
write_named_csvs(
  data_commercial_discard_composition,
  data_commercial_discard_weight,
  data_commercial_discard_rates,
  dir = "data-processed"
)

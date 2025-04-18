library(dplyr)
library(ggplot2)
library(pacfintools)

raw_pacfin_bds <-
  fs::dir_ls(here::here("data-raw", "bds"), regex = "PacFIN\\..+bds") |>
  purrr::map_df(
    .f = function(x) {load(x); return(bds.pacfin)}
  ) |>
  tibble::tibble()

process_bds_data(
    bds_data = raw_pacfin_bds,
    catch_file = "data_commercial_catch_expansions.csv",
    age_bins = age_bins,
    length_bins = len_bins,
    gears = gears,
    common_name = common_name)

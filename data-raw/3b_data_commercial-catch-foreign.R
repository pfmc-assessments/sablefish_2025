# Run after the 3_data_commercial-catch script that creates all_mortality_data
# The all mortality data file has a column for landings and catch where data that
# do not account for discarding (and discard mortality) are in the landings column
# and the catch column includes removal estimates that account for discards and 
# landings.  The foreign fleet and the at-sea fleet are in the catch column. 
# Assumptions:
# 1) There are no composition data for the foreign fleet and it makes the most sense 
# to mirror the selectivity for each specific gear type noting that this is not a 
# perfect assumption since the foreign fleet data includes both discarded and 
# retained fish.
# 2)  The at-sea catches include both retained and discard mortality estimates 
# and based upon the limited composition data the fleet generally has peak 
# selectivity ~ 40 cm (fish age 2-3 likely) which aligns reasonably well with the
# retained fish from the trawl fleet. Plus, FOS does not apply a discard 
# mortality rate to midwater gear, only bottom trawl.

landings <- all_mortality_data |>
  dplyr::select(-catch_mt) |>
  dplyr::filter(landings_mt > 0) |>
  dplyr::group_by(year, gear_group) |>
  dplyr::summarise(
    seas = 1,
    catch = round(sum(landings_mt), 2), 
    catch_se = 0.01
  ) |>
  dplyr::mutate(
    fleet = recode_fleet_cw(gear_group)
  ) |> 
  dplyr::select(-gear_group) |>
  dplyr::relocate(fleet, .after = seas) |>
  dplyr::arrange(fleet, year)

catch <- all_mortality_data |>
  dplyr::select(-landings_mt) |>
  dplyr::filter(catch_mt > 0) |>
  dplyr::group_by(year, gear_group) |>
  dplyr::summarise(
    seas = 1,
    catch = round(sum(catch_mt), 2), 
    catch_se = 0.01
  ) |>
  dplyr::mutate(
    fleet = recode_fleet_cw(gear_group) + 3
  ) |> 
  dplyr::select(-gear_group) |>
  dplyr::relocate(fleet, .after = seas) |>
  dplyr::arrange(fleet, year)

data_commercial_landings_catch <- dplyr::bind_rows(
  landings,
  catch
)

write_named_csvs(
  data_commercial_landings_catch,
  dir = "data-processed"
)

usethis::use_data(
  data_commercial_landings_catch,
  overwrite = TRUE
)



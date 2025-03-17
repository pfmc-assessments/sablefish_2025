#' Check model estimated total catch to the GEMM estimates
#'
#' 
#' @param  dir Directory to save figure
#' @param  r4ss_output List of model objects created by r4ss::SS_output()
#' @param  add_name String that can be added to the saved figure names.
#' @param  common_name Common name of the species in the GEMM database
#' 
#' @export
#' 
compare_total_catch <- function(
  dir = NULL,
  r4ss_output,
  add_name = NULL,
  common_name = "sablefish") {
  
  catch <- r4ss_output[["catch"]] |>
    dplyr::select(Fleet_Name, Yr, sel_bio, dead_bio, ret_bio) |>
    dplyr::rename(year = Yr) |>
    dplyr::mutate(
      discard = sel_bio - ret_bio,
      dead_discard = dead_bio - ret_bio,
      discard_rate = (sel_bio - ret_bio) / sel_bio
    ) |>
    dplyr::rename(
      landings = ret_bio,
      catch = dead_bio
    ) |>
    dplyr::select(-sel_bio)
  
  if (!is.null(dir)) {
    write.csv(
      catch,
      file = file.path(dir, paste0("model_estimated_catch_", save_name, ".csv")),
      row.names = FALSE
    )
  }
  
  gemm <- nwfscSurvey::pull_gemm(common_name = common_name) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      gemm_dead_discard = sum(total_discard_with_mort_rates_applied_mt),
      gemm_total_catch = sum(total_discard_with_mort_rates_applied_and_landings_mt)
    )
  
  catch_gemm <- catch |>
    dplyr::filter(year %in% unique(gemm$year))
  
  gg1 <- ggplot() +
    geom_bar(stat = "identity", data = catch_gemm, aes(x = year, y = catch, fill = Fleet_Name), alpha = 0.75) +
    geom_line(data = gemm, aes(x = year, y = gemm_total_catch), color = "black", linewidth = 2) +
    scale_fill_viridis_d() +
    ylab("Total Catch (mt)") +
    xlab("Year") +
    theme_bw()
  
  if (!is.null(dir)){
    ggsave(
      gg1,
      filename = file.path(dir, paste0("estimated_catch_comparison_", save_name, ".png"))
    )
  } else {
    gg1
  }
  
  gg2 <- ggplot() +
    geom_bar(stat = "identity", data = catch, 
             aes(x = year, y = dead_discard, fill = Fleet_Name), alpha = 0.75) +
    scale_fill_viridis_d() +
    ylab("Total Dead Discard (mt)") +
    xlab("Year") +
    theme_bw()
  
  if (!is.null(dir)){
    ggsave(
      gg2,
      filename = file.path(dir, "estimated_discard_mortality_all_years.png")
    )
  } else {
    gg2
  }
  
  out <- catch_gemm |>
    dplyr::group_by(year) |>
    dplyr::summarize(
      catch = sum(catch)
    ) |>
    dplyr::left_join(
      gemm
    ) |>
    dplyr::mutate(
      model_minus_gemm = round(catch - gemm_total_catch, 3)
    )
  return(out)
}
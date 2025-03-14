#' Check model estimated total catch to the GEMM estimates
#'
#' 
#' @param  dir Directory to save figure
#' @param  r4ss_output List of model objects created by r4ss::SS_output()
#' @param  years Vector of years to filter where years should be continous (2002:2023)
#' @param  common_name Common name of the species in the GEMM database
#' 
#' @export
#' 
compare_total_catch <- function(
  dir = NULL,
  r4ss_output,
  years = NULL,
  common_name = "sablefish") {
  
  catch <- r4ss_output[["catch"]] |>
    dplyr::select(Fleet_Name, Yr, sel_bio, dead_bio, ret_bio) |>
    dplyr::rename(year = Yr) |>
    dplyr::mutate(
      discard_bio = dead_bio - ret_bio
    )
  catch_all_years <- catch
  
  if (!is.null(years)) {
    catch <- catch |>
      dplyr::filter(year %in% years)
  } 
  
  gemm <- nwfscSurvey::pull_gemm(common_name = common_name) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      gemm_dead_bio = sum(total_discard_with_mort_rates_applied_mt),
      gemm_total_catch = sum(total_discard_with_mort_rates_applied_and_landings_mt)
    )
  
  gg1 <- ggplot() +
    geom_bar(stat = "identity", data = catch, aes(x = year, y = dead_bio, fill = Fleet_Name), alpha = 0.75) +
    geom_line(data = gemm, aes(x = year, y = gemm_total_catch), color = "black", linewidth = 2) +
    scale_fill_viridis_d() +
    ylab("Total Catch (mt)") +
    xlab("Year") +
    theme_bw()
  
  if (!is.null(dir)){
    ggsave(
      gg1,
      filename = file.path(dir, "estimated_catch_comparison.png")
    )
  } else {
    gg1
  }
  
  gg2 <- ggplot() +
    geom_bar(stat = "identity", data = catch_all_years, 
             aes(x = year, y = discard_bio, fill = Fleet_Name), alpha = 0.75) +
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
  
  out <- catch |>
    dplyr::group_by(year) |>
    dplyr::summarize(
      total_catch = sum(dead_bio)
    ) |>
    dplyr::left_join(
      gemm
    ) |>
    dplyr::mutate(
      model_minus_gemm = round(total_catch - gemm_total_catch, 3)
    )
  return(out)
}
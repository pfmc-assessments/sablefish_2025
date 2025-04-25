#' Plot final year age-based selectivity, retention, and discard
#' mortality for a selected year.
#'
#' @param replist List created by `r4ss::SS_output()`
#' @param fleets Vector of fleets to plot.
#' @param year Single numeric year to plot the selectivity for.
#' 
#' @export
#' 
plot_year_selex <- function(
  replist,
  fleets = 1:3,
  year = 2024) {
  selex <- replist[["ageselex"]]
  sex <- unique(selex[, "Sex"])
  n <- 8:ncol(selex)
  keys <- c("Asel", "Aret", "Amort")
  
  final_selex <- selex |>
    dplyr::filter(Yr == year, Factor %in% keys, Fleet %in% fleets) |>
    tidyr::pivot_longer(
      cols = n,
      values_to = "selex",
      names_to = "age"
    ) |>
    dplyr::mutate(
      age = as.numeric(age),
      Sex = as.factor(Sex)
    )
  if (dim(final_selex)[1] == 0) {
    cli::cli_abort("No parameters were found for year {year} and fleets {fleets}.")
  }
  
  gg <- ggplot2::ggplot(final_selex, ggplot2::aes(x = age, y = selex, color = Factor, linetype = Sex, shape = Sex)) +
    ggplot2::geom_line() + 
    ggplot2::geom_point() +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_viridis_d() +
    ggplot2::ylab("Selectivity-Retention-Discard Mortality") +
    ggplot2::facet_grid(c("Fleet"))
  gg
}




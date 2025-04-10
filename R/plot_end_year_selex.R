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
    cli::cli_abort("No parameters were found for yaer {year} and fleets {fleet}.")
  }
  
  gg <- ggplot(final_selex, aes(x = age, y = selex, color = Factor, linetype = Sex, shape = Sex)) +
    geom_line() + 
    geom_point() +
    ylim(c(0, 1)) +
    theme_bw() +
    scale_color_viridis_d() +
    ylab("Selectivity-Retention-Discard Mortality") +
    facet_grid(c("Fleet"))
  gg
}




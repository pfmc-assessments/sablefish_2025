#' Plot weight-at-age data
#'
#' @param data A data frame of weight-at-age values.
#' @param max_age The maximum age in the modelled data.
#' @return A {ggplot2} object.
#' @author Chantel Wetzel and Kelli F. Johnson
#' @export
plot_weight_at_age <- function(data, max_age) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = year,
      y = weight_kg,
      colour = factor(age_years),
      group = age_years
    )
  ) +
    ggplot2::stat_summary(fun = mean, geom = "line") +
    ggplot2::stat_summary(fun = mean, geom = "point") +
    ggplot2::ylab("Mean weight-at-age (kg)") +
    ggplot2::labs(col = "Age\n(years)") +
    ggplot2::scale_color_manual(values = r4ss::rich.colors.short(max_age + 5)) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"))
}
#' Plot weight-at-age data
#'
#' @param data A data frame of weight-at-age values.
#' @return A {ggplot2} object.
#' @author Chantel Wetzel and Kelli F. Johnson
#' @export
plot_weight_age <- function(data) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = age_years,
      y = weight_kg,
      colour = factor(state),
      group = state
    )
  ) +
    ggplot2::geom_point() + 
    ggplot2::ylab("Weight (kg)") +
    ggplot2::labs(col = "Age\n(years)") +
    ggplot2::scale_colour_viridis_d() + 
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white")) 
}
#' Plot weight-at-age data ouliers
#'
#' @param data A data frame of weight-at-age values.
#' @return A {ggplot2} object.
#' @author Chantel Wetzel and Kelli F. Johnson
#' @export
plot_weight_at_age_outlier <- function(data, ...) {
  data_plot <- if (!"outlier" %in% colnames(data)) {
    weight_at_age_outlier(data, filter = FALSE, drop = FALSE)
  } else {
    {
      data
    } |>
      dplyr::filter(!is.na(weight_kg))
  }
  data_lines <- data.frame(
    length_cm = 1:max(data_plot[["length_cm"]], na.rm = TRUE)
  ) %>%
    dplyr::mutate(
      lower = length_cm^3 * 2e-6, # need to modify for sablefish
      upper = length_cm^3 * 20e-6 # need to modify for sablefish
    )
  
  gg_length <- ggplot2::ggplot(
    data = data_plot,
    ggplot2::aes(
      x = length_cm,
      y = weight_kg
    )
  ) +
    ggplot2::geom_point(pch = 16, ggplot2::aes(color = outlier)) +
    ggplot2::scale_colour_manual(values = c(grDevices::rgb(0, 0, 0, 0.2), 2)) +
    ggplot2::geom_line(data = data_lines, colour = 4, ggplot2::aes(y = lower)) +
    ggplot2::geom_line(data = data_lines, colour = 4, ggplot2::aes(y = upper)) +
    ggplot2::ylim(c(0, max(data_plot[["weight_kg"]], na.rm = TRUE) * 1.01)) +
    ggplot2::xlab("Length (cm)") +
    ggplot2::ylab("Weight (kg)") +
    ggplot2::theme(
      legend.title = ggplot2::element_text("Outlier"),
      legend.position = "none"
    ) +
    wrap_by(...)
  
  gg_age <- ggplot2::ggplot(
    data = data_plot,
    ggplot2::aes(
      x = age_years,
      y = weight_kg
    )
  ) +
    ggplot2::geom_point(pch = 16, ggplot2::aes(colour = outlier)) +
    ggplot2::scale_colour_manual(values = c(grDevices::rgb(0, 0, 0, 0.2), 2)) +
    ggplot2::xlab("Age (years)") +
    ggplot2::ylab("Weight (kg)") +
    ggplot2::theme(
      legend.title = ggplot2::element_text("Outlier"),
      legend.position = "none"
    ) +
    wrap_by(...)
  
  gg_age_log <- gg_age +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::theme(legend.position = "top", legend.box = "horizontal") +
    wrap_by(...)
  
  final <- cowplot::plot_grid(
    gg_length,
    gg_age,
    gg_age_log,
    align = "h",
    nrow = 1
  )
  return(final)
}
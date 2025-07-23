#' Plot historical spawning outptu
#'
#' 
#' @param  historical_ssb Dataframe created by data_historical-SSB.R
#'
plot_historical_ssb <- function(historical_ssb){

  colors <- c("#0072B2", "#D55E00", "#CC79A7", "#009E73","#E69F00", "#F0E442", "#56B4E9", "#000000")
  ggplot2::ggplot(historical_ssb |> dplyr::filter(Year >= 1890) |> dplyr::arrange(Assessment_Year) |> dplyr::mutate(Assessment_Year = as.factor(Assessment_Year)),
        ggplot2::aes(x=Year, y=Value, color = Assessment_Year)) +
    ggplot2::geom_line(linewidth = 1.5)  +
    ggplot2::labs(
        title = "",
        x = "Year",
        y = "Spawning Output (mt)",
        color = "Assessment Year"
    ) +
    ggplot2::scale_color_manual(values = colors) + 
    ggplot2::scale_x_continuous(limits = c(1890, 2025), breaks=seq(1890, 2025, 20)) +
    ggplot2::scale_y_continuous(limits = c(0, 260000), breaks=seq(0, 3e5, 5e4), labels=scales::comma) +
    #ggplot2::guides(
    #    color = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
    #) +
    ggplot2::theme_bw(base_size = 12) + 
    ggplot2::theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        panel.grid.major = ggplot2::element_line(color = "grey90"),
        panel.grid.minor = ggplot2::element_blank()
    )
}

#' Plot historical stock status
#'
#' 
#' @param  historical_ssb Dataframe created by data_historical-SSB.R
#'
plot_historical_depl <- function(historical_ssb){
  
  data <- historical_ssb |> 
    dplyr::group_by(Assessment_Year) |>
    dplyr::mutate(
      depl = Value / Value[1],
      Assessment_Year = as.factor(Assessment_Year)
    ) |>
    dplyr::arrange(Assessment_Year) |> 
    dplyr::filter(Year >= 1890) 
  colors <- c("#0072B2", "#D55E00", "#CC79A7", "#009E73","#E69F00", "#F0E442", "#56B4E9", "#000000")
  ggplot2::ggplot(data, ggplot2::aes(x=Year, y=depl, color = Assessment_Year)) +
    ggplot2::geom_line(linewidth = 1.5)  +
    ggplot2::labs(
      title = "",
      x = "Year",
      y = "Stock Status",
      color = "Assessment Year"
    ) +
    ggplot2::geom_hline(yintercept = 0.40, linetype = "dashed", color = "black") + 
    ggplot2::geom_hline(yintercept = 0.25, linetype = "dashed", color = "red") + 
    ggplot2::scale_color_manual(values = colors) + 
    ggplot2::scale_x_continuous(limits = c(1890, 2025), breaks=seq(1890, 2025, 20)) +
    ggplot2::ylim(c(0, 1.30)) +
    #ggplot2::guides(
    #  color = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
    #) +
    ggplot2::theme_bw(base_size = 12) + 
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      panel.grid.minor = ggplot2::element_blank()
    )
}
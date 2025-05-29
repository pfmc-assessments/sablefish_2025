plot_historical_ssb <- function(){
    ggplot2::ggplot(historical_ssb)+
        ggplot2::geom_line(ggplot2::aes(x=Year, y=Value, color=Assessment_Year)) +
        ggplot2::geom_point(ggplot2::aes(x=Year, y=Value, color=Assessment_Year)) +
        # ggplot2::facet_wrap(~Quantity, scales = "free_y") +
        ggplot2::labs(
            title = "",
            x = "Model Year",
            y = "Spawning Biomass (Metric tons)",
            color = "Assessment Year"
        ) +
        ggplot2::scale_x_continuous(limits = c(1890, 2025), breaks=seq(1890, 2025, 20)) +
        ggplot2::scale_y_continuous(limits = c(0, 300000), breaks=seq(0, 3e5, 5e4), labels=scales::comma)+
        ggplot2::guides(
            color = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
        ) +
        ggplot2::theme_bw(base_size = 16) + 
        ggplot2::theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            panel.grid.major = ggplot2::element_line(color = "grey90"),
            panel.grid.minor = ggplot2::element_blank()
        )
}

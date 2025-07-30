
library(ggplot2)

projections <- readxl::read_excel(
  path = here::here("model", "_star_panel_requests", "request_7", "new_ss3_projections.xlsx"),
  sheet = "Sheet1")
projections$Option <- paste(projections$Model, projections$Period)

ggplot(projections |> dplyr::filter(Year >= 2027), aes(x = Year, y = Value, color = Option, linetype = Option)) +
  geom_line(linewidth = 1.25) +
  theme_bw() + 
  ylab("Metric (mt)") + 
  facet_wrap(facets = "Metric", ncol = 1)

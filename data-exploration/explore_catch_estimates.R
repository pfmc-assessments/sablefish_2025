
model_2023 <- r4ss::SS_output(here::here("model", "_bridging", "0_2023_model"))
discard_fleets <- r4ss::SS_output(here::here("model", "0_init"))

mortality <- dplyr::bind_rows(
  model_2023[["catch"]][, c("Fleet_Name", "Yr", "kill_bio")] |>
    dplyr::rename(dead_bio = kill_bio) |>
    dplyr::mutate(Assessment = as.factor(2023)),
  discard_fleets[["catch"]][, c("Fleet_Name", "Yr", "dead_bio")] |>
    dplyr::mutate(Assessment = as.factor(2025))
) |>
  dplyr::rename(
    Year = Yr
  ) |>
  dplyr::filter(Year < 2023)

total_mortality <- mortality |>
  dplyr::group_by(Year, Assessment) |>
  dplyr::summarise(
    catch = sum(dead_bio)
  ) 

diff_catch <- total_mortality |>
  tidyr::pivot_wider(
    names_from = Assessment,
    values_from = catch
  ) |>
  dplyr::mutate(diff = `2025` - `2023`)

library(ggplot2)
ggplot(mortality, aes(x = Year, y = dead_bio, fill = Fleet_Name)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Catch (mt)") +
  facet_grid("Assessment")

ggplot(total_mortality, aes(x = Year, y = catch, linetype = Assessment, color = Assessment)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  xlab("Year") + ylab("Catch (mt)") +
  scale_color_viridis_d(begin = 0, end = 0.5)

ymax <- max(diff_catch[, "diff"]) + 200
ymin <- -1 * ymax
ggplot(diff_catch, aes(x = Year, y = diff)) +
  geom_line(linewidth = 1) +
  ylim(c(ymin, ymax)) + 
  theme_bw() +
  xlab("Year") + ylab("2025 Catch (mt) - 2023 Catch (mt)") 

library(dplyr)
library(ggplot2)
library(PacFIN.Utilities)

raw_pacfin_bds <-
  fs::dir_ls(here::here("data-raw", "bds"), regex = "PacFIN\\..+bds") |>
  purrr::map_df(
    .f = function(x) {load(x); return(bds.pacfin)}
  ) |>
  tibble::tibble()

# Prior to running PacFIN.Utilities explore the ageing methods available in the 
# data and the number of samples without an ageing method

bds <- cleanPacFIN(
  Pdata = raw_pacfin_bds,
  CLEAN = TRUE
)

# Investigate the number of samples removed by cleanPacFIN to ensure that we want
# them removed

# Explore the length and age samples by port and year

# Explore the number of length and age samples available by gear and the grouped
# gears (traw, pot, hkl)


bds |>
  filter(year %in% c(2021, 2022), geargroup %in% c("TWL", "HKL", "POT")) |>
  group_by(state, geargroup) |>
  summarize(
    n = n()
  ) |>
  mutate(
    percent = round(n / sum(n), 2)
  )

bds |>
  filter(year %in% c(2021, 2022), geargroup %in% c("TWL", "HKL", "POT")) |>
  group_by(geargroup) |>
  summarize(
    n = n()
  ) |>
  mutate(
    percent = round(n / sum(n), 2)
  )

ggplot(bds |> filter(year > 2010),
    aes(y = lengthcm, x = year, group = year)) +
  geom_boxplot() +
  facet_wrap(facets = c("geargroup", "state"))
ggsave(filename = here::here("data", "pacfin", "length_by_gear_state.png"),
       width = 20, height = 20)

ggplot(bds |> filter(year > 2010),
       aes(y = lengthcm, x = year, group = year)) +
  geom_boxplot() +
  facet_wrap(facets = c("geargroup"))
ggsave(filename = here::here("data", "pacfin", "length_by_gear.png"),
       width = 20, height = 10)

bds[, "period"] <- "2011-2019"
bds[which(bds$year < 2011), "period"] <- "1980-2010"
bds[which(bds$year > 2019), "period"] <- "2020-2024"
ggplot(bds |> filter(geargroup != "NET"),
       aes(x = lengthcm, fill = geargroup, alpha = 0.25)) +
  geom_density() +
  facet_wrap(facets = c( "period", "state")) +
  scale_fill_viridis_d()
ggsave(filename = here::here("data", "pacfin", "length_by_gear_by_period_density.png"),
       width = 10, height = 12)

samples <- bds |>
  filter(year > 2010, !is.na(lengthcm)) |>
  group_by(year, state, geargroup) |>
  summarize(
    n = n()
  )

ggplot(bds |> filter(geargroup != "NET"),
       aes(x = lengthcm, fill = geargroup, alpha = 0.25)) +
  geom_density() +
  facet_grid(period~.) +
  scale_fill_viridis_d()
ggsave(filename = here::here("data", "pacfin", "length_by_gear_by_period_density_simple.png"),
       width = 10, height = 12)

bds$count <- 1
ggplot(bds |> filter(year > 2020, geargroup %in% c("HKL", "POT", "TWL")),
       aes(x = year, y = count, fill = geargroup)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = c("state")) +
  scale_fill_viridis_d()

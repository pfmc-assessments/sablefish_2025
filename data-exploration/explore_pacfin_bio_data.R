library(tidyverse)
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
raw_pacfin_bds|> 
  select(AGENCY_CODE, AGE_METHOD1, AGE_METHOD2, AGE_METHOD3, AGE_METHOD4) |>
  tidyr::pivot_longer(starts_with("AGE_METHOD"), names_to="Method Type", values_to="Method") |>
  mutate(
    Method = PacFIN.Utilities::codify_age_method(Method)
  ) |>
  count(AGENCY_CODE, `Method Type`, Method) |>
  tidyr::pivot_wider(names_from="Method", values_from=n)

raw_pacfin_bds |> 
  filter(if_all(AGE_METHOD1:AGE_METHOD4, is.na)) %>%
  count(SAMPLE_YEAR, age1) %>%
  group_by(SAMPLE_YEAR) %>% 
  mutate(
    total_n=sum(n),
    prop = n/total_n
  ) %>%
  filter(is.na(age1)) %>%
  print(n=100)


raw_pacfin_bds|>
  select(SAMPLE_YEAR, method=AGE_METHOD1, a=age4) %>%

  ggplot(aes(x=a, fill=method)) +
    geom_histogram() +
    facet_wrap(~SAMPLE_YEAR, scales="free_y")


raw_pacfin_bds|> 
  select(AGENCY_CODE, SEX_CODE, FISH_LENGTH, FISH_WEIGHT, AGE_METHOD1, AGE_METHOD2, AGE_METHOD3, AGE_METHOD4, age1, age2, age3, age4) |>
  tidyr::pivot_longer(starts_with("AGE_METHOD"), names_to="AGE_METHOD_NUM", values_to="AGE_METHOD") |>
  tidyr::pivot_longer(c(age1, age2, age3, age4), names_to="AGE_NUM", values_to="AGE") |>
  mutate(
    AGE_METHOD = PacFIN.Utilities::codify_age_method(AGE_METHOD)
  ) |>
  # count(AGENCY_CODE, AGE_METHOD, FISH_WEIGHT)
  # ) %>%

  ggplot(aes(x=AGE, y=FISH_LENGTH, color=SEX_CODE))+
    geom_point()+
    facet_grid(rows=vars(AGENCY_CODE), cols=vars(AGE_METHOD), scales="free_y")

  

raw_pacfin_bds|> 
  select(SAMPLE_YEAR, AGENCY_CODE, SEX_CODE, FISH_LENGTH, FISH_WEIGHT, AGE_METHOD1, AGE_METHOD2, AGE_METHOD3, AGE_METHOD4, age1, age2, age3, age4) |>
  tidyr::pivot_longer(starts_with("AGE_METHOD"), names_to="AGE_METHOD_NUM", values_to="AGE_METHOD") |>
  tidyr::pivot_longer(c(age1, age2, age3, age4), names_to="AGE_NUM", values_to="AGE") |>
  mutate(
    AGE_METHOD = PacFIN.Utilities::codify_age_method(AGE_METHOD)
  ) |>
  # count(AGENCY_CODE, AGE_METHOD, FISH_WEIGHT)
  # ) %>%

  ggplot(aes(x=AGE, y=FISH_LENGTH, color=SEX_CODE, shape=AGE_METHOD))+
    geom_point()+
    scale_shape_manual(values=c(15, 16, 17, 18))+
    facet_wrap(~SAMPLE_YEAR, scales="free_y")


# The maximum quantile at which to cap all data expansions within getExpansion_1().
expansion <- 0.95

# Determine what samples to retain or filter out when using cleanPacFIN()
# Keep all alternate (A), fork (F), and unknown (U) length types based on 
# FISH_LENGTH_TYPE_CODE. This decision should be species-specific
good_lengths <- c("U", "A", "F")
# Keep only random (R) samples based on SAMPLE_METHOD_CODE. Only random samples
# should be retained unless specified by a state agency.
good_methods <- "R"
# Keep commercial on-board (C), market (M), and blank samples based on SAMPLE_TYPE
good_samples <- c("", "M", "C")
# Keep data from all 3 states. This is the default.
good_states <- c("WA", "OR", "CA")
# Keep only break and burn (B, BB), unknown (U), and blank (") age reads based 
# on AGE_METHOD. This decision should be species-specific.
good_age_method <- c("B", "BB", "U", "")
#
bds <- cleanPacFIN(
  Pdata = raw_pacfin_bds,
  keep_gears = c("HKL", "POT", "TWL"),
  CLEAN = TRUE,
  keep_age_method = good_age_method,
  keep_sample_type = good_samples,
  keep_sample_method = good_methods,
  keep_length_type = good_lengths,
  keep_states = good_states,
  spp = "sablefish",
  savedir = here::here("data-raw", "bds")
) |>
  dplyr::mutate(
    stratification = paste(state, geargroup, sep = ".")
  )

# Investigate the number of samples removed by cleanPacFIN to ensure that we want
# them removed
head(bds)
# Explore the length and age samples by port and year


# Explore the number of length and age samples available by gear and the grouped
# gears (traw, pot, hkl)
bds %>%
  group_by(state, geargroup) %>%
  select(SEX, Age, lengthcm) %>%
  mutate(state=factor(state)) %>%

  ggplot(aes(x=lengthcm, fill=geargroup))+
    geom_histogram(position="identity", alpha=0.5)+
    facet_wrap(~state)

bds %>%
  group_by(state, geargroup) %>%
  select(SEX, Age, lengthcm) %>%
  mutate(state=factor(state)) %>%

  ggplot(aes(x=Age, fill=geargroup))+
    geom_histogram(position="identity", alpha=0.5)+
    facet_wrap(~state)

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


ggplot(bds,
    aes(y = lengthcm, x = year, fill=geargroup, color=geargroup, group = interaction(year, geargroup))) +
  geom_boxplot() +
  facet_wrap(facets = c("state"), nrow=3)


bds[, "period"] <- "2011-2019"
bds[which(bds$year < 2011), "period"] <- "1980-2010"
bds[which(bds$year > 2019), "period"] <- "2020-2024"

bds %>% group_by(state, period, geargroup) %>%
  summarise(
    mu = mean(lengthcm,na.rm=TRUE)
  ) %>%
  arrange(geargroup, state, period) %>%
  print(n=100)

ggplot(bds |> filter(geargroup != "NET"),
       aes(x = lengthcm, fill = geargroup, alpha = 0.25)) +
  geom_density() +
  facet_wrap(facets = c( "period", "state")) +
  scale_fill_viridis_d()
ggsave(filename = here::here("data", "pacfin", "length_by_gear_by_period_density.png"),
       width = 10, height = 12)

ggplot(bds |> filter(geargroup != "NET"),
       aes(x = Age, fill = geargroup, alpha = 0.25)) +
  geom_density() +
  facet_wrap(facets = c( "period", "state")) +
  scale_fill_viridis_d()
ggsave(filename = here::here("data", "pacfin", "age_by_gear_by_period_density.png"),
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

ggplot(bds |> filter(geargroup != "NET"),
       aes(x = Age, fill = geargroup, alpha = 0.25)) +
  geom_density() +
  facet_grid(period~.) +
  scale_fill_viridis_d()

ggsave(filename = here::here("data", "pacfin", "Age_by_gear_by_period_density_simple.png"),
       width = 10, height = 12)


bds$count <- 1
ggplot(bds |> filter(year > 2010, geargroup %in% c("HKL", "POT", "TWL")),
       aes(x = year, y = count, fill = geargroup)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = c("state")) +
  scale_fill_viridis_d()


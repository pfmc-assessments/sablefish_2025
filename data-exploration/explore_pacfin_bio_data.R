library(tidyverse)
library(ggplot2)
library(ggridges)
library(PEPtools)
library(pacfintools)

port_lats <- pacfin_ports_withlatlong |>
  dplyr::rename(
    pacfin_port_code = pcid
  ) |>
  dplyr::select(
    c(-name, -agencydesc, -agid)
  ) |>
  dplyr::distinct(pacfin_port_code, .keep_all = TRUE) |>
  tibble::tibble()


raw_pacfin_bds <-
  fs::dir_ls(here::here("data-raw", "bds"), regex = "PacFIN\\..+bds")[1] |>
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

bds <- readRDS(file.path(here::here(), "data-raw", "bds", "cleaned_pacfin_bds.rds"))

bds <- bds %>% left_join(port_lats, by=c("PCID"="pacfin_port_code"))
port_factor_levels <- bds %>% arrange(latitude) %>% pull(PACFIN_PORT_NAME) %>% unique
bds <- bds %>% mutate(
  port = factor(PACFIN_PORT_NAME, levels=port_factor_levels)
)

bds <- bds %>%
  mutate(
    region = case_when(
      state == "WA" ~ "WA",
      state == "OR" ~ "OR",
      state == "CA" & latitude >= 36.0 ~ "N. CA",
      state == "CA" & latitude < 36.0 ~ "S. CA"
    ),
    region = factor(region, levels = c("S. CA", "N. CA", "OR", "WA"))
  )

# Investigate the number of samples removed by cleanPacFIN to ensure that we want
# them removed
head(bds)
# Explore the length and age samples by port and year

colnames(bds)
bds <- bds[,c("year", "state", "region", "port", "geargroup", "lengthcm", "Age", "SEX")]

data_commercial_bds_summ <- bds
save(data_commercial_bds_summ, file=file.path(here::here(), "data", "data_commercial_bds_summ.rda"))

# Explore the number of length and age samples available by gear and the grouped
# gears (traw, pot, hkl)

custom_theme <- theme_bw()+
    theme(
      axis.text = element_text(size=14),
      axis.title = element_text(size=15),
      legend.text = element_text(size=14),
      legend.title = element_text(size=15),
      legend.position = "bottom",
      plot.margin = margin(10, 20, 10, 10),
      panel.spacing = unit(20, "pt"),
      strip.text = ggplot2::element_text(size=14),
    )

# Ridgeline length composition across geargroup by region
bds %>%
  group_by(region, geargroup) %>%
  select(SEX, Age, lengthcm) %>%
  mutate(region=factor(region)) %>%

  ggplot(aes(x=lengthcm, fill=geargroup))+
    ggridges::geom_density_ridges(aes(y=region), alpha=0.5, scale=1)+
    labs(y="Region", x="Length (cm)", fill="Gear Type")+
    scale_x_continuous(limits=c(0, 125), expand=c(0, 0))+
    custom_theme
ggsave(filename = here::here("data", "pacfin", "length_by_gear_region.png"),
       width = 20, height = 20)

# Ridgeline age composition across geargroup by region
bds %>%
  group_by(region, geargroup) %>%
  select(SEX, Age, lengthcm) %>%
  mutate(region=factor(region)) %>%

  ggplot(aes(x=Age, fill=geargroup))+
    ggridges::geom_density_ridges(aes(y=region), alpha=0.5, scale=1)+
    labs(y="Region", x="Age", fill="Gear Type")+
    scale_x_continuous(limits=c(0, 125), expand=c(0, 0), breaks=seq(0, 125, 25))+
    custom_theme
ggsave(filename = here::here("data", "pacfin", "age_by_gear_region.png"),
       width = 20, height = 20)

# Violin lengthcompos since 2011 across geargroup by region
ggplot(bds |> filter(year > 2010),
    aes(y = lengthcm, x = year, group = interaction(geargroup, year), fill=geargroup)) +
  geom_violin(draw_quantiles = c(0.50))+
  scale_y_continuous(limits=c(0, 125), expand=c(0, 0), breaks=seq(0, 125, 25))+
  labs(y="Length (cm)", x="Year", fill="Gear Type")+
  facet_grid(geargroup ~ region)+
  custom_theme+
  theme(panel.spacing.x = unit(40, "pt"))
ggsave(filename = here::here("data", "pacfin", "length_by_gear_region_time.png"),
       width = 20, height = 20)

# Violin lengthcompos since 2011 by geargroup
ggplot(bds |> filter(year > 2010),
       aes(y = lengthcm, x = year, group = year, fill=geargroup)) +
  geom_violin(draw_quantiles = c(0.50))+
  scale_y_continuous(limits=c(0, 125), expand=c(0, 0), breaks=seq(0, 125, 25))+
  labs(y="Length (cm)", x="Year", fill="Gear Type")+
  facet_wrap(facets = c("geargroup"))+
  custom_theme+
  theme(panel.spacing.x = unit(40, "pt"))
ggsave(filename = here::here("data", "pacfin", "length_by_gear_time.png"),
       width = 20, height = 10)


ggplot(bds |> filter(year > 2010, !is.na(Age)),
       aes(x = Age, fill = geargroup)) +
  geom_density(alpha = 0.4) +
  scale_color_viridis_c() +
  theme_bw() 

ggplot(bds |> filter(year > 2010, !is.na(Age)),
       aes(x = lengthcm, fill = geargroup)) +
  geom_density(alpha = 0.4) +
  scale_color_viridis_c() +
  theme_bw() 

ggplot(bds |> filter(year <= 2010, !is.na(Age)),
       aes(x = lengthcm, fill = geargroup)) +
  geom_density(alpha = 0.4) +
  scale_color_viridis_c() +
  theme_bw() 


bds[, "period"] <- "2011-2018"
bds[which(bds$year < 2010), "period"] <- "1980-2010"
bds[which(bds$year > 2018), "period"] <- "2019-2024"

bds %>% group_by(region, period, geargroup) %>%
  summarise(
    mu = mean(lengthcm,na.rm=TRUE)
  ) %>%
  arrange(geargroup, region, period) %>%
  print(n=100)

# Ridgeline length composition across geargroup by region and time period
ggplot(bds |> filter(geargroup != "NET"),
       aes(x = lengthcm, y=period, fill = geargroup, alpha = 0.25)) +
  ggridges::geom_density_ridges(scale=0.9) +
  scale_x_continuous(limits=c(0, 125), expand=c(0, 0), breaks=seq(0, 125, 25))+
  labs(y="Time Period", x="Length (cm)", fill="Gear Type")+
  guides(alpha="none")+
  facet_wrap(~region, ncol=2) +
  scale_fill_viridis_d()+
  custom_theme+
  theme(panel.spacing.x = unit(40, "pt"))

ggsave(filename = here::here("data", "pacfin", "length_by_gear_by_period_density.png"),
       width = 10, height = 10)


samples <- bds |>
  filter(year > 2010, !is.na(lengthcm)) |>
  group_by(year, region, geargroup) |>
  summarize(
    n = n()
  )

# Ridgeline length composition across geargroup by time period
ggplot(bds |> filter(geargroup != "NET"),
       aes(x = lengthcm, y=period, fill = geargroup, alpha = 0.25)) +
  ggridges::geom_density_ridges(scale=1) +
  labs(y="Time Period", x="Length (cm)", fill="Gear Type")+
  guides(alpha="none")+
  # facet_wrap(~region, ncol=2) +
  scale_fill_viridis_d()+
  custom_theme+
  theme(panel.spacing.x = unit(40, "pt"))
ggsave(filename = here::here("data", "pacfin", "length_by_gear_by_period_density_simple.png"),
       width = 10, height = 12)

# Barchart number samples geargroup by region
bds %>% 
  filter(geargroup %in% c("HKL", "POT", "TWL")) %>%
  count(year, region, geargroup) %>%

ggplot(aes(x = year, y = n, fill = geargroup)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = c("region")) +
  scale_fill_viridis_d()+
  labs(y="Number of Samples", x="Year", fill="Gear Type")+
  custom_theme
ggsave(filename = here::here("data", "pacfin", "length_samples_by_gear_by_time.png"),
       width = 10, height = 10)

# Ridgeline length composition by port
ggplot(bds, aes(x=lengthcm, y=port, fill=geargroup))+
  geom_density_ridges(scale=1, alpha=0.50)+
  scale_x_continuous(limits=c(0, 125))+
  labs(y="Port", x="Length (cm)", fill="Gear Type")+
  custom_theme
ggsave(filename = here::here("data", "pacfin", "length_samples_by_gear_by_port.png"),
       width = 10, height = 25)
 
# Barchart proportion geargroup by port
plot_prop_geargroup_port <- bds %>%
  count(port, geargroup) %>%
  group_by(port) %>%
  mutate(prop=n/sum(n)) %>%

  ggplot(aes(y=port, fill=geargroup)) +
    geom_col(aes(x=prop))+
    coord_cartesian(expand=0)+
    labs(x="Proportion of Samples", y="Port", fill="Gear Type")

# Barchart number samples geargroup by port
plot_n_geargroup_port <- bds %>%
  count(port, geargroup) %>%

  ggplot(aes(y=port, fill=geargroup)) +
    geom_col(aes(x=n))+
    coord_cartesian(expand=0)+
    labs(x="Number of Samples", y="Port", fill="Gear Type")

library(patchwork)

plot_n_geargroup_port + plot_prop_geargroup_port + plot_layout(guides="collect") & custom_theme
ggsave(filename = here::here("data", "pacfin", "samples_by_gear_by_port.png"),
       width = 10, height = 25)

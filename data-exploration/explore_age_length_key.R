library(FSA)
library(FSAdata)
library(plotrix)
# https://derekogle.com/fishR/examples/oldFishRVignettes/AgeLengthKey.pdf


data(RockBassLO2)
head(RockBassLO2)

# 1. Create a data frame with aged fish
# 1. Create a data frame with the unaged fish that has an age column with NAs

rb.age <- dplyr::filter(RockBassLO2, !is.na(age)) # get fish without NAs in age variable
str(rb.age)

rb.len <- dplyr::filter(RockBassLO2,is.na(age)) # get fish with NAs in age variable
str(rb.len)

Summarize(~tl,data = rb.age, digits=1)
# startcat is the beginning of the length category
# w is the width of the length categories
rb.age1 <- lencat(~tl, data = rb.age, startcat=110, w=10)
# the binned lengths are in a column called LCat

# matrix of counts of length bin (column) and age (rows)
rb.raw <- with(rb.age1, table(LCat, age))
rb.key <- prop.table(rb.raw, margin = 1)
round(rb.key, 2) 

rb.len1 <- alkIndivAge(rb.key, age ~ tl, data = rb.len)
head(rb.len1)

rb.comb <- rbind(rb.age,rb.len1)
head(rb.comb)

rb.sum <- Summarize(tl~age,data=rb.comb,digits=2) 


hist(~age,data=rb.comb,breaks=3:11,xlim=c(2,12),xlab="Age (yrs)",main="")

plot(tl~age, data=rb.comb, ylab="Total Length (mm)",xlab="Age (jittered)")
lines(mean~age,data=rb.sum,col="blue",lwd=2)

histStack(tl~age,data=rb.comb,breaks=seq(110,280,10),xlab="Total Length (mm)")

#===============================================================================
# Visualize data
#===============================================================================

age_comps_raw <- read.csv(here::here("data-processed", "data_commercial_discard_age_composition.csv"))
colnames(age_comps_raw)[1] <- "year"

age_comps <- age_comps_raw[, 1:60] |>
  dplyr::select(-month, -partition, -age_error, -age_low, -age_high, -input_n) |>
  tidyr::pivot_longer(
    cols = 4:54,
    names_to = "age",
    values_to = "prop"
  ) |>
  dplyr::mutate(
    sex_chara = as.factor(substring(age, first = 1, last = 1)),
    sex_chara = dplyr::case_when(sex == 0 ~ "u", .default = sex_chara),
    sex_chara = as.factor(sex_chara),
    sex_type = dplyr::case_when(sex_chara == "u" ~ "u", .default = "b"),
    age_number = as.numeric(substring(age, first = 2, last = 3)), 
    prop = round(prop / 100, 3)
  ) |>
  dplyr::select(-sex, -age) |>
  dplyr::rename(
    sex = sex_chara,
    age = age_number) |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    prop_to_add = dplyr::case_when(age >= 15 ~ prop, .default = 0),
    plus_group = dplyr::case_when(age >=15 ~ sum(prop_to_add), .default = 0),
    prop = dplyr::case_when(age == 15 ~ plus_group, .default = prop),
    group = dplyr::case_when(year <= 2013 ~ 1, .default = 2)
  )


gg_trawl <- ggplot(age_comps |> dplyr::filter(age < 16, fleet == 1) |> dplyr::mutate(age = as.factor(age)), aes(x = age, y = prop, fill = age)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  geom_text(aes(label = year),
            x = 10,
            y = 0.4,
            size = 6) +
  scale_y_continuous(limits = c(0, 0.60), breaks = seq(0, 0.50, by = 0.25)) +
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(0, "cm"),
        strip.text.x = element_blank(),
        plot.margin = margin(12, 12, 6, 12),
        text = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        axis.text = element_text(size = 18),
        legend.position = "none") +
  ylab("Proportion") +
  xlab("Age") + 
  facet_wrap("year", dir = "v", ncol = 2)

gg_pot <- ggplot(age_comps |> dplyr::filter(age < 16, fleet == 3) |> dplyr::mutate(age = as.factor(age)), aes(x = age, y = prop, fill = age)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  geom_text(aes(label = year),
            x = 10,
            y = 0.4,
            size = 6) +
  scale_y_continuous(limits = c(0, 0.60), breaks = seq(0, 0.50, by = 0.25)) +
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(0, "cm"),
        strip.text.x = element_blank(),
        plot.margin = margin(12, 12, 6, 12),
        text = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        axis.text = element_text(size = 18),
        legend.position = "none") +
  ylab("Proportion") +
  xlab("Age") + 
  facet_wrap("year", dir = "v", ncol = 2)

gg_hkl <- ggplot(age_comps |> dplyr::filter(age < 16, fleet == 2) |> dplyr::mutate(age = as.factor(age)), aes(x = age, y = prop, fill = age)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  geom_text(aes(label = year),
            x = 10,
            y = 0.4,
            size = 6) +
  scale_y_continuous(limits = c(0, 0.60), breaks = seq(0, 0.50, by = 0.25)) +
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(0, "cm"),
        strip.text.x = element_blank(),
        plot.margin = margin(12, 12, 6, 12),
        text = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        axis.text = element_text(size = 18),
        legend.position = "none") +
  ylab("Proportion") +
  xlab("Age") + 
  facet_wrap("year", dir = "v", ncol = 2)

ggplot2::ggsave(
  gg_trawl,
  filename = here::here(
    "data-raw",
    "discard",
    "wcgop",
    "figures",
    "discard_trawl_ages.png"
  ),
  width = 10, 
  height = 10
)

ggplot2::ggsave(
  gg_hkl,
  filename = here::here(
    "data-raw",
    "discard",
    "wcgop",
    "figures",
    "discard_hkl_ages.png"
  ),
  width = 10, 
  height = 10
)

ggplot2::ggsave(
  gg_pot,
  filename = here::here(
    "data-raw",
    "discard",
    "wcgop",
    "figures",
    "discard_pot_ages.png"
  ),
  width = 10, 
  height = 10
)

library(FSA)
library(FSAdata)
library(plotrix)
# https://derekogle.com/fishR/examples/oldFishRVignettes/AgeLengthKey.pdf

# Load in matrix of fish with lengths and ages
cols_to_keep <- c(
  "Year",
  "Age_years", 
  "Length_cm",
  "Latitude_dd"
)

survey_ages <- dplyr::bind_rows(
  data_survey_bio$nwfsc_combo |> dplyr::select(dplyr::all_of(cols_to_keep)), 
  data_survey_bio$nwfsc_slope |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$afsc_slope$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$triennial_early$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$triennial_late$age_data |> dplyr::select(dplyr::all_of(cols_to_keep))
) |>
  dplyr::mutate(
    area = dplyr::case_when(Latitude_dd >= 36 ~ "north", .default = "south")
  ) |>
  dplyr::rename_all(tolower) |>
  dplyr::filter(!is.na(age_years), !is.na(length_cm)) |>
  dplyr::select(-latitude_dd)

# Load in the WCGOP raw confidential data
load("C:/Assessments/wcgop/data_2023/sablefish/raw_discard_lengths/raw_wcgop_discard_comps.Rdata")
wcgop <- raw_comps |>
  dplyr::filter(!is.na(length))
years <- sort(unique(raw_comps$year))
areas <- c("north", "south")

len_age_est <- NULL
for (y in years) {
  for(a in areas) {
    age_data <- survey_ages |>
      dplyr::filter(year == y, area == a)
    if (y == 2020) {
      age_data <- survey_ages |>
        dplyr::filter(year %in% c(y - 1, y + 1), area == a)
    }
    
    no_age_data <- wcgop |>
      dplyr::filter(year == y, area_location == a) |>
      dplyr::mutate(
        age_years = as.numeric(NA),
        length_cm = as.numeric(length)
      )
    
    # There are some years where small fish are in the wcgop data that 
    # are not in the survey data.  Add some dummy fish that would all 
    # be age-0:
    if (min(no_age_data$length_cm) < min(age_data$length_cm)) {
      dummy_data <- data.frame(
        year = rep(y, 5),
        age_years = rep(0, 5),
        length_cm = seq(8, 24, 4),
        area = rep(a, 5)
      )
      age_data <- dplyr::bind_rows(
        age_data, dummy_data
      )
    } # if statement
    age_length_count <- lencat(
      ~length_cm, 
      data = age_data, 
      startcat = floor(min(age_data$length_cm)), 
      w = 1)
    age_length_matrix <- with(age_length_count, table(LCat, age_years))
    age_length_prop <- prop.table(age_length_matrix, margin = 1)
    
    if (dim(no_age_data)[1] > 0) {
      temp <- alkIndivAge(
        key = age_length_prop, 
        formula = age_years ~ length_cm, 
        data = no_age_data)
      len_age_est <- dplyr::bind_rows(
        len_age_est, temp
      )
    } # if statement
  } # area loop
} # year loop

wcgop_age_est <- len_age_est |>
  dplyr::select(-age, -length_cm) |>
  dplyr::rename(age = age_years)

save(
  wcgop_age_est,
  file = "C:/Assessments/wcgop/data_2023/sablefish/raw_discard_lengths/raw_wcgop_discard_comps_w_estimated_ages.Rdata")

# Visualize the estimated ages compared to the data
all_data <- dplyr::bind_rows(
  survey_ages |> 
    dplyr::filter(year >= 2004) |>
    dplyr::mutate(type = "Data") |>
    dplyr::rename(area_location = area), 
  len_age_est |> 
    dplyr::mutate(type = "ALK") |>
    dplyr::select(year, area_location, age_years, length_cm, type)
) 
gg1 <- ggplot(all_data |> dplyr::filter(year < 2012),
       aes(x = age_years, y = length_cm, color = type)) +
  geom_point(alpha = 0.25) +
  theme_bw() + 
  scale_color_viridis_d(begin = 0, end = 0.5) +
  xlab("Age") + ylab("Length (cm)") +
  facet_wrap(c("year", "area_location"))
gg2 <- ggplot(all_data |> dplyr::filter(year %in% 2012:2017),
              aes(x = age_years, y = length_cm, color = type)) +
  geom_point(alpha = 0.25) +
  theme_bw() + 
  scale_color_viridis_d(begin = 0, end = 0.5) +
  xlab("Age") + ylab("Length (cm)") +
  facet_wrap(c("year", "area_location"))
gg3 <- ggplot(all_data |> dplyr::filter(year %in% 2018:2023),
              aes(x = age_years, y = length_cm, color = type)) +
  geom_point(alpha = 0.25) +
  theme_bw() + 
  scale_color_viridis_d(begin = 0, end = 0.5) +
  xlab("Age") + ylab("Length (cm)") +
  facet_wrap(c("year", "area_location"))
ggsave(gg1,
       file = here::here("data-raw", "discard", "wcgop", "figures", "data_alk_2004-2011.png"),
       height = 12, width = 12)
ggsave(gg2,
       file = here::here("data-raw", "discard", "wcgop", "figures", "data_alk_2012-2017.png"),
       height = 12, width = 12)
ggsave(gg3,
       file = here::here("data-raw", "discard", "wcgop", "figures", "data_alk_2018-2023.png"),
       height = 12, width = 12)

summarize_all_data <- Summarize(
  length_cm ~ age_years,
  data = all_data, 
  digits = 0) 

gg4 <- ggplot(data = all_data, aes(x = age_years, y = length_cm)) + 
  geom_point(shape = 1) + 
  geom_line(data = summarize_all_data, aes(x = age_years, y = mean), color = "blue", linewidth = 2) +
  theme_bw() + 
  xlab("Age") + ylab("Length (cm)")
ggsave(gg4,
       file = here::here("data-raw", "discard", "wcgop", "figures", "compare_data_alk_all_years.png"),
       height = 12, width = 12)


a <- ggplot(wcgop_age_est[!is.na(wcgop_age_est$age), ], aes(x = age, y = length)) +
  geom_point() +
  ylim(c(0, 85)) +
  ylab("Length (cm)") + xlab("Estimated Age") + 
  theme_bw() + 
  theme(
    strip.text = element_text(size = 14),
    axis.text=element_text(size = 12),
    axis.title=element_text(size = 14)) + 
  facet_wrap("gear_groups", nrow = 3)
ggsave(a,
       file = here::here("report", "figures", "compare_data_alk_all_years.png"),
       height = 12, width = 12)

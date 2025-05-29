# devtools::load_all()
load("data/data_commercial_bds.rda")

require(dplyr)
require(tidyr)
n_2019 = read.csv(here::here("data-raw", "historical", "samplesizes_2019.csv"))
n_2019 = n_2019 %>%
  select(Year, Fishery, Ages, Lengths) %>%
    rename(`Ages (2019)` = Ages, 
         `Lengths (2019)` = Lengths)

data_commercial_bds$Fishery = with(data_commercial_bds, 
                                ifelse(geargroup == "TWL", "Trawl", "Fixed"))

n_2024 = data_commercial_bds %>%
  rename(Year = year, 
         `Ages (2024)` = age_years, 
         `Lengths (2024)` = length_cm) %>%
    select(c(Year, Fishery, `Ages (2024)`, `Lengths (2024)`)) %>%
  group_by(Year, Fishery) %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  as.data.frame()

n_compare = n_2024 %>% 
  full_join(n_2019) %>%
    complete(Year, Fishery) %>%
  mutate(across(everything(), ~ tidyr::replace_na(., 0))) %>%
  select(c(Year, Fishery, `Ages (2019)`, `Ages (2024)`)) %>%
  mutate(Diff. = `Ages (2024)` - `Ages (2019)`)


write.csv(n_compare, here::here("data-processed", "samplesizes_2024.csv"), row.names=F)

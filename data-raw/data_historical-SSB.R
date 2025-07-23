get_ssb_timeseries <- function(year){
   fname <- file.path(here::here(), "model", "_bridging", paste0("0_",year,"_model"))
    model <- r4ss::SS_output(fname, verbose = FALSE, printstats = FALSE)
    return(
        model$timeseries[,c("Yr", "SpawnBio")] |> 
          tibble::as_tibble() |>
          dplyr::rename(
              Year = Yr,
              Value = SpawnBio
          ) |>
          dplyr::mutate(
              Quantity = "SSB (MT)",
              Assessment_Year = year
          ) |>
          dplyr::select(
              Year,
              Quantity,
              Assessment_Year,
              Value
          ) |>
          dplyr::filter(Year <= year)
    )
}

file_path <- file.path(here::here(), "data-raw", "historical", "SIS_Timeseries_Sablefish_2005-2019_ForR.csv")
data <- readr::read_csv(file_path) |> 
  tibble::as_tibble() |> 
  dplyr::select(Year, dplyr::contains("SSB")) |>
  dplyr::rename(
      "SSB_2005_MT" = SSB.2005.mt,
      "SSB_2007_MT" = SSB.2007.mt,
      "SSB_2011_MT" = SSB.2011.mt,
      "SSB_2015_MT" = SSB.2015.mt,
  ) |>
  tidyr::pivot_longer(
      cols = 2:5,
      names_to = c("Quantity"),
      values_to = "Value"
  ) |>
  tidyr::separate(
      Quantity,
      into = c("Quantity", "Assessment_Year"),
      sep = "_",
      remove = TRUE
  ) |>
  dplyr::mutate(
      Year = as.integer(Year),
      Assessment_Year = as.double(Assessment_Year),
  ) |>
  dplyr::filter(!is.na(Value))

ssb19 <- get_ssb_timeseries(2019)
ssb21 <- get_ssb_timeseries(2021)
ssb23 <- get_ssb_timeseries(2023)
base_25 <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model")) 
ssb25 <- base_25$timeseries[,c("Yr", "SpawnBio")] |> 
  tibble::as_tibble() |>
  dplyr::rename(
    Year = Yr,
    Value = SpawnBio
  ) |>
  dplyr::mutate(
    Quantity = "SSB (MT)",
    Assessment_Year = 2025
  ) |>
  dplyr::select(
    Year,
    Quantity,
    Assessment_Year,
    Value
  ) |>
  dplyr::filter(Year <= 2025)

historical_ssb <- dplyr::bind_rows(data, ssb19, ssb21, ssb23,ssb25) |>
  dplyr::mutate(
      Assessment_Year = factor(Assessment_Year)
  )

write_named_csvs(
    historical_ssb,
    dir = here::here("data-processed")
)

recode_Project <- function(x, gls = FALSE) {
  out <- dplyr::case_match(
    .x = x,
    "AFSC.Slope" ~ "aslope",
    "NWFSC.Combo" ~ "wcgbt",
    "NWFSC.Slope" ~ "nslope",
    "Triennial" ~ "tri",
    .default = NA_character_
  )
  if (gls) {
    out <- glue::glue("\\glsentryshort{{s-{out}}}")
  }
  return(out)
}

recode_fleet_cw <- function(x) {
  x <- tolower(as.character(x))
  dplyr::case_when(
    x %in% c("twl", "trawl") ~ "1",
    x %in% c("hkl") ~ "2",
    x %in% c("pot") ~ "3",
    x %in% c("akshlf", "triennial", "tri") ~ "4",
    x %in% c("akslp", "afsc.slope", "aslope", "ak slope") ~ "5",
    x %in% c("nwslp", "nwfsc.slope", "nslope", "nwfsc slope") ~ "6",
    x %in% c("nwcbo", "nwfsc.combo", "wcgbt", "wcgbts") ~ "7",
    x %in% c("env", "env. index") ~ "8",
    TRUE ~ NA_character_
  ) |>
    as.numeric()
}

recode_fleet_area <- function(x) {
  x <- tolower(x)
  dplyr::case_when(
    x %in% c("north-trawl") ~ "1",
    x %in% c("north-hkl") ~ "2",
    x %in% c("north-pot") ~ "3",
    x %in% c("south-trawl") ~ "4",
    x %in% c("south-hkl") ~ "5",
    x %in% c("south-pot") ~ "6",
    x %in% c("akshlf", "triennial", "tri") ~ "7",
    x %in% c("akslp", "afsc.slope", "aslope", "ak slope") ~ "8",
    x %in% c("nwslp", "nwfsc.slope", "nslope", "nwfsc slope") ~ "9",
    x %in% c("nwcbo", "nwfsc.combo", "wcgbt", "wcgbts") ~ "10",
    x %in% c("env", "env. index") ~ "11",
    TRUE ~ NA_character_
  ) |>
    as.numeric()
}

recode_fleet_text_cw <- function(x, case = FALSE) {
  stopifnot(inherits(x, "numeric") || inherits(x, "integer"))
  out <- dplyr::case_when(
    x == 1 ~ "trawl fleet",
    x == 2 ~ "hook-and-line fleet",
    x == 3 ~ "pot fleet",
    x == 4 ~ "\\glsentryshort{s-tri}",
    x == 5 ~ "\\glsentryshort{s-aslope}",
    x == 6 ~ "\\glsentryshort{s-nslope}",
    x == 7 ~ "\\glsentryshort{s-wcgbt}",
    x == 8 ~ "environmental survey",
  )
  if (case == "title") {
    out <- stringi::stri_trans_totitle(out)
  }
  return(out)
}

recode_fleet_text_area <- function(x, case = FALSE) {
  stopifnot(inherits(x, "numeric") || inherits(x, "integer"))
  out <- dplyr::case_when(
    x == 1 ~ "trawl fleet north",
    x == 2 ~ "hook-and-line fleet north",
    x == 3 ~ "pot fleet north",
    x == 4 ~ "trawl fleet south",
    x == 5 ~ "hook-and-line fleet south",
    x == 6 ~ "pot fleet south",
    x == 7 ~ "\\glsentryshort{s-tri}",
    x == 8 ~ "\\glsentryshort{s-aslope}",
    x == 9 ~ "\\glsentryshort{s-nslope}",
    x == 10 ~ "\\glsentryshort{s-wcgbt}",
    x == 11 ~ "environmental survey",
  )
  if (case == "title") {
    out <- stringi::stri_trans_totitle(out)
  }
  return(out)
}

recode_fleet_figure_cw <- function(x) {
  stopifnot(inherits(x, "numeric") || inherits(x, "integer"))
  dplyr::case_when(
    x == 1 ~ "Trawl",
    x == 2 ~ "Hook-and-Line",
    x == 3 ~ "Pot",
    x == 4 ~ "Triennial",
    x == 5 ~ "AFSC Slope",
    x == 6 ~ "NWFSC Slope",
    x == 7 ~ "WCGBT",
    x == 3 ~ "Env. Index"
  )
}

recode_fleet_figure_area <- function(x) {
  stopifnot(inherits(x, "numeric") || inherits(x, "integer"))
  dplyr::case_when(
    x == 1 ~ "Trawl North",
    x == 2 ~ "Hook-and-Line North",
    x == 3 ~ "Pot North",
    x == 4 ~ "Trawl South",
    x == 5 ~ "Hook-and-Line South",
    x == 6 ~ "Pot North",
    x == 4 ~ "Triennial",
    x == 5 ~ "AFSC Slope",
    x == 6 ~ "NWFSC Slope",
    x == 7 ~ "WCGBT",
    x == 11 ~ "Env. Index"
  )
}

recode_partition_text <- function(x) {
  dplyr::case_when(
    x == 0 ~ "whole",
    x == 1 ~ "discarded",
    x == 2 ~ "retained"
  )
}

# to do - Could format the names
recode_parameter_names <- function(x, fleet_names) {
  temp <- x |>
    gsub(pattern = "Mat50.+(_[FM])", replacement = "Maturity at 50\\\\%\\1") |>
    gsub(pattern = "Mat_slope(_[FM])", replacement = "Maturity slope\\1") |>
    gsub(pattern = "(.+)_(Fem)(.+)", replacement = "\\1 (female)") |>
    gsub(pattern = "(.+)_(Mal)(.+)", replacement = "\\1 (male)") |>
    gsub(pattern = "NatM", replacement = "$M$") |>
    gsub(pattern = "L_at_A", replacement = "Length-at-age ") |>
    gsub(pattern = "VonBert_([a-zA-Z]+)", replacement = "von Bertalanffy \\1") |>
    gsub(pattern = "CV_([a-zA-Z]+)", replacement = "Growth CV \\1") |>
    gsub(pattern = "Wtlen_([0-9+])", replacement = "Weight-length \\1") |>
    gsub(pattern = "FracFemale(_.+)", replacement = "Frac. female\\1") |>
    gsub(pattern = "inter", replacement = "intercept") |>
    gsub(pattern = "_base|_uniform", replacement = "") |>
    # Stock--recruit parameters
    gsub(pattern = "SR_BH_steep", replacement = "$h$") |>
    gsub(pattern = "SR_LN\\(R0\\)", replacement = "$ln(R\\_0)$") |>
    gsub(pattern = "SR_sigmaR", replacement = "$\\\\sigma\\_R$") |>
    gsub(pattern = "SR_autocorr", replacement = "Stock--recr. $\\\\rho\\_1$") |>
    # Q parameters
    gsub(pattern = "extraSD", replacement = "extra SD") |>
    gsub(pattern = "LnQ", replacement = "$ln$(Q)") |>
    # Selectivity parameters
    gsub(pattern = "_I.+Age_", replacement = " age ") |>
    gsub(pattern = "_([A-Z]+)(\\([0-9]+\\))", replacement = " *\\2_") |>
    gsub(pattern = "_BLK[0-9]+repl_", replacement = " ") |>
    gsub(pattern = "_logit|_DblN|_se|Mort|", replacement = "") |>
    gsub(pattern = "_([0-9]+)Male", replacement = " \\1 male ") |>
    gsub(pattern = "maleoffset", replacement = "male offset") |>
    gsub(pattern = "atZero", replacement = "at 0") |>
    gsub(pattern = "atDogleg", replacement = "at dogleg") |>
    gsub(pattern = "atMaxage", replacement = "at max. age") |>
    gsub(pattern = "AgeSel", replacement = "Age") |>
    gsub(pattern = "(.+)_RecrDev_", replacement = "\\1 Recr. Dev. ") |>
    gsub(pattern = "ForeRecr_", replacement = "Forecast Recr. Dev. ")
  contains_fleet <- grepl("*\\([0-9]+)", temp)
  if (any(contains_fleet) && !missing(fleet_names)) {
    fleet_numbers <- as.numeric(gsub(
      pattern = ".+\\(|\\).+",
      replacement = "",
      x = temp[contains_fleet]
    ))
    replace_number_with <- fleet_names[fleet_numbers]
    new_text <- purrr::map2(
      .x = temp[contains_fleet],
      .y = replace_number_with,
      .f = \(xx, yy) gsub("\\*\\([0-9]+)", yy, x = xx)
    ) |>
      gsub(pattern = "_", replacement = " \\1")
    temp[contains_fleet] <- new_text
  }
  # Do NOT do the following if more than 1 area
  if (!any(grepl("_GP_2", x))) {
    temp <- temp |>
      gsub(pattern = "_GP_1", replacement = "")
  }
  final <- gsub(pattern = "_([a-zA-Z ])", replacement = " \\1", x = temp)
  return(final)
}
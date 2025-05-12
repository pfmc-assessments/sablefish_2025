#' Recode utility functions to convert from character string to fleet numbers.
#'
#' 
#' @param x String 
#' @param gls Option to glue the recoded object. Default of FALSE 
recode_project <- function(x, gls = FALSE) {
  out <- dplyr::case_match(
    .x = x,
    "AFSC/RACE Slope Survey" ~ "afscslope",
    "Groundfish Slope and Shelf Combination Survey" ~ "wcgbt",
    "Groundfish Slope Survey" ~ "nwfscslope",
    "Groundfish Triennial Shelf Survey" ~ "triennial",
    .default = NA_character_
  )
  if (gls) {
    out <- glue::glue("\\glsentryshort{{s-{out}}}")
  }
  return(out)
}

recode_project_doc <- function(x, gls = FALSE) {
  out <- dplyr::case_match(
    .x = x,
    "AFSC/RACE Slope Survey" ~ "AFSC Slope",
    "Groundfish Slope and Shelf Combination Survey" ~ "WCGBT",
    "Groundfish Slope Survey" ~ "NWFSC Slope",
    "Groundfish Triennial Shelf Survey" ~ "Triennial",
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
    x %in% c("hkl", "fixed-gear") ~ "2",
    x %in% c("pot") ~ "3",
    x %in% c("akshlf", "triennial", "triennial_early", "triennial_late", "tri", "groundfish triennial shelf survey") ~ "7",
    x %in% c("nwslp", "nwfsc.slope", "nwfsc_slope", "nslope", "nwfsc slope", "groundfish slope survey") ~ "8",
    x %in% c("nwcbo", "nwfsc.combo", "wcgbt", "wcgbts", "groundfish slope and shelf combination survey") ~ "9",
    x %in% c("env", "env. index") ~ "10",
    TRUE ~ NA_character_
  ) |>
    as.numeric()
}


recode_fleet_text_cw <- function(x, case = FALSE) {
  stopifnot(inherits(x, "numeric") || inherits(x, "integer"))
  out <- dplyr::case_when(
    x == 1 ~ "trawl",
    x == 2 ~ "hkl",
    x == 3 ~ "pot",
    x == 4 ~ "trawl discard",
    x == 5 ~ "hkl discard",
    x == 6 ~ "pot discard",
    x == 7 ~ "\\glsentryshort{s-tri}",
    x == 8 ~ "\\glsentryshort{s-nslope}",
    x == 9 ~ "\\glsentryshort{s-wcgbt}",
    x == 10 ~ "environmental survey",
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
    x == 7 ~ "Triennial",
    x == 8 ~ "NWFSC Slope",
    x == 9 ~ "WCGBT",
    x == 10 ~ "Env. Index"
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
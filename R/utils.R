#' Write csv files with same name as object to `dir`
#'
#' Pass an unlimited amount of data frames to the function via `...`, i.e.,
#' `ldots`, and save each data frame as a csv file without row names using the
#' object name as the file name. The default is to save them in your current
#' working directory but you can change it to any directory you want. The name
#' of the function is plural to emphasize that it is vectorized and can
#' simultaneously save multiple objects to individual files, thanks to
#' [purrr::map()] and friends.
#'
#' @param dir A string providing the file path to the directory where you want
#'   to save all the csv files. The directory does not need to currently exist
#'   because it will be created.
#' @param ... As many data frames as you want. Make sure the objects have good
#'   names because the names will be used to save the files.
#' @noRd
write_named_csvs <- function(..., dir = getwd()) {
  fs::dir_create(dir)
  dots <- match.call(expand.dots = FALSE)[["..."]]
  file_names <- fs::path(
    dir,
    paste0(purrr::map_chr(as.list(dots), deparse), ".csv")
  )
  purrr::map2(
    .x = list(...),
    .y = file_names,
    .f = \(x, y) utils::write.csv(x = x, file = y, row.names = FALSE)
  )
  return(file_names)
}
#' @noRd
#' @examples
#' \dontrun{
#' copy_model_dir(
#'   "\\\\nwcfile/fram/Assessments/CurrentAssessments/sablefish_2023/models/2023/FixRetSel",
#'   "c:/github/pfmc-assessments/sablefish/models/2023/FixRetSel"
#' )
#' }
copy_model_dir <- function(dir_old, dir_new, force = FALSE) {
  dir_old <- gsub("\\\\", "/", dir_old)
  if (!force && file.exists(dir_new)) {
    message("`force = FALSE` but the directory already exists")
    message("Change to `force = TRUE` to overwrite files.")
    return(invisible(FALSE))
  }
  fs::dir_create(dir_new)
  # Base
  message("Copying base-model files.")
  message("This will take some time!")
  files_old <- fs::dir_ls(fs::path(dir_old, "base"), type = "file")
  fs::dir_create(fs::path(dir_new, "base"))
  fs::file_copy(
    files_old,
    gsub(dir_old, dir_new, files_old),
    overwrite = TRUE
  )
  # Others
  message("Copying .tex, .png, .csv, and .md files.")
  files_old <- fs::dir_ls(
    dir_old, regexp = "tex|png|csv|md", recurse = TRUE
  )
  files_old <- files_old[!grepl("plots/", files_old)]
  fs::dir_create(fs::path(dir_new, unique(basename(dirname(files_old)))))
  fs::file_copy(
    files_old,
    gsub(dir_old, dir_new, files_old),
    overwrite = TRUE
  )
  message("Finished copying files to ", dir_new)
  return(invisible(TRUE))
}

read_all_models_fast <- function(dir, ...) {
  output <- purrr::map(
    .x = fs::dir_ls(path = dir, type = "directory"),
    .f = \(x) r4ss::SS_output(
      x,
      verbose = FALSE,
      printstats = FALSE,
      covar = FALSE,
      NoCompOK = TRUE,
      compfile = NULL,
      ...
    )
  )
  invisible(return(output))
}

bind_compositions <- function(x) {
  stopifnot(inherits(x, "list"))
  good_names <- colnames(x[[1]])
  one_data_frame <- purrr::map_dfr(x, \(y) setNames(y, good_names))
  old_names <- colnames(one_data_frame)
  new_names <- gsub("^[UMF]([0-9]+)\\.*[0-9]*$", "U\\1", old_names)
  no_dots <- gsub("\\.{3}[0-9]+$", "", new_names)
  colnames(one_data_frame) <- no_dots
  return(one_data_frame)
}

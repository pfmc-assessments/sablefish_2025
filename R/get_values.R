#' Function to pull values from the read in report file and calculate the
#  confidence intervals.
#'
#' @param replist r4ss model output list created by r4ss::SS_output()
#' @param label description
#' @param yrs Vector of years.
#' @param ci_value To calculate confidence intervals, the desired interval must
#'   be specified. The default is 0.95.
#' @param single Default is FALSE
#'
#' @return
#' Individual .csv files for each executive summary table and additional tables
#' (catch, timeseries, numbers-at-age).
#' @author Chantel R. Wetzel, Kelli F. Johnson, Ian G. Taylor
#' @export
#'
get_values <- function(replist, label, yrs, ci_value, single = FALSE) {
  dat <- replist[["derived_quants"]]
  if (label == "Main_RecrDev" || label == "Late_RecrDev" || label == "ForeRecr") {
    dat <- replist[["parameters"]]
  }
  
  if (!single) {
    value <- dat[grep(label, dat[["Label"]]), ]
    value <- value[value[["Label"]] >= paste0(label, "_", yrs[1]) &
                     value[["Label"]] <= paste0(label, "_", max(yrs)), ]
    dq <- value[["Value"]]
    ind <- names(value) %in% c("StdDev", "Parm_StDev")
    sd <- value[, ind]
  }
  
  if (single) {
    value <- dat[grep(label, dat[["Label"]])[1], ]
    dq <- value[["Value"]]
    sd <- value[["StdDev"]]
  }
  
  if (label == "Recr" || label == "Recr_virgin") {
    low <- exp(log(dq) - qnorm(1 - (1 - ci_value) / 2) * sqrt(log(1 + (sd / dq) * (sd / dq))))
    high <- exp(log(dq) + qnorm(1 - (1 - ci_value) / 2) * sqrt(log(1 + (sd / dq) * (sd / dq))))

  }
  if (label != "Recr" && label != "Recr_virgin") {
    low <- dq - qnorm(1 - (1 - ci_value) / 2) * sd
    high <- dq + qnorm(1 - (1 - ci_value) / 2) * sd
  }
  
  if (!single) {
    if (length(dq) > 0) {
      return(data.frame(yrs, dq, low, high))
    } else {
      return(NULL)
    }
  }
  if (single) {
    return(data.frame(dq, low, high))
  }
}
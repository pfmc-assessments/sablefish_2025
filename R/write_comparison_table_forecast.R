#' Table of forecast derived quantities across models
#'
#' @param dir Directory location to save the output rda file. 
#' @param model_summary List object created by `r4ss::SSsummarize()`.
#' @param model_names Text string of model names to add to table columns
#' @param add_name Optional text string that can be appended to the saved rda 
#'   file.
#' 
#' @export
create_comparison_table_forecast <- function(
  dir,
  model_summary,
  model_names,
  add_name = NULL) {

  x <- model_summary
  ii <- 1:length(model_names)
  n <- length(model_names)
  param <- matrix(NA, 16, max(ii))

  param <-  rbind(
    as.numeric(x$quants %>%
      filter(str_detect(Label, "OFL") & Yr == 2025) %>%
      select(model1, model2, model3)),
    as.numeric(x$quants %>%
                 filter(str_detect(Label, "OFL") & Yr == 2027) %>%
                 select(model1, model2, model3)),
    as.numeric(x$quants %>%
                 filter(str_detect(Label, "OFL") & Yr == 2029) %>%
                 select(model1, model2, model3)), 
    as.numeric(x$quants %>%
                 filter(str_detect(Label, "OFL") & Yr == 2031) %>%
                 select(model1, model2, model3)),
    as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2025", 1:n]),
    as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2027", 1:n]),
    as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2029", 1:n]),
    as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2031", 1:n]),
    as.numeric(x$SpawnBio[x$SmryBio$Yr == "2025", 1:n]),
    as.numeric(x$SpawnBio[x$SmryBio$Yr == "2027", 1:n]),
    as.numeric(x$SpawnBio[x$SmryBio$Yr == "2029", 1:n]),
    as.numeric(x$SpawnBio[x$SmryBio$Yr == "2031", 1:n]),
    as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2025", 1:n]),
    as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2027", 1:n]),
    as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2029", 1:n]),
    as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2031", 1:n])
  )
  
  param <- as.data.frame(param)
  colnames(param) <- model_names
  rownames(param) <- c("OFL 2025",
                    "OFL 2027",
                    "OFL 2029",
                    "OFL 2031",
                    "Spawning Biomass 2025",
                    "Spawning Biomass 2027",
                    "Spawning Biomass 2029",
                    "Spawning Biomass 2030",
                    "Summary (age 3+) Biomass 2025",
                    "Summary (age 3+) Biomass 2027",
                    "Summary (age 3+) Biomass 2029",
                    "Summary (age 3+) Biomass 2030",
                    "Fraction Unfished 2025",
                    "Fraction Unfished 2027",
                    "Fraction Unfished 2029",
                    "Fraction Unfished 2031")
  
  param_table <- list()
  param_table$table <- param
  param_table$cap <- "Sensitivities relative to the base model."
  save(param,
       file = file.path(dir, paste0(add_name, "_model_para_comp.rda"))
    )
  write.csv(param,
       file = file.path(dir, paste0(add_name, "_model_para_comp.csv"))
    )
}
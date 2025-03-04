#' Table of estimated param and derived quantities across models
#'
#' @param dir Directory
#' @param model_list List of model folder names.  These folders should be inside 
#'   the directory.
#' @param model_names Text string of model names to add to figure legend.
#' @param add_name Optional text string that can be appended to the saved csv 
#'   file.
#' 
#' @export
create_comparison_table <- function(
  dir,
  model_list,
  model_names,
  add_name = NULL) {
  

  for (a in 1:length(model_list)) {
    model <- r4ss::SS_output(file.path(dir, model_list[a]))
    if (a == 1) {
      all_models <- model
    } else {
      all_models <- list(all_models, model)
    }
  }

  x <- SSsummarize(all_models)
  ii <- 1:length(model_names)
  n <- length(model_names)
  out <- matrix(NA, 24, max(ii))

  out <-  rbind(
    as.numeric(x$likelihoods[x$likelihoods$Label == "TOTAL",1:n]), 
    as.numeric(x$likelihoods[x$likelihoods$Label == "Survey",1:n]), 
    as.numeric(x$likelihoods[x$likelihoods$Label == "Length_comp",1:n]),
    as.numeric(x$likelihoods[x$likelihoods$Label == "Age_comp",1:n]), 
    as.numeric(x$likelihoods[x$likelihoods$Label == "Recruitment",1:n]), 
    as.numeric(x$likelihoods[x$likelihoods$Label == "Forecast_Recruitment",1:n]),
    as.numeric(x$likelihoods[x$likelihoods$Label == "Parm_priors",1:n]),
    as.numeric(x$pars[x$pars$Label == "SR_LN(R0)", 1:n]), 
    as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_Virgin", 1:n]),
    as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2023", 1:n]),
    as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2023", 1:n]), 
    as.numeric(x$quants[x$quants$Label == "Dead_Catch_SPR", 1:n]),
    as.numeric(x$pars[x$pars$Label == "SR_BH_steep", 1:n]),
    as.numeric(x$pars[x$pars$Label == "NatM_uniform_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "L_at_Amin_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "L_at_Amax_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "VonBert_K_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "CV_young_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "CV_old_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "NatM_uniform_Mal_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "L_at_Amin_Mal_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "L_at_Amax_Mal_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "VonBert_K_Mal_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "CV_young_Mal_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "CV_old_Mal_GP_1", 1:n]) )  

  out <- as.data.frame(out)
  colnames(out) <- modelnames
  rownames(out) <- c("Total Likelihood",
                    "Survey Likelihood",
                    "Length Likelihood",
                    "Age Likelihood",
                    "Recruitment Likelihood",
                    "Forecast Recruitment Likelihood",
                    "Parameter Priors Likelihood",
                    "log(R0)",
                    "SB Virgin",
                    "SB 2023",
                    "Fraction Unfished 2023",
                    "Total Yield - SPR 50",
                    "Steepness",
                    "Natural Mortality - Female",
                    "Length at Amin - Female",
                    "Length at Amax - Female",
                    "Von Bert. k - Female",
                    "CV young - Female",
                    "CV old - Female",
                    "Natural Mortality - Male",
                    "Length at Amin - Male",
                    "Length at Amax - Male",
                    "Von Bert. k - Male",
                    "CV young - Male",
                    "CV old - Male")
  write.csv(out, file = file.path(dir, paste0(add_name, "model_param_comparisons.csv")))
}
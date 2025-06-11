#' Table of estimated param and derived quantities across models
#'
#' @param dir Directory location to save the output rda file. 
#' @param model_summary List object created by `r4ss::SSsummarize()`.
#' @param model_names Text string of model names to add to table columns
#' @param add_name Optional text string that can be appended to the saved rda 
#'   file.
#' 
#' @export
create_comparison_table <- function(
  dir,
  model_summary,
  model_names,
  add_name = NULL) {

  x <- model_summary
  ii <- 1:length(model_names)
  n <- length(model_names)
  param <- matrix(NA, 25, max(ii))

  param <-  rbind(
    as.numeric(x$likelihoods[x$likelihoods$Label == "TOTAL",1:n]), 
    as.numeric(x$likelihoods[x$likelihoods$Label == "Survey",1:n]), 
    as.numeric(x$likelihoods[x$likelihoods$Label == "Length_comp",1:n]),
    as.numeric(x$likelihoods[x$likelihoods$Label == "Age_comp",1:n]), 
    #as.numeric(x$likelihoods[x$likelihoods$Label == "Discard",1:n]), 
    #as.numeric(x$likelihoods[x$likelihoods$Label == "Mean_body_wt", 1:n]), 
    as.numeric(x$likelihoods[x$likelihoods$Label == "Recruitment",1:n]), 
    as.numeric(x$likelihoods[x$likelihoods$Label == "Forecast_Recruitment",1:n]),
    as.numeric(x$likelihoods[x$likelihoods$Label == "Parm_priors",1:n]),
    as.numeric(x$pars[x$pars$Label == "SR_LN(R0)", 1:n]), 
    as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_Virgin", 1:n]),
    as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2025", 1:n]),
    as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2025", 1:n]), 
    as.numeric(x$quants[x$quants$Label == "Dead_Catch_SPR", 1:n]),
    as.numeric(x$pars[x$pars$Label == "SR_BH_steep", 1:n]),
    as.numeric(x$pars[x$pars$Label == "NatM_uniform_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "L_at_Amin_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "L_at_Amax_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "VonBert_K_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "lnSD_young_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "LnSD_old_Fem_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "NatM_uniform_Mal_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "L_at_Amin_Mal_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "L_at_Amax_Mal_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "VonBert_K_Mal_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "lnSD_young_Mal_GP_1", 1:n]),
    as.numeric(x$pars[x$pars$Label == "LnSD_old_Mal_GP_1", 1:n]) 
  )  

  param <- as.data.frame(param)
  colnames(param) <- model_names
  rownames(param) <- c("Total Likelihood",
                    "Survey Likelihood",
                    "Length Likelihood",
                    "Age Likelihood",
                    #"Discard Likelihood",
                    #"Body Weight Likelihood",
                    "Recruitment Likelihood",
                    "Forecast Recruitment Likelihood",
                    "Parameter Priors Likelihood",
                    "log(R0)",
                    "SB Virgin",
                    "SB 2025",
                    "Fraction Unfished 2025",
                    "Total Yield - SPR 45",
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
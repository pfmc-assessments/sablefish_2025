#' Plot Age-Based Selectivity Curve
#' 
#' Create a plot of selectivity-at-age for a given fishery or survey
#' fleet. Plots show selectivity curves for all model timeblocks and
#' available sexes.
#' 
#' @param model_out model object created by [r4ss::SS_output()]
#' @param fleet_num the fleet for which to plot selectivity
#' 
#' @return ggplot plot object
#' @export plot_fleet_selectivity
#' 
#' @examples
#' 
#' fleet_num <- 1 
#' mod_out <- r4ss::SS_output(dir=model_dir)
#' plot_fleet_selectivity(model_out, fleet_num)
#' 
#' 
#' sel1 <- plot_fleet_selectivity(model_out, 1)
#' sel2 <- plot_fleet_selectivity(model_out, 2)
#' sel3 <- plot_fleet_selectivity(model_out, 3)
#' sel4 <- plot_fleet_selectivity(model_out, 4)
#' sel5 <- plot_fleet_selectivity(model_out, 5)
#' sel6 <- plot_fleet_selectivity(model_out, 6)+theme(legend.position.inside=c(0.65, 0.3))
#' sel7 <- plot_fleet_selectivity(model_out, 7)
#' sel7_leg <- sel7+guides(shape=guide_legend(), linetype=guide_legend())
#' legend <- cowplot::get_plot_component(sel7_leg, 'guide-box-inside', return_all = TRUE)
#' (sel1+sel2+sel3+sel4+sel5+sel6+sel7+legend$grob[[2]])+plot_layout(axes="collect")
#' 
plot_fleet_selectivity <- function(model_out, fleet_num){
    data <- r4ss::SS_readdat(file.path(model_path, "data.ss"))
    fleet_name <- data$fleetnames[fleet_num]
    fleet_type <- data$fleetinfo %>% dplyr::filter(fleetname==fleet_name) %>% dplyr::pull(type)
    if(fleet_type == 1){
        fleet_years <- data$catch %>% dplyr::filter(fleet==fleet_num) %>% dplyr::pull(year)
        fleet_active_years <- c(min(fleet_years), max(fleet_years))
    }else if(fleet_type == 3){
        fleet_years <- data$CPUE %>% dplyr::filter(index==fleet_num) %>% dplyr::pull(year)
        fleet_active_years <- c(min(fleet_years), max(fleet_years))
    }

    ctl_file <- r4ss::SS_readctl_3.30(file.path(model_path, "control.ss"))
    tv_selex <- ctl_file$age_selex_parms_tv
    fleet_tv_entry <- rownames(tv_selex[grep(paste0("(",fleet_num,")"), rownames(tv_selex), fixed=TRUE),])
    fleet_end_blocks <- as.vector(sapply(fleet_tv_entry, \(x) stringr::str_extract(x, "\\d+$")))

    age_selex <- model_out$ageselex %>% dplyr::as_tibble() %>% dplyr::filter(Factor == "Asel")

    all_timeblock_years <- age_selex %>% dplyr::filter(!(Yr %in% c(min(Yr), max(Yr)))) %>% dplyr::pull(Yr) %>% unique %>% as.numeric %>% sort
    fleet_timeblock_years <- all_timeblock_years[which(all_timeblock_years %in% c(min(all_timeblock_years), fleet_end_blocks, max(fleet_active_years)))]
    fleet_timeblock_years_names <- fleet_timeblock_years
    fleet_timeblock_years_names[1] <- max(fleet_timeblock_years[1], fleet_active_years[1])
    fleet_timeblock_years_names[length(fleet_timeblock_years_names)] <- min(fleet_timeblock_years[length(fleet_timeblock_years)], fleet_active_years[2])

    timeblock_names <- sapply(1:(length(fleet_timeblock_years_names)-1), function(i){
        paste0(fleet_timeblock_years_names[i], "-",fleet_timeblock_years_names[i+1])
    })

    # Correct timeblock names by reducing the block end year by 1 for all blocks
    # except for the last one (so that the last block goes to the final model year).
    if(length(timeblock_names) > 1){
        timeblock_names[1:(length(timeblock_names)-1)] <- sapply(1:(length(timeblock_names)-1), function(i){
            block_start_year <- as.numeric(stringr::str_extract(timeblock_names[i], "^\\d+"))
            block_end_year <- as.numeric(stringr::str_extract(timeblock_names[i], "\\d+$"))
            return(paste0(block_start_year,"-",block_end_year-1))
        })
    }
    
    available_tb <- age_selex %>% dplyr::pull(Yr) %>% unique %>% as.numeric %>% sort
    filter_years <- fleet_timeblock_years 
    if(!(fleet_timeblock_years[1] %in% available_tb)){
        filter_years <- 2024
    }

    timeblock_key <- data.frame(Yr=fleet_timeblock_years, timeblock=c(timeblock_names, timeblock_names[length(timeblock_names)]))

    data <- age_selex %>% tidyr::pivot_longer(`0`:`70`, names_to="age", values_to="sel") %>% 
        dplyr::filter(Yr %in% filter_years, Fleet==fleet_num) %>%
        dplyr::mutate(
            age=as.integer(age),
            sex = factor(ifelse(Sex==1, "Female", "Male")),
            Fleet = factor(Fleet, labels=fleet_name)
        ) %>%
        dplyr::left_join(timeblock_key, by="Yr")
        

    plot <- ggplot(data, aes(x=age, y=sel, color=factor(timeblock), linetype=sex, shape=sex, group=interaction(sex, timeblock)))+
        geom_line()+
        geom_point(size=2)+
        labs(x="Age", y="Selectivity", color="Time Block", linetype="Sex", shape="Sex")+
        guides(shape="none", linetype="none")+
        scale_y_continuous(limits=c(0, 1), breaks=seq(0,1, 0.25), labels=seq(0,1,0.25))+
        scale_shape_manual(values=c(16, 1))+
        facet_wrap(~Fleet)+
        theme_bw()+
        theme(
            legend.position = "inside",
            legend.position.inside = c(0.65, 0.8),
            legend.text = element_text(size=12),
            legend.title = element_text(size=14),
            axis.title = element_text(size=14),
            axis.text = element_text(size=12),
            strip.text = element_text(size=16)
        )

    return(plot)
}


#' Plot Length-Based Retention Curve
#' 
#' Create a plot of retention-at-age for a given fishery or survey
#' fleet. Plots show selectivity curves for all model timeblocks.
#' 
#' @param model_out model object created by [r4ss::SS_output()]
#' @param fleet_num the fleet for which to plot selectivity
#' 
#' @return ggplot plot object
#' @export  plot_fleet_retention
#' 
#' @examples
#' 
#' fleet_num <- 1 
#' mod_out <- r4ss::SS_output(dir=model_dir)
#' plot_fleet_retention(model_out, fleet_num)
#' 
#' ret1 <- plot_fleet_retention(model_out, 1)
#' ret2 <- plot_fleet_retention(model_out, 2)
#' ret3 <- plot_fleet_retention(model_out, 3)
#' (ret1+ret2+ret3)+plot_layout(axes="collect")
#' 
plot_fleet_retention <- function(model_out, fleet_num){
    data <- r4ss::SS_readdat(file.path(model_path, "data.ss"))
    fleet_name <- data$fleetnames[fleet_num]
    fleet_type <- data$fleetinfo %>% filter(fleetname==fleet_name) %>% pull(type)
    if(fleet_type == 1){
        fleet_years <- data$catch %>% filter(fleet==fleet_num) %>% pull(year)
        fleet_active_years <- c(min(fleet_years), max(fleet_years))
    }else if(fleet_type == 3){
        fleet_years <- data$CPUE %>% filter(index==fleet_num) %>% pull(year)
        fleet_active_years <- c(min(fleet_years), max(fleet_years))
    }

    ctl_file <- r4ss::SS_readctl_3.30(file.path(model_path, "control.ss"))
    tv_selex <- ctl_file$size_selex_parms_tv
    fleet_tv_entry <- rownames(tv_selex[grep(paste0("(",fleet_num,")"), rownames(tv_selex), fixed=TRUE),])
    fleet_end_blocks <- as.vector(sapply(fleet_tv_entry, \(x) stringr::str_extract(x, "\\d+$")))

    age_selex <- model_out$sizeselex %>% as_tibble() %>% filter(Factor == "Ret")

    all_timeblock_years <- age_selex %>% filter(!(Yr %in% c(min(Yr), max(Yr)))) %>% pull(Yr) %>% unique %>% as.numeric %>% sort
    fleet_timeblock_years <- all_timeblock_years[which(all_timeblock_years %in% c(all_timeblock_years, fleet_end_blocks))]
    fleet_timeblock_years_names <- fleet_timeblock_years
    fleet_timeblock_years_names[1] <- max(fleet_timeblock_years[1], fleet_active_years[1])
    fleet_timeblock_years_names[length(fleet_timeblock_years_names)] <- min(fleet_timeblock_years[length(fleet_timeblock_years)], fleet_active_years[2])

    timeblock_names <- sapply(seq(1, length(fleet_timeblock_years_names)-2, 2), function(i){
        paste0(fleet_timeblock_years_names[i], "-",fleet_timeblock_years_names[i+1])
    })

    available_tb <- age_selex %>% pull(Yr) %>% unique %>% as.numeric %>% sort
    filter_years <- fleet_timeblock_years 
    if(!(fleet_timeblock_years[1] %in% available_tb)){
        filter_years <- 2024
    }

    timeblock_key <- data.frame(Yr=fleet_timeblock_years, timeblock=c(rep(timeblock_names, each=2), timeblock_names[length(timeblock_names)]))

    data <- age_selex %>% pivot_longer(6:ncol(age_selex), names_to="age", values_to="ret") %>% 
        filter(Yr %in% filter_years, Fleet==fleet_num) %>%
        mutate(
            age=as.integer(age),
            sex = factor(ifelse(Sex==1, "Female", "Male")),
            Fleet = factor(Fleet, labels=fleet_name)
        ) %>%
        left_join(timeblock_key, by=c("Yr"))
        

    plot <- ggplot(data, aes(x=age, y=ret, color=factor(timeblock), linetype=sex, shape=sex, group=interaction(sex, timeblock)))+
        geom_line()+
        geom_point(size=2)+
        labs(x="Age", y="Retention", color="Time Block", linetype="Sex", shape="Sex")+
        guides(shape="none", linetype="none")+
        scale_y_continuous(limits=c(0, 1), breaks=seq(0,1, 0.25), labels=seq(0,1,0.25))+
        scale_shape_manual(values=c(16, 1))+
        facet_wrap(~Fleet)+
        theme_bw()+
        theme(
            legend.position = "inside",
            legend.position.inside = c(0.75, 0.2),
            legend.text = element_text(size=12),
            legend.title = element_text(size=14),
            axis.title = element_text(size=14),
            axis.text = element_text(size=12),
            strip.text = element_text(size=16)
        )

    return(plot)
}

#' Plot Selectivity, Retention and Discard Mortality
#' 
#' Create a plot of selectivity, retention, discard mortality
#' -at-age across sexes and fishing fleets
#' 
#' @param model_out model object created by [r4ss::SS_output()]
#' @param model_path path to model directory containing control.ss file
#' @param sexes sexes across which plot (default 1:2)
#' @param years year to plot data for (default 2024)
#' 
#' @return ggplot plot object
#' @export  plot_sel_ret_discard
#' 
#' @examples
#' 
#' mod_out <- r4ss::SS_output(dir=model_dir)
#' 
#' plot_sel_ret_discard(mod_out, model_dir, sexes=1:2, year=2024)
#' 
plot_sel_ret_discard <- function(model_out, model_path, sexes=1:2, year=2024){
    data <- r4ss::SS_readdat(file.path(model_path, "data.ss"))
    fleet_names <- data$fleetinfo %>% dplyr::filter(type == 1) %>% dplyr::pull(fleetname)

    names(fleet_names) <- c(1:length(fleet_names))

    data <- model_out$ageselex %>% 
        dplyr::select(-Label) %>% 
        dplyr::filter(Factor %in% c("Asel", "Aret", "Amort"), Sex %in% sexes, Yr == year) %>% 
        tidyr::pivot_longer(7:ncol(.), names_to="age", values_to="val") %>%
        tidyr::pivot_wider(names_from="Factor", values_from="val") %>%
        dplyr::mutate(
            age = as.numeric(age),
            keep = Asel*Aret,
            disc = Asel*(1-Aret),
            dead = Asel*(Aret + (1-Aret)*Amort),
            Sex = factor(Sex, levels=c(1, 2), labels=c("Female", "Male")),
            Fleet = factor(Fleet, levels=names(fleet_names), labels=fleet_names)
        ) %>%
        dplyr::filter(!is.na(Fleet)) %>%
        tidyr::pivot_longer(Asel:dead, names_to="Quantity", values_to="value") %>%
        dplyr::mutate(
            Quantity = factor(
                Quantity, 
                levels=c("Asel", "Aret", "Amort", "keep", "dead", "disc"),
                labels = c("Selectivity", "Retention", "Discard Mortality", "Keep = Sel*Ret", "Dead = Sel*(Ret+(1-Ret)*Mort)", "Discard = Sel*(1-Ret)")
            )
        )

    plot <- ggplot2::ggplot(data, aes(x=age, value, color=Quantity, shape=Quantity))+
        ggplot2::geom_point()+
        ggplot2::geom_line()+
        ggplot2::scale_color_manual(values=c("blue", "red", "orange", "purple", "green", "grey"))+
        ggplot2::labs(x="Age", y="Selectivity, Retention, Mortality", color="", shape="")+
        ggplot2::facet_grid(rows=vars(Sex), cols=vars(Fleet))+
        ggplot2::theme_bw()+
        ggplot2::theme(
            legend.position = "bottom",
            legend.text = element_text(size=12),
            legend.title = element_text(size=14),
            axis.title = element_text(size=14),
            axis.text = element_text(size=12),
            strip.text = element_text(size=16)
        )

    return(plot)

}

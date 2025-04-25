#library(r4ss)
#library(tidyverse)
#library(stringr)
#
#model_path <- file.path(here::here(), "model", "13_m_prior")
#
#fleet_num = 5
#model_out <- r4ss::SS_output(dir=model_path)
#
#
#plots <- sapply(1:7, \(x) plot_fleet_selectivity(model_out, x))
#
#
#
#
#library(patchwork)
#library(cowplot)
#
#sel1 <- plot_fleet_selectivity(model_out, 1)
#sel2 <- plot_fleet_selectivity(model_out, 2)
#sel3 <- plot_fleet_selectivity(model_out, 3)
#sel4 <- plot_fleet_selectivity(model_out, 4)
#sel5 <- plot_fleet_selectivity(model_out, 5)
#sel6 <- plot_fleet_selectivity(model_out, 6)+theme(legend.position.inside=c(0.65, 0.3))
#sel7 <- plot_fleet_selectivity(model_out, 7)
#sel7_leg <- sel7+guides(shape=guide_legend(), linetype=guide_legend())
#
#legend <- cowplot::get_plot_component(sel7_leg, 'guide-box-inside', return_all = TRUE)
#cowplot::ggdraw(legend$grob[[2]])
#
#(sel1+sel2+sel3+sel4+sel5+sel6+sel7+plot_spacer()+legend$grob[[2]])+plot_layout(axes="collect")
#ggsave("~/Desktop/test.jpeg", height=11, width=8.5)
#
plot_fleet_selectivity <- function(
    model_path, 
    model_out, 
    fleet_num,
    data_file_name = "2025_sablefish_dat.ss",
    ctl_file_name = "2025_sablefish_ctl.ss"){
    data <- r4ss::SS_readdat(file.path(model_path, data_file_name))
    fleet_name <- data$fleetnames[fleet_num]
    fleet_type <- data$fleetinfo %>% filter(fleetname==fleet_name) %>% pull(type)
    if(fleet_type == 1){
        fleet_years <- data$catch %>% filter(fleet==fleet_num) %>% pull(year)
        fleet_active_years <- c(min(fleet_years), max(fleet_years))
    }else if(fleet_type == 3){
        fleet_years <- data$CPUE %>% filter(index==fleet_num) %>% pull(year)
        fleet_active_years <- c(min(fleet_years), max(fleet_years))
    }

    ctl_file <- r4ss::SS_readctl_3.30(file.path(model_path, ctl_file_name))
    tv_selex <- ctl_file$age_selex_parms_tv
    fleet_tv_entry <- rownames(tv_selex[grep(paste0("(",fleet_num,")"), rownames(tv_selex), fixed=TRUE),])
    fleet_end_blocks <- as.vector(sapply(fleet_tv_entry, \(x) stringr::str_extract(x, "\\d+$")))

    age_selex <- model_out$ageselex %>% as_tibble() %>% filter(Factor == "Asel")

    all_timeblock_years <- age_selex %>% filter(!(Yr %in% c(min(Yr), max(Yr)))) %>% pull(Yr) %>% unique %>% as.numeric %>% sort
    fleet_timeblock_years <- all_timeblock_years[which(all_timeblock_years %in% c(min(all_timeblock_years), fleet_end_blocks, max(fleet_active_years)))]
    fleet_timeblock_years_names <- fleet_timeblock_years
    fleet_timeblock_years_names[1] <- max(fleet_timeblock_years[1], fleet_active_years[1])
    fleet_timeblock_years_names[length(fleet_timeblock_years_names)] <- min(fleet_timeblock_years[length(fleet_timeblock_years)], fleet_active_years[2])

    timeblock_names <- sapply(1:(length(fleet_timeblock_years_names)-1), function(i){
        paste0(fleet_timeblock_years_names[i], "-",fleet_timeblock_years_names[i+1])
    })

    available_tb <- age_selex %>% pull(Yr) %>% unique %>% as.numeric %>% sort
    filter_years <- fleet_timeblock_years 
    if(!(fleet_timeblock_years[1] %in% available_tb)){
        filter_years <- 2024
    }

    timeblock_key <- data.frame(Yr=fleet_timeblock_years, timeblock=c(timeblock_names, timeblock_names[length(timeblock_names)]))

    data <- age_selex %>% pivot_longer(`0`:`70`, names_to="age", values_to="sel") %>% 
        filter(Yr %in% filter_years, Fleet==fleet_num) %>%
        mutate(
            age=as.integer(age),
            sex = factor(ifelse(Sex==1, "Female", "Male")),
            Fleet = factor(Fleet, labels=fleet_name)
        ) %>%
        left_join(timeblock_key, by="Yr")
        

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

    # show(plot)
    return(plot)
}
#
#
#
#ret1 <- plot_fleet_retention(model_out, 1)
#ret2 <- plot_fleet_retention(model_out, 2)
#ret3 <- plot_fleet_retention(model_out, 3)
#ret7_leg <- sel7+guides(shape=guide_legend(), linetype=guide_legend())
#
#(ret1+ret2+ret3)+plot_layout(axes="collect")
#ggsave("~/Desktop/test_ret.jpeg", width=8.5, height=6)
#
plot_fleet_retention <- function(model_path, model_out, fleet_num){
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


library(tidyverse)
library(patchwork)
library(ggsci)


save_figures <- function(plot, file, orientation = c("landscape", "portrait", "special"), dpi = 900){
    orientation <- match.arg(orientation)

    if(orientation == "portrait"){
        height <- 11
        width <- 8.5
    } else if(orientation=="special") {
      height <- 6
      width <- 11
    } else{
        height <- 8.5
        width <- 11
    }

    cowplot::ggsave2(filename = file, plot, height = height, width = width, dpi = dpi)

}

# Cases per capita
fig_us_cases_percapita <- readr::read_rds(here::here("output", "usa-capita.rds"))

fig_nc_cases_percapita <- readr::read_rds(here::here("output", "nc-capita.rds"))

# jw 2023/11/07: adding scale_x_date for 2 year breaks for us to match NC
fig_1 <- {fig_us_cases_percapita+scale_x_date(name = NULL, date_minor_breaks = '1 year',
                                             breaks = seq.Date(from = as.Date('2008-01-01'), to = as.Date('2022-01-01'), by = '2 years'), date_labels = '%Y')} /fig_nc_cases_percapita + plot_annotation(tag_levels = "A")

save_figures(fig_1, here::here('output', 'fig_1_update.png'),"landscape")
save_figures(fig_1, here::here('output', 'fig_1_submit.eps'),"landscape")
save_figures(fig_1, here::here('output', 'fig_1_submit.pdf'),"landscape")

# Models Fits
## OVerall
fig_us_fitted <- read_rds(here::here("output", "p_us_fit.rds"))
fig_us_smooths <- read_rds(here::here("output", "p_us_smooths.rds"))

## North Carolina

fig_nc_fitted <- read_rds(here::here("output", "p_nc_fit.rds"))
fig_nc_smooths <- read_rds(here::here("output", "p_nc_smooths.rds"))

add_lyme_theme <- list(
    theme_bw(base_size = 12),
    scale_x_continuous(breaks = seq(1,12), minor_breaks = NULL,
                       labels = month.abb,
                       name = NULL)
)

add_lyme_case_theme <- list(
    labs(x = NULL, y = "Total Cases"),
    scale_y_continuous(labels = scales::comma),
    theme_bw(base_size = 12)
)

fig_us_smooths+add_lyme_theme


fig_2 <- {fig_us_smooths+add_lyme_theme + fig_nc_smooths+add_lyme_theme + scale_y_continuous(n.breaks = 3)} / {fig_us_fitted+add_lyme_case_theme+scale_x_date(name = NULL, minor_breaks = NULL,
                                                                                                                                                              breaks = seq.Date(from = as.Date('2008-01-01'), to = as.Date('2022-01-01'), by = '2 years'), date_labels = '%Y') + fig_nc_fitted+add_lyme_case_theme} + plot_annotation(tag_levels = "A")

save_figures(fig_2, here::here('output', 'fig_s2_submit.png'),"landscape")
save_figures(fig_2, here::here('output', 'fig_s2_submit.pdf'),"landscape")
save_figures(fig_2, here::here('output', 'fig_s2_submit.eps'),"landscape")
save_figures(fig_2, here::here('output', 'fig_s2_submit.jpg'),"landscape")

# Geospatial


fig_3 <- read_rds(here::here("combined_geospatial_update.rds"))
save_figures(fig_3, here::here('output', 'fig_3_update.png'),"special")
save_figures(fig_3, here::here('output', 'fig_2_submit.eps'),"special")
save_figures(fig_3, here::here('output', 'fig_2_submit.pdf'),"special")
## Supplemental Figures


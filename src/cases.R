# Purpose: plotting of incidence of COVID-19 cases 

library(tidyverse)
library(data.table)
library(covidcast)



options(covidcast.auth = Sys.getenv("COVIDCAST"))


dat_nc <- covidcast_signal("jhu-csse","confirmed_7dav_incidence_num" , geo_type = "state",
                 geo_values = c("nc"))


dat_usa <- covidcast_signal("jhu-csse","confirmed_7dav_incidence_num" , geo_type = "state",
                 geo_values = c("*"))

setDT(dat_usa)
setDT(dat_nc)

dat_usa_red <- dat_usa[time_value>=min(dat_nc$time_value),list(USACaseCNT = sum(value)), by = "time_value"]


MMWR_BLUE <- "#0436a4"

fig_usa <- dat_usa_red[time_value < as.Date("2023-01-01")] |>
ggplot(aes(time_value, USACaseCNT/1000))+
geom_line(color = MMWR_BLUE)+
scale_y_continuous(expand = c(0,10))+
theme_bw()+
labs(y = "Reported Seven Day Incidence\nCOVID-19 Cases (1 000s)", x = NULL)+
scale_x_date(date_breaks = "4 month", date_labels = "%b %Y")

fig_nc <- dat_nc[time_value < as.Date("2023-01-01")] |>
ggplot(aes(time_value, value))+
geom_line(color = MMWR_BLUE)+
scale_y_continuous(expand = c(0,10))+
theme_bw()+
labs(y = "Reported Seven Day Incidence\nCOVID-19 Cases", x = NULL)+
scale_x_date(date_breaks = "4 month", date_labels = "%b %Y")

library(patchwork)

fig_cases <- fig_usa/ fig_nc + plot_annotation(tag_levels = "A")
library(here)
cowplot::ggsave2(filename = here("output", "figure-nc-usa-cases.png"), fig_cases, height = 8, width = 8, dpi = 1200)
cowplot::ggsave2(filename = here("output", "figure-nc-usa-cases.jpg"), fig_cases, height = 8, width = 8, dpi = 1200)
cowplot::ggsave2(filename = here("output", "figure-nc-usa-cases.eps"), fig_cases, height = 8, width = 8, dpi = 1200)

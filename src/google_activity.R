# Purpose: Trend populated weighted google mobility

library(tidyverse)
library(data.table)
library(here)

# Data available from google (https://www.google.com/covid19/mobility/)

dat <- fread(here("data", "nc_mobility.csv"))

pop <- fread(here("data-raw", "NCprojectionsbyagegrp2022.csv"))

nc_pop = pop[race== "Total" & sex == "Total" &county != "State", .(PopulationCNT = sum(total)), by = c("year", "fips")]


pop_all = nc_pop[year==2020]

# Verify that this is 10M

sum(pop_all$PopulationCNT, na.rm= TRUE)

MMWR_BLUE <- "#0436a4"

fig_1 <- dat[variable == "Parks"] |>
left_join(pop_all, by = c("FIPS" = "fips")) |>
group_by(date) |>
summarise(WeightedParks = weighted.mean(value, PopulationCNT)) |>
filter(date < as.Date("2022-01-01")) |>
mutate(WeightedParks = frollmean(WeightedParks,7)) |>
ggplot(aes(date, WeightedParks))+
geom_hline(yintercept = 0, lty = 2, color = "orange", linewidth =1.2)+
geom_line(linewidth = 1, color = MMWR_BLUE)+
theme_bw()+
labs(
    y = "Smoothed Population Weighted Mobility Score\n(Percent Above Average)",
    x = NULL
)+
scale_x_date(date_breaks = "3 month", date_labels = "%b %Y",
             #jw 2023-11-07 minor breaks on actual months
             date_minor_breaks = '1 month')

cowplot::ggsave2(filename = here("output", "figure-nc-mobility-update.jpg"), fig_1,
                 # jw 2023-11-07 height from 8 to 4
                 height = 4, width = 8, dpi = 1200)

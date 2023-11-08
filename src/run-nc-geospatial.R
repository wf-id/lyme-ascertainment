library(tidyverse)
library(data.table)
library(here)
library(sf)
library(mgcv)
library(gratia)
library(gtsummary)
library(flextable)


set.seed(1)

nc_counties <- st_as_sf(tigris::counties(state = "NC"))

nc_age <- fread(here("data-raw", "NCprojectionsbyagegrp2022.csv"))

nc_age[race== "Total" & sex == "Total" & county == "Alamance"]

# str(nc_age)

nc_age <- nc_age[race== "Total" & sex == "Total",list(PopulationCNT = sum(total)), by = c("fips", "county", "year")]


# Bring in raw data

target_files <- fs::dir_ls(here("data-raw"), regexp = "*-nc\\.csv")


dat_raw <- rbindlist(lapply(target_files, fread), idcol = "YearNBR")


names(dat_raw) <- c("YearNBR","CountyNM", "State", "CaseCNT", "DiseaseDSC", "IncidenceNBR", "CaseCNT2", "LatNBR", "LngNBR", "ViewDSC")


dat_raw[, YearNBR := as.numeric(stringr::str_extract(basename(YearNBR), "\\d+"))]

setorderv(dat_raw, c("YearNBR", "CountyNM"))

dat_raw_geo <- left_join(dat_raw,
                         nc_counties, by = c("CountyNM" = "NAME")) |>
                left_join(nc_age, by = c("CountyNM" = "county",
                "YearNBR" = "year")) |>
                st_as_sf() |>
                mutate(CasesPer100 = CaseCNT / (PopulationCNT/100000)) |>
                mutate(LogPopulationCNT = log(PopulationCNT)) |>
                mutate(LatScaleNBR = scale(LatNBR, scale = FALSE),
                LngScaleNBR = scale(LngNBR, scale = FALSE))

fit <- gam(CaseCNT ~ s(LngNBR,LatNBR) + YearNBR+offset(LogPopulationCNT),
           data = subset(dat_raw_geo, YearNBR < 2020), family = negbin(3))


fit |>
gtsummary::tbl_regression(exponentiate = TRUE) |>
as_flex_table() |>
format_flex_table() |>
flextable::save_as_docx(path = here::here('output', 'table-geo-regression.docx'))


p_sm <- plot(fit, scheme = 2)


nc_spatial_smooths <- draw(fit, ncol=1)+
theme_void()+
  colorspace::scale_fill_continuous_diverging(palette = 'Tropic',
                                               na.value = 'white') +
  scale_color_manual(values = 'white') +
labs(title = NULL, subtitle = NULL, caption = NULL)

new_dat <- subset(dat_raw_geo, YearNBR >= 2018)
new_dat$pred <- as.vector(predict(fit,new_dat, type = "response"))
new_dat$delta <- with(new_dat, CaseCNT - pred)
new_dat$delta100k <- with(new_dat, delta/ (PopulationCNT/100000))



pred_frame <- do.call(cbind, predict(fit,new_dat, type = "response", se = TRUE)) |>
as.data.table()

pred_frame[,PredLo := fit - 1.96*se.fit]
pred_frame[,PredHi := fit + 1.96*se.fit]

pred_frame <- cbind(data.table(YearNBR = new_dat$YearNBR,
            CountyNM = new_dat$CountyNM),
            CaseCNT = new_dat$CaseCNT,
      pred_frame)

pred_frame[,Delta := CaseCNT - fit]
pred_frame[,DeltaLo := CaseCNT - PredLo]
pred_frame[,DeltaHi := CaseCNT - PredHi]


plot_diff_2020 <- new_dat |>
filter(YearNBR>=2020) |>
ggplot(aes(fill = delta))+
geom_sf()+

  colorspace::scale_fill_continuous_diverging(palette = "Blue-Red 3")+
theme_void(base_size = 12)+
theme(strip.text = element_text(size = rel(2), color = "black",
                                hjust = 0, margin = margin(l=10)),

      legend.box.margin = margin(5,5,25,5),
      panel.spacing = unit(0, 'pt')
      )+
labs(fill = "Difference from\nPrediction (Cases)")+
facet_wrap(~YearNBR, ncol = 2)+
theme(legend.position = "right")

library(patchwork)

fig_combined_geospatial <- plot_diff_2020/ {nc_spatial_smooths} + plot_annotation(tag_levels = "A")

write_rds(fig_combined_geospatial, here::here("combined_geospatial_update.rds"))

annual_cases <- as.data.table(dat_raw_geo)[,list(Cases = sum(CaseCNT)), by = "YearNBR"]

annual_cases[]

library(gt)

pred_frame |>
filter(YearNBR>=2020) |>
mutate_if(is.numeric, round, digits = 1) |>
mutate(Estimate = sprintf("%s (%s to %s)",fit, PredLo, PredHi )) |>
mutate(Difference = sprintf("%s (%s to %s)",Delta, DeltaHi, DeltaLo )) |>
select(YearNBR, CountyNM, CaseCNT, Estimate, Difference) |>
mutate(YearNBR = factor(YearNBR, c("2020", "2021"))) |>
pivot_wider(names_from = YearNBR, values_from = c(CaseCNT, Estimate, Difference)) |>
select(CountyNM, CaseCNT_2020, Estimate_2020, Difference_2020,
               CaseCNT_2021, Estimate_2021, Difference_2021) |>
               gt::gt(rowname_col = "CountyNM") |>
cols_label(
    CaseCNT_2020 = "Reported Case",
    Estimate_2020 = "Estimated Cases (95% CI)",
    Difference_2020 = "Estimated Difference (95% CI)",
    CaseCNT_2021 = "Reported Cases",
    Estimate_2021 = "Estimated Cases (95% CI)",
    Difference_2021 = "Estimated Difference (95% CI)"
) |>
cols_align(align = "right", columns = everything()) |>
tab_spanner(
    label = "2020",
    columns = c(
      CaseCNT_2020, Estimate_2020, Difference_2020
    )
  ) |>
tab_spanner(
    label = "2021",
    columns = c(
      CaseCNT_2021, Estimate_2021, Difference_2021
    )
  ) |>
tab_footnote(
    footnote = "Lyme disease cases reported to the North Carolina Department of Health and Human Services",
    locations = cells_column_labels(c(CaseCNT_2021,CaseCNT_2020))
) |>
tab_footnote(
    footnote = "CI, 95% Confidence Interval",
    locations = cells_column_labels(c(Estimate_2020, Difference_2020,Estimate_2021, Difference_2021))
) |>
gt::gtsave(here::here("output", "nc-geospatial-results.docx"))

write_rds(dat_raw_geo, here("data", "nc-county-year.rds"))

cowplot::ggsave2(filename = here("output", "fig-nc-county-year.png"), width = 11, height = 8, dpi = 300)

cowplot::ggsave2(filename = here("output", "fig-nc-county-year.pdf"), width = 11, height = 8, dpi = 300)


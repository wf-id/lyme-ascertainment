library(tidyverse)
library(readxl)
library(fs)
library(here)
library(data.table)
library(mgcv)
library(brms)
set.seed(42)
options(brms.backend = "cmdstanr")
# Helper functions------------------------------------------

save_figure <- function(figure, fig_name){
    locs <- file.path(here::here("output"), sprintf("%s.%s", fig_name, c("png", "pdf")))

    cowplot::ggsave2(filename = locs[1], plot = figure, height = 8, width = 11, dpi = 900)
    cowplot::ggsave2(filename = locs[2], plot = figure, height = 8, width = 11, dpi = 900)
    cli::cli_alert("Done!\n")

}
 

qr <- function(x, digitz = 1){

  stopifnot(is.numeric(digitz))

  nsmall_use <- ifelse(digitz<0,0,digitz)

  stringr::str_trim(format(round(x, digitz), nsmall = nsmall_use))
}

# Bring in our data ------------------------------------------
dat_all_lyme <- read_rds(here("data", "nc-lyme-monthly.rds")) 

dat_all_lyme[,value := round(CaseCNT)]

nc_age <- fread(here("data-raw", "NCprojectionsbyagegrp2022.csv"))

nc_pop = nc_age[race== "Total" & sex == "Total" &county != "State", .(PopulationCNT = sum(total)), by = c("year", "county")][
    ,.(Total = sum(PopulationCNT)), by = "year"
][
    ,Poplog := log(Total)
][]

dat_all_lyme <- left_join(dat_all_lyme, nc_pop, by = c("YearNBR" = "year"))


fig_nc_cases_percapita <-dat_all_lyme |>
ggplot(aes(DateDT, value/(Total/100000)))+
geom_line()+
labs(y = "Cases Reported per 100,000", x = NULL)+
theme_classic(base_size = 14)+
scale_y_continuous(limits = c(0,NA), breaks = seq(0,3,.25), expand = c(0,.2))

readr::write_rds(fig_nc_cases_percapita, here::here("output", "nc-capita.rds"))


fit <- brm(value ~ s(MonthNBR, bs = "cc", k = 12) + offset(Poplog),
           data = dat_all_lyme[YearNBR<2020],
           family = poisson(),
           backend = "cmdstanr",
           save_model = here("output", "ncmodel"))
fitgp <- brm(bf(value ~ s(MonthNBR, bs = "cc", k = 12) + offset(Poplog)),
           data = dat_all_lyme[YearNBR<2020],
           family = negbinomial,
           backend = "cmdstanr",
           save_model = here("output", "ncmodel"))

summary(fit)
summary(fitgp)
loo::loo(fit, fitgp, moment_match = TRUE)

plot(conditional_smooths(fitgp), ask = FALSE)


p_nc_smooths <- plot(conditional_smooths(fitgp), ask = FALSE)$`mu: s(MonthNBR,bs="cc",k=12)` + 
scale_x_continuous(name = NULL, breaks = c(1,4,8,12), labels = c("January", "April", "August", "December"))+
labs(y = "Smooth")

write_rds(p_nc_smooths, here::here("output", "p_nc_smooths.rds"))

save_figure(p_nc_smooths, "figure-nc-cases-smooths")


post_samps = posterior_epred(fitgp, dat_all_lyme)

str(post_samps)

dat_underascertained <- copy(dat_all_lyme)

dat_underascertained$MedianEstimate <- matrixStats::colMedians(post_samps)
dat_underascertained$CrILo <- matrixStats::colQuantiles(post_samps, probs = 0.025)
dat_underascertained$CrIHi <- matrixStats::colQuantiles(post_samps, probs = 0.975)


for(i in 1:nrow(dat_underascertained)){

    raw <- dat_underascertained$value[i]/(post_samps[,i])

    dat_underascertained$Underasertainment[i] <- median(raw)
    dat_underascertained$UnderasertainmentLo[i] <- quantile(raw, probs = 0.025)
    dat_underascertained$UnderasertainmentHi[i] <- median(raw, probs = 0.975)
}

dat_underascertained |>
ggplot(aes(MonthNBR, Underasertainment, color = factor(YearNBR)))+
geom_line()+
theme_minimal()->a

a

cowplot::ggsave2("test.png", a, bg = "white")

p_nc_fit <- dat_underascertained |>
ggplot(aes(DateDT))+
geom_point(aes(y = value), color = 'orange', size = 2)+
theme_bw()+
#geom_line(aes(y = MedianEstimate), color = "blue")+
geom_ribbon(aes(ymin = CrILo, ymax= CrIHi), color ="blue", alpha = .2)

write_rds(p_nc_fit, here::here("output", "p_nc_fit.rds"))

sum(dat_underascertained[Date>=as.Date("2020-03-01")]$MedianEstimate) - sum(dat_underascertained[Date>=as.Date("2020-03-01")]$value) 


dat_underascertained |>
mutate(
    Estimate = sprintf("%s (%s to %s)", qr(MedianEstimate),qr(CrILo),qr(CrIHi)),
    Ascertainment = sprintf("%s (%s to %s)", qr(Underasertainment,2),qr(UnderasertainmentLo,2),qr(UnderasertainmentHi,2))
) |>
select(YearNBR, Month = MonthDSC, Estimate,Ascertainment) |>
flextable::flextable() |>
idstyle::format_flex_table() |>
flextable::save_as_docx(path = here::here("output", "table-nc-estimates.docx"))

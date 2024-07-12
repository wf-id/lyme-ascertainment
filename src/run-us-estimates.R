library(tidyverse)
library(readxl)
library(fs)
library(here)
library(data.table)
library(mgcv)
library(brms)

set.seed(42)

save_figure <- function(figure, fig_name){
    locs <- file.path(here::here("output"), sprintf("%s.%s", fig_name, c("png", "pdf")))

    cowplot::ggsave2(filename = locs[1], plot = figure, height = 8, width = 11, dpi = 900)
    cowplot::ggsave2(filename = locs[2], plot = figure, height = 8, width = 11, dpi = 900)
    cli::cli_alert("Done!\n")

}

# Bring in raw data and reformat for analysis

dat_us <- fread(here("data-raw","Lyme_Disease_Cases_by_Month_of_Disease_Onset_United_States.csv"))

dat_us <- dat_us[Year!= "Total"]

dat_us_long <- melt(dat_us, id.vars = "Year")


dat_us_long[,value := as.numeric(stringr::str_remove(value, ","))]
dat_us_long[,MonthDT := lubridate::mdy(paste0(variable," 01 ", Year))]
dat_us_long <- dat_us_long[order(MonthDT)]
dat_us_long[,YearNBR := lubridate::year(MonthDT)]
dat_us_long[,MonthNBR := lubridate::month(MonthDT)]

us_pop <- fread(here("data", "us-unpopulation.csv"))

us_pop = us_pop[,YearNBR := lubridate::year(Date)][YearNBR <= 2023][,list(Date, Population, YearNBR)]

# Fit our model

fig_usa_cases <- dat_us_long |>
ggplot(aes(MonthDT, value))+
geom_line()+
labs(y = "Total Cases Reported", x = NULL)+
theme_classic(base_size = 14)+
scale_y_continuous(limits = c(0,NA), labels = scales::comma, expand = c(0,100))


dat_us_long <- left_join(dat_us_long, us_pop, by = "YearNBR")

dat_us_long <- as.data.table(dat_us_long)[,PopLog := log(Population)]

fig_usa_cases_percapita <-dat_us_long |>
ggplot(aes(MonthDT, value/(Population/100000)))+
geom_line()+
labs(y = "Cases Reported per 100,000", x = NULL)+
theme_classic(base_size = 14)+
scale_y_continuous(limits = c(0,2.75), breaks = seq(0,3,.25), expand = c(0,.2))

readr::write_rds(fig_usa_cases_percapita, here::here("output", "usa-capita.rds"))

save_figure(fig_usa_cases_percapita, "figure-us-cases-percapita")

# Tables for Summary ------------------------------------------

dat_us_long[,.(CaseCNT = sum(value), Population = max(Population)), by = c("Year")][
    ,CasesPCT := CaseCNT / (Population/ 100000)
] |>
select(-Population) |>
setNames(c("Years", "Cases", "Cases per 100k")) |>
flextable::flextable() |>
idstyle::format_flex_table() |>
flextable::save_as_docx(path = here("output", "table-usa-case-info.docx"))

dat_us_long[,CaseDefinitionDSC := case_when(YearNBR < 2011 ~ "2008",
                                            YearNBR < 2017~ "2008",
                                            YearNBR < 2022~ "2017")]
# Fit models ------------------------------------------

prior1 <- prior(normal(0,1), class = sd, group = "CaseDefinitionDSC")

get_prior(bf(value ~ -1 +s(MonthNBR, bs = "cc", k = 12) + (1|CaseDefinitionDSC) + offset(PopLog)),
           data = dat_us_long[YearNBR<2020])

fit <- brm(value ~ -1 + s(MonthNBR, bs = "cc", k = 12) + (1|CaseDefinitionDSC) + offset(PopLog),
           data = dat_us_long[YearNBR<2020],
           family = poisson(), prior = prior1,
           backend = "cmdstanr",
           save_model = here("output", "usamodel"),
           iter = 4000,
           control  = list(max_treedepth = 15, adapt_delta = .99))
fit

fitnb <- brm(value ~ -1 + s(MonthNBR, bs = "cc", k = 12) + (1|CaseDefinitionDSC)+ offset(PopLog),
           data = dat_us_long[YearNBR<2020],
           family = negbinomial, prior = prior1,
           backend = "cmdstanr",
           save_model = here("output", "usamodel"), 
           control  = list(max_treedepth = 13))
fitnb

# Save models ------------------------------------------

readr::write_rds(fit, here::here("output", "model-usa-poisson.rds"))
readr::write_rds(fitnb, here::here("output", "model-usa-negbinom.rds"))

# Determine which model is superior ------------------------------------------
loo::loo(fit, fitnb)
as.data.frame()

usa_loo_results <- loo::loo(fit, fitnb)

usa_loo_results$diffs |>
as.data.frame()  |>
rownames_to_column() |>
#setNames(c("Model","ELPD Difference", "Standard Error of Difference", "LOO", "LOO (SE)")) |>
flextable::flextable()
flextable::save_as_docx(path = "usa-loo-fit.docx")

readr::write_rds(usa_loo_results, here::here("output", "loo-usa-results.rds"))

# Examine outputs ------------------------------------------
o <- conditional_smooths(fitnb)

p_marge <- plot(o, theme = theme_bw(base_size = 14))

p_usa_smooths <- p_marge$`mu: s(MonthNBR,bs="cc",k=12)` + 
scale_x_continuous(name = NULL, breaks = c(1,4,8,12), labels = c("January", "April", "August", "December"))+
labs(y = "Smooth")

write_rds(p_usa_smooths, here::here("output", "p_us_smooths.rds"))

save_figure(p_usa_smooths, "figure-us-cases-smooths")



summary(fitnb) |>
gtsummary::tbl_regression()

post_samps = posterior_epred(fitnb, dat_us_long)

str(post_samps)

dat_underascertained <- copy(dat_us_long)

dat_underascertained$MedianEstimate <- matrixStats::colMedians(post_samps)
dat_underascertained$CrILo <- matrixStats::colQuantiles(post_samps, probs = 0.025)
dat_underascertained$CrIHi <- matrixStats::colQuantiles(post_samps, probs = 0.975)


for(i in 1:nrow(dat_underascertained)){

    raw <- dat_underascertained$value[i]/(post_samps[,i])

    dat_underascertained$Underasertainment[i] <- median(raw)
    dat_underascertained$UnderasertainmentLo[i] <- quantile(raw, probs = 0.025)
    dat_underascertained$UnderasertainmentHi[i] <- median(raw, probs = 0.975)
}

fig_usa_fitted <- dat_underascertained |>
ggplot(aes(MonthDT))+
geom_point(aes(y = value), color = "orange", size = 2)+
theme_bw()+
#geom_line(aes(y = MedianEstimate), color = "orange")+
geom_ribbon(aes(ymin = CrILo, ymax= CrIHi), color ="blue", alpha = .2)

write_rds(fig_usa_fitted, here::here("output", "p_us_fit.rds"))


save_figure(fig_usa_fitted, "figure-us-cases-predicted")
qr <- function(x, digitz = 1){

  stopifnot(is.numeric(digitz))

  nsmall_use <- ifelse(digitz<0,0,digitz)

  stringr::str_trim(format(round(x, digitz), nsmall = nsmall_use))
}

dat_underascertained |>
mutate(
    Estimate = sprintf("%s (%s to %s)", qr(MedianEstimate),qr(CrILo),qr(CrIHi)),
    Ascertainment = sprintf("%s (%s to %s)", qr(Underasertainment,2),qr(UnderasertainmentLo,2),qr(UnderasertainmentHi,2))
) |>
select(Year, Month = variable, Estimate,Ascertainment) |>
flextable::flextable() |>
idstyle::format_flex_table() |>
flextable::save_as_docx(path = here::here("output", "table-us-estimates.docx"))


dat_underascertained |>
ggplot(aes(MonthDT))+
theme_bw()+
geom_line(aes(y = Underasertainment), color = "firebrick")+
geom_ribbon(aes(ymin = UnderasertainmentLo, ymax= UnderasertainmentHi), fill ="firebrick", alpha = .2)+
geom_hline(yintercept = 1)

sum(dat_underascertained[Date>=as.Date("2020-03-01")]$MedianEstimate) - sum(dat_underascertained[Date>=as.Date("2020-03-01")]$value) 


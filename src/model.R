rm(list = ls())

library("data.table")
library("texreg")
library("plm")
library("sandwich")
library("lmtest")
library("ggplot2")
library("mgcv")

data.table::setDTthreads(2)

source("fun.R")

deaths = data.table::fread("../dat/deaths.csv", na.strings = "")
municipalities = data.table::fread("../dat/spatialagg.txt")
coverage = data.table::fread("../dat/coverage.csv")

# subset dataset
deaths = deaths[sep_dec == TRUE]
deaths = merge(
    x = deaths,
    y = municipalities[Sampled == "Sampled", list(amco = ACODE, corop = Corop, egg = EGG, province = Provincie)],
    by = "amco",
    all.x = FALSE, all.y = FALSE)
deaths = merge(
    x = deaths,
    y = coverage[drop == FALSE],
    by = "amco",
    all.x = FALSE, all.y = FALSE)

aggvrbs = c("year", "event_month", "agegroup", "pr_gender", "skill_level", "final_under_roof", "final_meet_strangers")

# 10 year age bins, municipalities
agebin = 10
deaths[, agegroup := floor(pr_age / agebin) * agebin]
deaths[!is.na(HISCO), farmer := HISCO %in% c(61220)] # also 62210, 62105 ?

# 10 year age bins, egg regions
excess_egg10 = excess(deaths,
    aggvrbs = c("egg", "farmer", aggvrbs))

excess_egg10[, list(nflu = sum(flu), nbase = sum(nbaseline), cells = .N), by = skill_level]

modlist_base = list(
    `skill` = lm(log1p(emr) ~ skill_level, 
        data = excess_egg10),
    `farmer` = lm(log1p(emr) ~ skill_level + farmer, 
        data = excess_egg10),
    `exposure` = lm(log1p(emr) ~ farmer + final_under_roof*final_meet_strangers, 
        data = excess_egg10),
    `both` = lm(log1p(emr) ~ skill_level + farmer + final_under_roof*final_meet_strangers, 
        data = excess_egg10),
    `+ age` = lm(log1p(emr) ~ skill_level + farmer + final_under_roof*final_meet_strangers + factor(agegroup), 
        data = excess_egg10),
    `+ sex` = lm(log1p(emr) ~ skill_level + farmer + final_under_roof*final_meet_strangers + factor(agegroup) + pr_gender, 
        data = excess_egg10),
    `+ month` = lm(log1p(emr) ~ skill_level + farmer + final_under_roof*final_meet_strangers + factor(agegroup) + pr_gender + factor(event_month), 
        data = excess_egg10),
    `+ region FE` = lm(log1p(emr) ~ skill_level + farmer + final_under_roof*final_meet_strangers + factor(agegroup) + pr_gender + factor(event_month) + factor(egg), 
        data = excess_egg10))
prefmod = modlist_base$`+ region FE`
prefform = formula(prefmod)

# map to reorder coefficients
# varnames = texreg::extract(modlist_base[["+ region FE"]])@coef.names
# cat(varnames[!grepl("egg", varnames)], sep = "\n")
coefmap = list(
    "skill_levelmedium_skilled" = "medium_skilled",
    "skill_levellower_skilled" = "lower_skilled",
    "skill_levelunskilled" = "unskilled",

    "farmerTRUE" = "farmer",

    "I(HISCAM_NL/100)" = "hiscam",
    "HISCAM_NL" = "hiscam",
    "EDF: s(HISCAM_NL)" = "EDF: s(HISCAM_NL)",

    "final_under_roof" = "indoor",
    "final_meet_strangers" = "strangers",
    "final_under_roof:final_meet_strangers" = "indoor and strangers",

    "final_under_roof:skill_levelmedium_skilled" = "indoor and medium skilled",
    "final_under_roof:skill_levellower_skilled" = "indoor and lower skilled",
    "final_under_roof:skill_levelunskilled" = "indoor and unskilled",
    "final_meet_strangers:skill_levelmedium_skilled" = "strangers and medium skilled",
    "final_meet_strangers:skill_levellower_skilled" = "strangers and lower skilled",
    "final_meet_strangers:skill_levelunskilled" = "strangers and unskilled",

    "HISCAM_NL:final_meet_strangers" = "hiscam x strangers",
    "HISCAM_NL:final_under_roof" = "hiscam x indoor",
    
    "factor(agegroup)20" = "agegroup 20",
    "factor(agegroup)30" = "agegroup 30",
    "factor(agegroup)40" = "agegroup 40",
    "factor(agegroup)50" = "agegroup 50",
    "factor(agegroup)60" = "agegroup 60",
    "factor(agegroup)70" = "agegroup 70",
    
    "pr_genderm" = "male",
    
    "factor(event_month)10" = "event_month10",
    "factor(event_month)11" = "event_month11",
    "factor(event_month)12" = "event_month12",
    
    "(Intercept)" = "(Intercept)"
)

coeflist = lapply(modlist_base, coeftest, vcov. = sandwich::vcovCL, cluster = ~ egg)
texreg::screenreg(modlist_base, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4))
texreg::texreg(modlist_base, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Regression models of log excess mortality rate. Region-clustered standard errors between parentheses.",
    label = "tab:basemodels",
    file = "../out/models_base.tex")

# models with dropped/recoded observations
excess_egg10_nofarmers = excess(
    deaths[HISCO != 61220 | is.na(HISCO)], # this should not drop NA occupations for comparability? Surefly these are dropped in the regression anyway?
    aggvrbs = c("egg", aggvrbs))
deaths_farmrecoded = copy(deaths)
deaths_farmrecoded[HISCO == 61220, skill_level := "lower_skilled"]
excess_egg10_farmrecoded = excess(
    deaths_farmrecoded,
    aggvrbs = c("egg", aggvrbs))

modlist_altocc = list(
    `all occupations` = prefmod,
    `no farmers` = update(prefmod, . ~ . - farmer, data = excess_egg10_nofarmers),
    `farmers recoded` = update(prefmod, . ~ . - farmer, data = excess_egg10_farmrecoded))

coeflist = lapply(modlist_altocc, coeftest, vcov. = sandwich::vcovCL, cluster = ~ egg)
texreg::screenreg(modlist_altocc, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4))
texreg::texreg(modlist_altocc, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Regression models of log excess mortality rate, exluding selected occupations. Region-clustered standard errors between parentheses.",
    label = "tab:altoccmodels",
    file = "../out/models_altocc.tex")

# hiscam
excess_egg10_hiscam = excess(deaths,
    aggvrbs = c("egg", "farmer", "HISCAM_NL", aggvrbs[aggvrbs != "skill_level"]))
modlist_hiscam = list(
    `hisclass` = prefmod,
    `hiscam` = update(prefmod, 
        . ~ . - skill_level + I(HISCAM_NL / 100),
        data = excess_egg10_hiscam[HISCAM_NL > 0]),
    `hiscam spline` = mgcv::gam(update(formula(prefmod), . ~ . - skill_level + s(HISCAM_NL, k = 5)),
            data = excess_egg10_hiscam[HISCAM_NL > 0])
    )

coeflist = lapply(modlist_hiscam[1:2], coeftest, vcov. = sandwich::vcovCL, cluster = ~ egg)
texreg::screenreg(modlist_hiscam[1:2], 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4))
texreg::texreg(modlist_hiscam[1:2], 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Regression models of log excess mortality rate using occuaptional status (HISCAM) rather than skill (HISCLASS-based), exluding selected occupations. Region-clustered standard errors between parentheses.",
    label = "tab:hiscammodels",
    file = "../out/models_hiscam.tex")

pdf("../out/hiscamspline.pdf")
plot(modlist_hiscam$`hiscam spline`, col = 2, lwd = 1.5)
dev.off()

# interactions
modlist_inter = list(
    `no interactions` = prefmod,
    `strangers interactions` = update(prefmod,
        . ~ . - skill_level - final_under_roof - final_under_roof:final_meet_strangers + 
        final_meet_strangers*skill_level),
    `indoors interactions` = update(prefmod,
        . ~ . - skill_level - final_meet_strangers - final_under_roof:final_meet_strangers + 
        final_under_roof*skill_level),
    `hiscam interactions` = update(prefmod,
        . ~ . - skill_level - final_meet_strangers - final_under_roof - final_under_roof:final_meet_strangers + 
        HISCAM_NL*final_meet_strangers + HISCAM_NL*final_under_roof,
        data = excess_egg10_hiscam)
    )

coeflist = lapply(modlist_inter, coeftest, vcov. = sandwich::vcovCL, cluster = ~ egg)
texreg::screenreg(modlist_inter, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4))
texreg::texreg(modlist_inter, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Regression models of log excess mortality rate with interactions, exluding selected occupations. Region-clustered standard errors between parentheses.",
    label = "tab:intermodels",
    file = "../out/models_inter.tex")

# check model outcomes at different aggregation levels
# 10 year age bins, municipalities
excess_amco10 = excess(deaths,
    aggvrbs = c("amco", "farmer", aggvrbs))

# 10 year age bins, corop regions
excess_corop10 = excess(deaths,
    aggvrbs = c("corop", "farmer", aggvrbs))

# 10 year age bins, provinces
excess_province10 = excess(deaths,
    aggvrbs = c("province", "farmer", aggvrbs))

# rename to region to refer to same clustering variable
excess_amco10[, region := amco]
excess_egg10[, region := egg]
excess_corop10[, region := corop]
excess_province10[, region := province]

modlist_regions = list(
    `municipalities` = lm(prefform, data = excess_amco10),
    `*EGG*` = lm(prefform, data = excess_egg10),
    `COROP` = lm(prefform, data = excess_corop10),
    `Province` = lm(prefform, data = excess_province10)
)
coeflist = lapply(modlist_regions, coeftest, vcov. = sandwich::vcovCL, cluster = ~ region)

texreg::texreg(modlist_regions, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Regression models of log excess mortality rate at different levels of aggregation. Region-clustered standard errors between parentheses.",
    label = "tab:regionmodels",
    file = "../out/models_regions.tex")


# models for high mortality regions only
# or maybe do this on amco level first?
deaths[data.table::between(pr_age, 10, 79), agegroup := 40]
excess_egg = excess(deaths,
    aggvrbs = c("egg", "year", "agegroup"))
hist(excess_egg$emr, breaks = 20)
plot(ecdf(excess_egg$emr))
quantile(excess_egg$emr, 1:9 / 10)
mean(excess_egg$emr)


excess_amco = excess(deaths,
    aggvrbs = c("amco", "year", "agegroup"))
hist(excess_amco$emr, breaks = 20)
plot(ecdf(excess_amco$emr))
quantile(excess_amco$emr, 1:9 / 10)
mean(excess_amco$emr)

# 2-3 are very normal, med = 2.6ish, mean = 3
# so let's go higher than 2.5 = high

excess_egg10 = merge(
    excess_egg10,
    excess_egg[, list(egg, eggemr = emr)], 
    by = "egg",
    all.x = TRUE)

modlist_high = list(
    `low EM` = lm(prefform, data = excess_egg10[eggemr <= 2.5]),
    `high EM` = lm(prefform, data = excess_egg10[eggemr > 2.5]))

coeflist = lapply(modlist_high, coeftest, vcov. = sandwich::vcovCL, cluster = ~ egg)

texreg::texreg(modlist_high, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Regression models of log excess mortality rate for low and high excess mortality regions. Region-clustered standard errors between parentheses.",
    label = "tab:hilomodels",
    file = "../out/models_hilo.tex")

# alt zero handling
screenreg(
    list(
        prefmod,
        lm(update.formula(prefform, log(emr) ~ .), data = excess_egg10[emr > 0]),
        glm(update.formula(prefform, emr ~ .), data = excess_egg10, family = quasipoisson)
    ),
    omit.coef = "egg"
)

# example data set
out = na.omit(excess_egg10[order(-emr)])
out = out[, list(EGG = egg, 
    indoor = final_under_roof,
    strangers = final_meet_strangers,
    sex = pr_gender,
    month = event_month, agegroup, baseline, flu, emr)]
set.seed(232)
writeLines(
    knitr::kable(out[sample(.N, 5)], 
        format = "latex", 
        lab = "tab:example",
        caption = "Example excess mortality dataset",
        digits = 2),
    con = "../out/data_example.tex")

# check correlations
m = model.matrix(log1p(emr) ~ factor(agegroup) + pr_gender + factor(event_month) + skill_level + final_under_roof + final_meet_strangers, data = excess_egg10)
# as.data.table(m)[, cor(.SD, .SD), .SDcols = -1]
cm = round(cor(m[-1, -1]), digits = 2)
cm = data.table(row = rep(rownames(cm), ncol(cm)),
    col = rep(colnames(cm), each = nrow(cm)),
    value = c(cm))
pdf("../out/cormat.pdf")
ggplot(cm, aes(row, col, fill = value)) + geom_tile() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_fill_viridis_c()
dev.off()
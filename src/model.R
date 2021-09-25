rm(list = ls())

library("data.table")
library("texreg")
library("sandwich")
library("lmtest")
library("ggplot2")
library("mgcv")

data.table::setDTthreads(2)

source("fun.R")
source("coefmap.R")

deaths = data.table::fread("../dat/deaths_subset.csv", na.strings = "")
municipalities = data.table::fread("../dat/spatialagg.txt")
popdens = data.table::fread("../dat/inwonertal + bevolkingsdichtheid.txt")
army = data.table::fread("../dat/legerplaatsen.csv", na.strings = "")

popdens[, popdens1918 := PopSize1918 / hectare]
popdens_amco = merge(popdens, municipalities, by = "ACODE")
popdens_egg = popdens_amco[, 
    list(pop1918 = sum(PopSize1918), popdens1918 = sum(PopSize1918) / sum(hectare)), 
    by = list(egg = EGG)]
popdens_corop = popdens_amco[, 
    list(pop1918 = sum(PopSize1918), popdens1918 = sum(PopSize1918) / sum(hectare)), 
    by = Corop]
popdens_prov = popdens_amco[, 
    list(pop1918 = sum(PopSize1918), popdens1918 = sum(PopSize1918) / sum(hectare)), 
    by = Provincie]

aggvrbs = c("year", "event_month", "agegroup", "pr_gender", "skill_level", "exposure")
deaths[, agegroup := cut(pr_age, c(11, 30, 45, 60, 80))]

# agebin = 10
# deaths[, agegroup := floor(pr_age / agebin) * agebin]
deaths[!is.na(HISCO), farmer := HISCO %in% c(61220)] # also 62210, 62105 ?

deaths[, skill_level := relevel(factor(skill_level), ref = "higher_skilled")]

# 10 year age bins, egg regions
excess_egg = excess(deaths,
    aggvrbs = c("egg", "farmer", aggvrbs))
excess_egg[!is.na(skill_level), list(sum(nflu + nbaseline))] # this N is correct, also dropping 1914 and baselin = 0

# % of emr == 0 observations
excess_egg[, sum(emr == 0) / .N]

modlist_base = list(
    `skill` = lm(log1p(emr) ~ skill_level, 
        data = excess_egg),
    `farmer` = lm(log1p(emr) ~ skill_level + farmer, 
        data = excess_egg),
    `exposure` = lm(log1p(emr) ~ farmer + exposure, 
        data = excess_egg),
    `both` = lm(log1p(emr) ~ skill_level + farmer + exposure, 
        data = excess_egg),
    `+ demog.` = lm(log1p(emr) ~ skill_level + farmer + exposure + agegroup + pr_gender, 
        data = excess_egg),
    `+ month FE` = lm(log1p(emr) ~ skill_level + farmer + exposure + agegroup + pr_gender + factor(event_month), 
        data = excess_egg),
    `+ region FE` = lm(log1p(emr) ~ skill_level + farmer + exposure + agegroup + pr_gender + factor(event_month) + factor(egg), 
        data = excess_egg))
prefmod = modlist_base$`+ region FE`
prefform = formula(prefmod)

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

# models with hiscam and dropped/recoded observations for farmers
excess_egg_hiscam = excess(deaths,
    aggvrbs = c("egg", "farmer", "hiscam", aggvrbs[aggvrbs != "skill_level"]))
excess_egg_nofarmers = excess(
    deaths[HISCO != 61220 | is.na(HISCO)], # this should not drop NA occupations for comparability? These are dropped in the regression anyway
    aggvrbs = c("egg", aggvrbs))
deaths_farmrecoded = copy(deaths)
deaths_farmrecoded[HISCO == 61220, skill_level := "lower_skilled"]
excess_egg_farmrecoded = excess(
    deaths_farmrecoded,
    aggvrbs = c("egg", aggvrbs))

modlist_altocc = list(
    `all occupations` = prefmod,
    `no farmers` = update(prefmod, . ~ . - farmer, data = excess_egg_nofarmers),
    `farmers recoded` = update(prefmod, . ~ . - farmer, data = excess_egg_farmrecoded),
    `hiscam` = update(prefmod, . ~ . - skill_level + hiscam, data = excess_egg_hiscam[hiscam > 0])
)
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

m_hiscam_spline = mgcv::gam(update(formula(prefmod), . ~ . - skill_level + s(hiscam, k = 5)),
            data = excess_egg_hiscam[hiscam > 0])

pdf("../out/hiscamspline.pdf")
mypar()
plot(m_hiscam_spline, col = 2, lwd = 1.5)
abline(h = 0, col = "gray")
dev.off()

# check model outcomes at different aggregation levels
# municipalities
excess_amco = excess(deaths,
    aggvrbs = c("amco", "farmer", aggvrbs))

# corop regions
excess_corop = excess(deaths,
    aggvrbs = c("corop", "farmer", aggvrbs))

# provinces
excess_prov = excess(deaths,
    aggvrbs = c("prov", "farmer", aggvrbs))

# rename to region to refer to same clustering variable
excess_amco[, region := amco]
excess_egg[, region := egg]
excess_corop[, region := corop]
excess_prov[, region := prov]

regform = update(prefform, . ~ . - factor(egg) + factor(region))
modlist_regions = list(
    `municipalities` = lm(regform, data = excess_amco),
    `*EGG*` = lm(regform, data = excess_egg),
    `COROP` = lm(regform, data = excess_corop),
    `Province` = lm(regform, data = excess_prov)
)
coeflist = lapply(modlist_regions, coeftest, vcov. = sandwich::vcovCL, cluster = ~ region)
texreg::screenreg(modlist_regions, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4))
texreg::texreg(modlist_regions, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Regression models of log excess mortality rate at different levels of aggregation. Region-clustered standard errors between parentheses.",
    label = "tab:regionmodels",
    file = "../out/models_regions.tex")

# population density
excess_amco = popdens_amco[excess_amco, on = c(ACODE = "amco")]
setnames(excess_amco, "PopSize1918", "pop1918")
excess_egg = popdens_egg[excess_egg, on = c(egg = "egg")]
excess_corop = popdens_corop[excess_corop, on = c(Corop = "corop")]
excess_prov = popdens_prov[excess_prov, on = c(Provincie = "prov")]

popform = update(prefform, . ~ . - factor(egg) + log(pop1918) - factor(region))
modlist_popdens = list(
    `municipalities` = lm(popform, data = excess_amco),
    `*EGG*` = lm(popform, data = excess_egg),
    `COROP` = lm(popform, data = excess_corop),
    `Province` = lm(popform, data = excess_prov)
)
coeflist = lapply(modlist_popdens, coeftest, vcov. = sandwich::vcovCL, cluster = ~ region)

screenreg(modlist_popdens)
texreg::texreg(modlist_popdens, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Regression models of log excess mortality rate including population density controls and no region FE. Region-clustered standard errors between parentheses.",
    label = "tab:popdensmodels",
    file = "../out/models_popdens.tex")

# army bases
excess_amco = army[excess_amco, on = c("amco" = "ACODE")]
excess_amco[, base1918 := !is.na(dataset2)]
excess_amco[, armyhosp1913 := !is.na(dataset1)]
modlist_army = list(
    `municipalities` = lm(popform, data = excess_amco),
    `base` = lm(update(popform, . ~ . + base1918), data = excess_amco),
    `hosp` = lm(update(popform, . ~ . + armyhosp1913), data = excess_amco),
    `both` = lm(update(popform, . ~ . + armyhosp1913 + base1918), data = excess_amco)
    # update(modlist_popdens$municipalities, . ~ . + armyhosp1913 + base1918, data = excess_amco_wboth)
)
coeflist = lapply(modlist_army, coeftest, vcov. = sandwich::vcovCL, cluster = ~ region)
texreg::texreg(modlist_army, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Regression models of log excess mortality rate including army base and hospital controls. Region-clustered standard errors between parentheses.",
    label = "tab:popdensmodels",
    file = "../out/models_army.tex")

# models for high mortality regions only
# first, ID high mort regions by EMR egg only
excess_egg_only = excess(deaths, aggvrbs = c("egg", "year"))
excess_amco_only = excess(deaths, aggvrbs = c("amco", "year"))
summary(excess_egg_only$emr)
summary(excess_amco_only$emr)

# emr 2-3 are very normal, med = 2.6ish, mean = 3
# so let's go higher than 2.5 = high
excess_egg = merge(
    excess_egg,
    excess_egg_only[, list(egg, eggemr = emr)], 
    by = "egg",
    all.x = TRUE)

modlist_hilo = list(
    prefmod,
    `low EM` = lm(prefform, data = excess_egg[eggemr <= 2.5]),
    `high EM` = lm(prefform, data = excess_egg[eggemr > 2.5]))

coeflist = lapply(modlist_hilo, coeftest, vcov. = sandwich::vcovCL, cluster = ~ egg)

# texreg::screenreg(modlist_hilo, omit.coef = "egg")
texreg::screenreg(modlist_hilo, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Regression models of log excess mortality rate for low and high excess mortality regions. Region-clustered standard errors between parentheses.",
    label = "tab:hilomodels",
    file = "../out/models_hilo.tex")

# alt zero handling
modlist_zeroes = list(
    `log x + 1` = prefmod,
    `drop 0s` = lm(update.formula(prefform, log(emr) ~ .), data = excess_egg[emr > 0]),
    `no log` = lm(update.formula(prefform, emr ~ .), data = excess_egg),
    `norm. emr` = lm(update.formula(prefform, emrr ~ .), data = excess_egg),
    `q-poiss EMR` = glm(update.formula(prefform, emr ~ .), data = excess_egg, family = quasipoisson),
    `q-poiss N ` = glm(update.formula(prefform, nflu ~ .), data = excess_egg, family = quasipoisson, offset = log(baseline)),
    `asinh` = lm(update.formula(prefform, asinh(emr) ~ .), data = excess_egg)
    # `tobit` = censReg::censReg(update.formula(prefform, emr ~ . ), data = excess_egg)
    # `tobit x` = AER::tobit(update.formula(prefform, emr ~ . ), data = excess_egg)
)
coeflist = lapply(modlist_zeroes, coeftest, vcov. = sandwich::vcovCL, cluster = ~ egg)
texreg::screenreg(modlist_zeroes, 
    custom.coef.map = coefmap)
texreg::texreg(modlist_zeroes, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    caption = "Alternative model forms for regressions of log excess mortality rate. Region-clustered standard errors between parentheses.",
    label = "tab:altmodels",
    file = "../out/models_altform.tex")

# example data set
set.seed(11)
out = na.omit(excess_egg[pr_gender == "m"])
out = out[, list(EGG = egg, 
    month = event_month, 
    agegroup, 
    skill_level = skill_level,
    exposure = exposure,
    baseline, 
    flu, 
    emr)]
writeLines(
    knitr::kable(out[sample(.N, 5)], 
        format = "latex", 
        lab = "tab:example",
        caption = "Example excess mortality dataset",
        digits = 2),
    con = "../out/data_example.tex")

# check correlations
m = model.matrix(log1p(emr) ~ factor(agegroup) + pr_gender + factor(event_month) + skill_level + exposure, data = excess_egg)
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

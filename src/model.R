rm(list = ls())

library("data.table")
library("texreg")
library("plm")
library("sandwich")
library("lmtest")
library("ggplot2")

data.table::setDTthreads(2)

source("fun.R")

deaths = data.table::fread("../dat/deaths.csv", na.strings = "")
municipalities = data.table::fread("../dat/spatialagg.txt")

deaths = deaths[sep_dec == TRUE]

deaths = municipalities[, list(amco = ACODE, corop = Corop, egg = EGG, province = Provincie)
    ][ deaths, on = "amco"]

deaths[, skill_level := factor(skill_level, 
    levels = c("higher_skilled", "medium_skilled", "lower_skilled", "unskilled"))]

aggvrbs = c("year", "event_month", "agegroup", "pr_gender", "skill_level", "final_under_roof", "final_meet_strangers")

# 10 year age bins, municipalities
agebin = 10
deaths[, agegroup := floor(pr_age / agebin) * agebin]

# 10 year age bins, egg regions
excess_egg10 = excess(deaths,
    aggvrbs = c("egg", aggvrbs))

# ma5 = lm(emr ~ factor(agegroup) + skill_level + pr_gender , data = excess_amco5)
# ma5l = lm(log1p(emr) ~ factor(agegroup) + skill_level + pr_gender , data = excess_amco5)

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

modlist_base = list(
    `skill` = lm(log1p(emr) ~ skill_level, 
        data = excess_egg10),
    `exposure` = lm(log1p(emr) ~ final_under_roof*final_meet_strangers, 
        data = excess_egg10),
    `both` = lm(log1p(emr) ~ skill_level + final_under_roof*final_meet_strangers, 
        data = excess_egg10),
    `+ age` = lm(log1p(emr) ~ skill_level + final_under_roof*final_meet_strangers + factor(agegroup), 
        data = excess_egg10),
    `+ sex` = lm(log1p(emr) ~ skill_level + final_under_roof*final_meet_strangers + factor(agegroup) + pr_gender, 
        data = excess_egg10),
    `+ month` = lm(log1p(emr) ~ skill_level + final_under_roof*final_meet_strangers + factor(agegroup) + pr_gender + factor(event_month), 
        data = excess_egg10),
    `+ region FE` = lm(log1p(emr) ~ skill_level + final_under_roof*final_meet_strangers + factor(agegroup) + pr_gender + factor(event_month) + factor(egg), 
        data = excess_egg10))

# map to reorder coefficients
# varnames = texreg::extract(modlist_base[["+ region FE"]])@coef.names
# cat(varnames[!grepl("egg", varnames)], sep = "\n")
coefmap = list(
    "skill_levelmedium_skilled" = "skill_levelmedium_skilled",
    "skill_levellower_skilled" = "skill_levellower_skilled",
    "skill_levelunskilled" = "skill_levelunskilled",

    "final_under_roof" = "indoor",
    "final_meet_strangers" = "strangers",
    "final_under_roof:final_meet_strangers" = "indoor x strangers",
    
    "factor(agegroup)20" = "factor(agegroup)20",
    "factor(agegroup)30" = "factor(agegroup)30",
    "factor(agegroup)40" = "factor(agegroup)40",
    "factor(agegroup)50" = "factor(agegroup)50",
    "factor(agegroup)60" = "factor(agegroup)60",
    "factor(agegroup)70" = "factor(agegroup)70",
    
    "pr_genderm" = "pr_genderm",
    
    "factor(event_month)10" = "factor(event_month)10",
    "factor(event_month)11" = "factor(event_month)11",
    "factor(event_month)12" = "factor(event_month)12",
    
    "(Intercept)" = "(Intercept)"
)

coeflist = lapply(modlist_base, coeftest, vcov. = sandwich::vcovCL, cluster = ~ egg)

texreg::screenreg(modlist_base, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    file = "../out/models_base.txt")
texreg::texreg(modlist_base, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    file = "../out/models_base.tex")

# check model outcomes at different aggregation levels
# 10 year age bins, municipalities
excess_amco10 = excess(deaths,
    aggvrbs = c("amco", aggvrbs))

# 10 year age bins, corop regions
excess_corop10 = excess(deaths,
    aggvrbs = c("corop", aggvrbs))

# 10 year age bins, provinces
excess_province10 = excess(deaths,
    aggvrbs = c("province", aggvrbs))

# rename to region to refer to same clustering variable
excess_amco10[, region := amco]
excess_egg10[, region := egg]
excess_corop10[, region := corop]
excess_province10[, region := province]

modlist_regions = list(
    `municipalities` = lm(log1p(emr) ~ skill_level + final_under_roof*final_meet_strangers + factor(agegroup) + pr_gender + factor(event_month) + factor(region), 
        data = excess_amco10),
    `*EGG*` = lm(log1p(emr) ~ skill_level + final_under_roof*final_meet_strangers + factor(agegroup) + pr_gender + factor(event_month) + factor(region), 
        data = excess_egg10),
    `COROP` = lm(log1p(emr) ~ skill_level + final_under_roof*final_meet_strangers + factor(agegroup) + pr_gender + factor(event_month) + factor(region), 
        data = excess_corop10),
    `Province` = lm(log1p(emr) ~ skill_level + final_under_roof*final_meet_strangers + factor(agegroup) + pr_gender + factor(event_month) + factor(region), 
        data = excess_province10)
)
coeflist = lapply(modlist_regions, coeftest, vcov. = sandwich::vcovCL, cluster = ~ region)

texreg::texreg(modlist_regions, 
    custom.coef.map = coefmap,
    override.se = lapply(coeflist, `[`, i = , j = 2),
    override.pval = lapply(coeflist, `[`, i = , j = 4),
    file = "../out/models_regions.tex")

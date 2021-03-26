rm(list = ls())

library("data.table")
library("texreg")

source("fun.R")

deaths = data.table::fread("../dat/deaths.csv")
municipalities = data.table::fread("../dat/spatialagg.txt")

deaths = municipalities[, list(amco = ACODE, corop = Corop, egg = EGG)][deaths, on = "amco"]

# 5 year age bins, municipalities
agebin = 5
deaths[, agegroup := floor(pr_age / agebin) * agebin]
excess_amco5 = excess(deaths)

# 10 year age bins, municipalities
agebin = 10
deaths[, agegroup := floor(pr_age / agebin) * agebin]
excess_amco10 = excess(deaths)

# 10 year age bins, corop regions
excess_corop10 = excess(deaths,
    aggvrbs = c("year", "agegroup", "pr_gender", "corop", "skill_level", "final_under_roof", "final_meet_strangers"))

# 10 year age bins, egg regions
excess_egg10 = excess(deaths,
    aggvrbs = c("year", "agegroup", "pr_gender", "egg", "skill_level", "final_under_roof", "final_meet_strangers"))

ma5 = lm(emr ~ factor(agegroup) + skill_level + pr_gender , data = excess_amco5)
ma5l = lm(log1p(emr) ~ factor(agegroup) + skill_level + pr_gender , data = excess_amco5)

ma10 = lm(emr ~ factor(agegroup) + skill_level + pr_gender , data = excess_amco10)
ma10l = lm(log1p(emr) ~ factor(agegroup) + skill_level + pr_gender , data = excess_amco10)

mc10 = lm(emr ~ factor(agegroup) + skill_level + pr_gender , data = excess_corop10)
mc10l = lm(log1p(emr) ~ factor(agegroup) + skill_level + pr_gender , data = excess_corop10)

me10 = lm(emr ~ factor(agegroup) + skill_level + pr_gender , data = excess_egg10)
me10l = lm(log1p(emr) ~ factor(agegroup) + skill_level + pr_gender , data = excess_egg10)

texreg::screenreg(list(ma5, ma5l, ma10, ma10l, mc10, mc10l, me10, me10l),
    file = "../out/models.txt")

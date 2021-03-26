rm(list = ls())

library("data.table")
library("texreg")

source("fun.R")

deaths = data.table::fread("../dat/deaths.csv")

# 10 year age bins, municipalities
agebin = 10
deaths[, agegroup := floor(pr_age / agebin) * agebin]
excess_amco10 = excess(deaths)


m1 = lm(emr ~ factor(agegroup) + skill_level + pr_gender , data = excess_amco10)
m2 = lm(log1p(emr) ~ factor(agegroup) + skill_level + pr_gender , data = excess_amco10)

texreg::screenreg(list(m1, m2))

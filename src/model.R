rm(list = ls())

library("data.table")
library("texreg")

excess = data.table::fread("../dat/excess.csv")

m1 = lm(emr ~ factor(agegroup) + skill_level + pr_gender , data = excess)
m2 = lm(log1p(emr) ~ factor(agegroup) + skill_level + pr_gender , data = excess)

texreg::screenreg(list(m1, m2))

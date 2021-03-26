rm(list = ls())

library("data.table")
library("texreg")

deaths= data.table::fread("../dat/deaths.csv")

# aggregate and calc excess mortality
agebin = 10
deaths[, agegroup := floor(pr_age / agebin) * agebin]

aggvrbs = c("year", "agegroup", "pr_gender", "amco", "skill_level", "final_under_roof", "final_meet_strangers")

# omit 1914 because belgian refugees
excess = deaths[year <= 1918 & year != 1914, list(deaths = .N), by = aggvrbs]

# cross-join to get a row for groups with zero deaths
tojoin = deaths[year <= 1918, do.call(CJ, c(.SD, unique = TRUE)), .SDcols = aggvrbs]
# tojoin = na.omit(tojoin)

excess = merge(excess, tojoin, by = aggvrbs, all = TRUE)
excess[is.na(deaths), deaths := 0]

excess[, y1918 := year == 1918]
excess = excess[between(agegroup, 10, 70),
    list(flu = mean(deaths[y1918 == TRUE]), 
        baseline = mean(deaths[y1918 == FALSE])),
    by = c(aggvrbs[aggvrbs != "year"])]

# drop if no deaths 1910-1917
excess = excess[baseline > 0]

excess[, emr := flu / baseline]

m1 = lm(emr ~ factor(agegroup) + skill_level + pr_gender , data = excess)
m2 = lm(log1p(emr) ~ factor(agegroup) + skill_level + pr_gender , data = excess)

texreg::screenreg(list(m1, m2))

rm(list = ls())

library("data.table")
library("knitr")
options(knitr.kable.NA = "-")

source("fun.R")
source("coefmap.R")

deaths = data.table::fread("../dat/deaths.csv", na.strings = "")

# subset dataset
deaths = deaths[sep_dec == TRUE] # only compare pandemic months
deaths[!is.na(skill_level), .N, by = pr_age][order(pr_age)]

deaths = deaths[data.table::between(pr_age, 12, 79)] # no <10 and no >=80 (ending at 79 for 10y binning)
deaths = deaths[year <= 1918]

deaths[, agegroup := cut(pr_age, c(11, 30, 45, 60, 80))]
deaths[!is.na(HISCO), farmer := HISCO %in% c(61220)] # also 62210, 62105 ?
toboot = deaths[!is.na(skill_level) & !is.na(exposure) & !is.na(farmer) & !is.na(agegroup) & !is.na(pr_gender)]

# boostrap with sims in list column
# x = toboot[!is.na(skill_level), list(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), by = skill_level]
# x[, list(skill_level, sapply(V1, mean), sapply(V1, sd))]

# bootstrap with sims in long form
skill_level = toboot[order(match(skill_level, c("higher_skilled", "medium_skilled", "lower_skilled", "unskilled"))), 
    unlist(boot::boot(.SD, excess_boot, R = 1e3, parallel = "multicore", ncpus = 3)["t"]), 
    by = skill_level]

skill_level[, i := 1:.N, by = skill_level]
skill_level[, delta := V1 - shift(V1), by = i]
out = skill_level[, 
    list(EMR = mean(V1), sd = sd(V1), difference = mean(delta), sd_diff = sd(delta)), 
    by = list(group = skill_level)]
out = knitr::kable(out, digits = 2, format = "latex", 
    caption = "Excess mortality rates in September-December 1918 by occupational skill group, with bootstrapped standard errors.",
    label = "lab:emr_byskill_boot")
writeLines(out, "../out/emr_byskill_boot.tex")

# other boots
exposure = toboot, 
    unlist(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), 
    by = exposure]
farmer = toboot, 
    unlist(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), 
    by = farmer]
agegroup = toboot, 
    unlist(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), 
    by = agegroup]
sex = toboot, 
    unlist(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), 
    by = pr_gender]

out = rbindlist(
    list(
        skill_level = skill_level[, 
            list(mean = mean(V1), sd = sd(V1)), 
            by = list(group = skill_level)],
        exposure = exposure[, 
            list(mean = mean(V1), sd = sd(V1)), 
            by = list(group = exposure)]
    ),
    idcol = "group")

hist(skills[, V1[skill_level == "higher_skilled"] - V1[skill_level == "lower_skilled"]])
quantile(skills[, V1[skill_level == "higher_skilled"] - V1[skill_level == "unskilled"]], c(0.5, 0.8, 0.9, 0.95, 0.99))

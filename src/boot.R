rm(list = ls())

library("data.table")
library("knitr")
library("boot")
options(knitr.kable.NA = "-")

source("fun.R")
source("coefmap.R")

deaths = data.table::fread("../dat/deaths_subset.csv", na.strings = "")

deaths[, agegroup := cut(pr_age, c(11, 30, 45, 60, 80))]
deaths[!is.na(HISCO), farmer := HISCO %in% c(61220)] # also 62210, 62105 ?

toboot = deaths[!is.na(skill_level) & !is.na(exposure) & !is.na(farmer) & !is.na(agegroup) & !is.na(pr_gender)]

# boostrap with sims in list column
# x = toboot[!is.na(skill_level), list(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), by = skill_level]
# x[, list(skill_level, sapply(V1, mean), sapply(V1, sd))]

# bootstrap with sims in long form
set.seed(0912)
skill_level = toboot[order(match(skill_level, c("higher_skilled", "medium_skilled", "lower_skilled", "unskilled"))), 
    unlist(boot::boot(.SD, excess_boot, R = 1e3, parallel = "multicore", ncpus = 3)["t"]), 
    by = skill_level]

skill_level[, i := 1:.N, by = skill_level]
skill_level[, delta := V1 - shift(V1), by = i]
out = skill_level[, 
    list(`Excess mort.` = mean(V1), 
         se = sd(V1), # bootstrap se is sd of the replications
         difference = mean(delta), 
         `se(diff)` = sd(delta)), 
    by = list(group = skill_level)]
out = knitr::kable(out, digits = 2, format = "latex", 
    caption = "Excess mortality rates in September-December 1918 by occupational skill group and their differences, with bootstrapped standard errors.",
    label = "lab:emr_byskill_boot")
writeLines(out, "../out/emr_byskill_boot.tex")

# other boots
exposure = toboot[, 
    unlist(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), 
    by = exposure]
farmer = toboot[, 
    unlist(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), 
    by = farmer]
agegroup = toboot[, 
    unlist(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), 
    by = agegroup]
sex = toboot[, 
    unlist(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), 
    by = pr_gender]

out = rbindlist(
    list(
        skill_level = skill_level[, 
            list(mean = mean(V1), se = sd(V1)), 
            by = list(group = skill_level)],
        exposure = exposure[, 
            list(mean = mean(V1), se = sd(V1)), 
            by = list(group = exposure)]
    ),
    idcol = "group")

# now do the emr by occupation + corop, checking for t.test(x, 1)
# need to loop and try for sparse cells

toboot = toboot[order(match(skill_level, c("higher_skilled", "medium_skilled", "lower_skilled", "unskilled")))]

fill = list()
for (i in unique(toboot$corop)){
    cat(i, "\n")
    try(
        fill[[as.character(i)]] <- toboot[corop == i,
        unlist(boot::boot(.SD, excess_boot, R = 500, parallel = "multicore", ncpus = 3)["t"]), 
        by = skill_level]
    )
}

# lapply(fill, function(x) set(x, j = "delta", value = V1 - shift(V1)))
corop = rbindlist(fill, id = "corop")
corop[, i := 1:.N, by = skill_level]
corop[, delta := V1 - shift(V1), by = i]

corop[, list(
        em = mean(V1),
        sd_em = sd(V1),
        diff = mean(delta),
        sd_diff = sd(delta)),
    by = list(skill_level, corop)]


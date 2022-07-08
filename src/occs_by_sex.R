rm(list = ls())

library("data.table")
library("knitr")

source("fun.R")

# cleaned deaths
deaths = data.table::fread("../dat/deaths.csv", na.strings = "")
deaths_subset = fread("../dat/deaths_subset.csv", na.strings = "")
deaths[, hiscolab := names(sort(-table(occtitle_st)))[1], by = HISCO]

totab = merge(deaths[, .N, by = list(HISCO, hiscolab, pr_gender, skill_level, exposure)],
    deaths_subset[, .N, by = list(HISCO, pr_gender, skill_level, exposure)],
    by = c("HISCO", "pr_gender", "skill_level", "exposure"),
    suffixes = c("_all", "_subset"),
    all = TRUE)

totab[, share_all := sum(N_all, na.rm = TRUE), by = pr_gender]
totab[, share_all := N_all / share_all]
totab[, share_subset := sum(N_subset, na.rm = TRUE), by = pr_gender]
totab[, share_subset := N_subset / share_subset]
totab = totab[order(-N_all)]

out = dcast(totab, HISCO + hiscolab + skill_level + exposure ~ pr_gender, value.var = list("N_all", "N_subset", "share_all", "share_subset"))[order(-N_subset_m)]
knitr::kable(out[1:100])
topoccs = totab[1:30, HISCO]

plot(N_all ~ N_subset, data = totab, log = "xy")
curve(1*x, add = TRUE)
plot(N_all_m ~ N_subset_m, data = out, log = "xy")
curve(1*x, add = TRUE)
plot(N_all_f ~ N_subset_f, data = out, log = "xy")
curve(1*x, add = TRUE)

plot(share_all ~ share_subset, data = totab, log = "xy")
curve(1*x, add = TRUE)
plot(share_all_m ~ share_subset_m, data = out, log = "xy")
curve(1*x, add = TRUE)
plot(share_all_f ~ share_subset_f, data = out, log = "xy")
curve(1*x, add = TRUE)

fwrite(out, "../out/occs_by_sex.csv")

knitr::kable(out[order(-share_all_f), list(HISCO, hiscolab, share_all_m, share_all_f, share_subset_m, share_subset_f)][1:20], digits = 4)
knitr::kable(out[order(-share_all_f), list(skill_level, hiscolab, share_all_m, share_all_f, share_subset_m, share_subset_f)][1:20], digits = 4)
knitr::kable(out[order(-share_all_f), list(exposure, hiscolab, share_all_m, share_all_f, share_subset_m, share_subset_f)][1:20], digits = 4)

# so there are a few female occupations in there: maid, nurse, seemstress, teachers (where it should be noted hisco seems to distinguish female teachers from male teachers [prim v. sec?])
deaths[HISCO_THREE == 133, .N, by = list(pr_gender, hiscolab)]
deaths[HISCO_THREE == 132, .N, by = list(pr_gender, hiscolab)]

knitr::kable(out[order(-(share_all_f / share_all_m)),list(skill_level, hiscolab, share_all_m, share_all_f, share_subset_m, share_subset_f)][1:20], digits = 4)
knitr::kable(out[order(-(share_subset_f / share_subset_m)),list(skill_level, hiscolab, share_all_m, share_all_f, share_subset_m, share_subset_f)][1:20], digits = 4)

knitr::kable(out[order(-(share_all_f / share_all_m)),list(skill_level, hiscolab, N_all_m, N_all_f, N_subset_m, N_subset_f)][1:20], digits = 4)
knitr::kable(out[order(-(share_subset_f / share_subset_m)),list(skill_level, hiscolab, N_all_m, N_all_f, N_subset_m, N_subset_f)][1:20], digits = 4)

# the numbers are maybe not too bad in the raw data: 13 men for every woman with an occupation, or 11 in the final data
deaths[, sum(HISCO > 0, na.rm = TRUE), by = pr_gender]
deaths_subset[, sum(HISCO > 0, na.rm = TRUE), by = pr_gender]
tab = deaths_subset[, sum(HISCO > 0, na.rm = TRUE), by = list(pr_gender, skill_level)][order(pr_gender, skill_level)]
tab[!is.na(skill_level), list(skill_level, V1 / sum(V1)), by = pr_gender]
# the high frequency of maids means unskilled are a bigger pct here, but the numers are not huge: 50 v. 30 %. Higher skilled women are very rare though.

tab = deaths_subset[, sum(HISCO > 0, na.rm = TRUE), by = list(pr_gender, exposure)][order(pr_gender, exposure)]
tab[!is.na(exposure), list(exposure, V1 / sum(V1)), by = pr_gender]

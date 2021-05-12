rm(list = ls())

source("fun.R")

library("data.table")
library("knitr")

deaths = data.table::fread("../dat/deaths.csv", na.strings = "")

deaths = deaths[!is.na(HISCO) & sep_dec == TRUE & data.table::between(pr_age, 10, 70) & event_year <= 1918]
deaths[, agegroup := 40]

emr_occ = excess(deaths, aggvrbs = c("HISCO", "year", "agegroup"))

occs = deaths[,
    list(
        occtitle = names(sort(-table(occtitle_st)))[1],
        .N,
        strangers = sum(final_meet_strangers),
        indoors = sum(final_under_roof),
        both = sum(final_meet_strangers * final_under_roof),
        neither = sum(final_meet_strangers + final_under_roof == 0),
        skill = gsub("_?skilled", "", skill_level[1]),
        hiscam = HISCAM_NL[1]),
    by = HISCO]
occs = emr_occ[, list(HISCO, emr)][occs, on = "HISCO"]
out = knitr::kable(
    x = occs[order(-N)][1:30],
    digits = 1,
    caption = "Most frequent occupations on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topoccs_selected",
    format = "latex")
writeLines(out, "../out/topoccs_sepdec.tex")

out = knitr::kable(
    x = occs[order(-indoors), list(HISCO, emr, occtitle, indoors)][1:20],
    digits = 1,
    caption = "Most frequent indoors occupations on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topindoors",
    format = "latex")
writeLines(out, "../out/topindoors.tex")
out = knitr::kable(
    x = occs[order(-strangers), list(HISCO, emr, occtitle, strangers)][1:20],
    digits = 1,
    caption = "Most frequent strangers occupations on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topstrangers",
    format = "latex")
writeLines(out, "../out/topstrangers.tex")
out = knitr::kable(
    x = occs[order(-both), list(HISCO, emr, occtitle, both)][1:20],
    digits = 1,
    caption = "Most frequent indoors and strangers occupations on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topboth",
    format = "latex")
writeLines(out, "../out/topboth.tex")
out = knitr::kable(
    x = occs[order(-neither), list(HISCO, emr, occtitle, neither)][1:20],
    digits = 1,
    caption = "Most frequent indoors nor strangers occupations on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topneither",
    format = "latex")
writeLines(out, "../out/topneither.tex")

# also most frequent occupations by skill level
occs = deaths[,
    list(
        occtitle = names(sort(-table(occtitle_st)))[1],
        .N), 
    by = list(skill_level, HISCAM_NL, HISCO)]
occs = emr_occ[, list(HISCO, emr)][occs, on = "HISCO"]
out = knitr::kable(
    x = occs[order(-N), list(HISCO, occtitle, skill_level, HISCAM_NL, N, emr)][1:40],
    digits = 1,
    caption = "Most frequent skill levels on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topskill",
    format = "latex")
writeLines(out, "../out/topskill.tex")

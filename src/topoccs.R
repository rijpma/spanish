rm(list = ls())

source("fun.R")

library("data.table")
library("knitr")

deaths = data.table::fread("../dat/deaths_subset.csv", na.strings = "")
occlabels = data.table::fread("../dat/occlabels.csv")

deaths = deaths[!is.na(HISCO)]

emr_occ = excess(deaths, aggvrbs = c("HISCO", "year"))

occs = deaths[,
    list(
        title = names(sort(-table(occtitle_st)))[1],
        .N,
        contact = unique(final_meet_strangers),
        indoors = unique(final_under_roof),
        # both = unique(final_meet_strangers * final_under_roof),
        # neither = unique(final_meet_strangers + final_under_roof == 0),
        skill = gsub("_?skilled", "", skill_level[1]),
        hiscam = HISCAM_NL[1]),
    by = HISCO]
occs = occs[emr_occ[, list(HISCO, emr)], on = "HISCO"]
occs[, skill := gsub("un", "unskilled", skill)]
occs = merge(occs, occlabels, by.x = "HISCO", by.y = "hisco", all.x = TRUE)


out = xtable::xtable(
    occs[order(-N)][1:25, list(HISCO, label, N, contact, indoors, skill, hiscam, emr)],
    digits = 1,
    caption = "Most frequent occupations on death certificates for deceased age 16-79, September--December months in 1910--1918.",
    label = "tab:topoccs_selected")
print(
    out, 
    include.rownames = FALSE,
    table.placement = "h!",
    file = "../out/topoccs_sepdec.tex")

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

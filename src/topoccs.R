rm(list = ls())

library("data.table")
library("knitr")

deaths = data.table::fread("../dat/deaths.csv", na.strings = "")

occs = deaths[!is.na(HISCO),
    list(
        occtitle = occtitle_st[1],
        .N,
        strangers = sum(final_meet_strangers),
        indoors = sum(final_under_roof),
        both = sum(final_meet_strangers * final_under_roof),
        neither = sum(final_meet_strangers + final_under_roof == 0)),
    by = HISCO]
out = knitr::kable(occs[order(-N)][1:20],
    caption = "Most frequent occupations on death certificates, 1910–1930.",
    label = "tab:topoccs_overal",
    format = "latex")
writeLines(out, "../out/topoccs_overal.tex")

occs = deaths[!is.na(HISCO) & sep_dec == TRUE & data.table::between(pr_age, 10, 70) & event_year <= 1918,
    list(
        occtitle = occtitle_st[1],
        .N,
        strangers = sum(final_meet_strangers),
        indoors = sum(final_under_roof),
        both = sum(final_meet_strangers * final_under_roof),
        neither = sum(final_meet_strangers + final_under_roof == 0)),
    by = HISCO]
out = knitr::kable(
    x = occs[order(-N)][1:20],
    caption = "Most frequent occupations on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topoccs_selected",
    format = "latex")

writeLines(out, "../out/topoccs_sepdec.tex")

out = knitr::kable(
    x = occs[order(-indoors), list(HISCO, occtitle, indoors)][1:20],
    caption = "Most frequent indoors occupations on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topindoors",
    format = "latex")
writeLines(out, "../out/topindoors.tex")
out = knitr::kable(
    x = occs[order(-strangers), list(HISCO, occtitle, strangers)][1:20],
    caption = "Most frequent strangers occupations on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topstrangers",
    format = "latex")
writeLines(out, "../out/topstrangers.tex")
out = knitr::kable(
    x = occs[order(-both), list(HISCO, occtitle, both)][1:20],
    caption = "Most frequent indoors and strangers occupations on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topboth",
    format = "latex")
writeLines(out, "../out/topboth.tex")
out = knitr::kable(
    x = occs[order(-neither), list(HISCO, occtitle, neither)][1:20],
    caption = "Most frequent indoors nor strangers occupations on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topneither",
    format = "latex")
writeLines(out, "../out/topneither.tex")

# also most frequent occupations by skill level
occs = deaths[!is.na(HISCO) & sep_dec == TRUE & data.table::between(pr_age, 10, 70) & event_year <= 1918,
    list(occtitle = occtitle_st[1], .N), 
    by = list(skill_level, HISCO)]
out = knitr::kable(
    x = occs[order(-N), list(HISCO, occtitle, skill_level, N)][1:40],
    caption = "Most frequent skill levels on death certificates for deceased age 10-70, september-december 1910–1918.",
    label = "tab:topskill",
    format = "latex")
writeLines(out, "../out/topskill.tex")

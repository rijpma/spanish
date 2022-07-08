rm(list = ls())

library("data.table")
library("knitr")

source("fun.R")

sumstats = function(dat){
    out = dat[!is.na(amco), 
        list(
            certificates = .N, 
            municip. = uniqueN(amco),
            `mean age` = mean(pr_age, na.rm = TRUE),
            `% male` = mean(pr_gender == "m", na.rm = TRUE) * 100,
            `% unskilled` = mean(skill_level == "unskilled", na.rm = TRUE) * 100,
            `% contact` = mean(exposure == "strangers only", na.rm = TRUE) * 100
        )
    ]
    return(out)
}

# cleaned deaths
deaths = data.table::fread("../dat/deaths.csv", na.strings = "")

# municipalities with sufficient number of certificates
municipalities = data.table::fread("../dat/spatialagg.txt")

deaths = deaths[year <= 1918]
deaths = deaths[!is.na(amco)]

sumstatlist = list()
sumstatlist[["start"]] = sumstats(dat = deaths)

# Rick's list: drop municipalities with insufficient certificates
deaths = merge(
    x = deaths,
    y = municipalities[Sampled == "Sampled", list(amco = ACODE, corop = Corop, egg = EGG, prov = Provincie)],
    by = "amco",
    all.x = FALSE, all.y = FALSE)
deaths = deaths[egg != 81] # poor coverage after further selection steps

sumstatlist[["certificate coverage"]] = sumstats(dat = deaths)

# drop low coverage municipalities, should be same as in maps.R
coverage = deaths[, 
    list(hisco = mean(!is.na(HISCO), na.rm = TRUE),
         age = mean(!is.na(pr_age), na.rm = TRUE),
         sex = mean(!is.na(pr_gender), na.rm = TRUE),
         date = mean(!is.na(death_date), na.rm = TRUE)), 
     by = list(amco)]
coverage[, poor_coverage := hisco <= 0.1 | age <= 0.2 | date <= 0.4]
coverage[, poor_coverage := hisco <= 0.1 | age <= 0.5 | date <= 0.5 | sex <= 0.5]
coverage[, poor_coverage_strict_hisco := hisco <= 0.2 | age <= 0.5 | date <= 0.5 | sex <= 0.5]
# would drop an additional 75 munics

deaths = merge(
    x = deaths,
    y = coverage[poor_coverage == FALSE],
    by = "amco",
    all.x = FALSE, all.y = FALSE)

sumstatlist[["variable coverage"]] = sumstats(dat = deaths)

# drop 1914
deaths = deaths[year != 1914]

sumstatlist[["drop 1914"]] = sumstats(dat = deaths)

# sep-dec only
deaths = deaths[sep_dec == TRUE]

sumstatlist[["Sep-Dec"]] = sumstats(dat = deaths)

# subset ages
deaths = deaths[data.table::between(pr_age, 12, 79)]

sumstatlist[["11 < age < 79"]] = sumstats(dat = deaths)

# complete cases look
# huge amount of contact occs in missing ages
sumstatlist[["complete cases"]] = sumstats(
    dat = deaths[!is.na(skill_level) 
        & !is.na(exposure)
        & !is.na(event_month) 
        & !is.na(pr_gender) 
        & !is.na(pr_age) 
    ]
)

fwrite(deaths, "../dat/deaths_subset.csv")
fwrite(deaths[pr_age >= 16], "../dat/deaths_subset_age16.csv")
fwrite(deaths[pr_age >= 16 & poor_coverage_strict_hisco == FALSE], "../dat/deaths_subset_age16_hisco20.csv")
fwrite(coverage, "../dat/coverage.csv")

out = rbindlist(sumstatlist, id = "selection")
out = knitr::kable(
    x = out, 
    digits = 0, 
    format = "latex",
    caption = "Summary statistics and selection steps.",
    label = "sumselect")
writeLines(out, "../out/selection_sumstats.tex")

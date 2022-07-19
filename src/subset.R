rm(list = ls())

library("data.table")
library("xtable")
library("ggplot2")

source("fun.R")

sumstats = function(dat){
    out = dat[!is.na(amco), 
        list(
            certificates = .N, 
            municip. = uniqueN(amco),
            `mean age` = mean(pr_age, na.rm = TRUE),
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

out = ggplot(melt(coverage, id.vars = "amco", value.name = "share_present"), aes(x = share_present)) + geom_histogram() + facet_wrap(~ variable) + theme_classic()
pdf("../out/coverage.pdf", height = 6)
print(out)
dev.off()

coverage[, poor_coverage := hisco <= 0.1 | age <= 0.2 | date <= 0.4]
coverage[, poor_coverage := hisco <= 0.1 | age <= 0.5 | date <= 0.5 | sex <= 0.5]
coverage[, poor_coverage_strict_hisco := hisco <= 0.2 | age <= 0.5 | date <= 0.5 | sex <= 0.5]
coverage[, poor_coverage_strict_everything := hisco <= 0.2 | age <= 0.8 | date <= 0.8 | sex <= 0.8]
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
deaths = deaths[data.table::between(pr_age, 16, 79)]

sumstatlist[["15 < age < 79"]] = sumstats(dat = deaths)

deaths_with_women = copy(deaths)
deaths = deaths[pr_gender == "m"]

sumstatlist[["men only"]] = sumstats(dat = deaths)

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
fwrite(deaths_with_women, "../dat/deaths_with_women.csv")
fwrite(coverage, "../dat/coverage.csv")

out = rbindlist(sumstatlist, id = "selection")
out = xtable(
    x = out, 
    digits = 0, 
    format = "latex",
    caption = "Summary statistics and selection steps. Source: Openarch Death Certificates. The table shows the stepwise selection of cases for analysis. The database contains a total of 741758 death certificates. We select only death certificates from municipalities fully available in the database (715703 cases) and where there is sufficient coverage for our variables (220795 cases). We also drop certain periods, age groups, and women (16786), and finally select complete cases (11469).",
    label = "sumselect")
print(out, file = "../out/selection_sumstats.tex")

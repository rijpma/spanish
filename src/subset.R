rm(list = ls())

library("data.table")

# cleaned deaths
deaths = data.table::fread("../dat/deaths.csv", na.strings = "")

# municipalities with sufficient number of certificates
municipalities = data.table::fread("../dat/spatialagg.txt")

# drop low coverage municipalities, should be same as in maps.R
coverage = deaths[, 
    list(hisco = mean(!is.na(HISCO), na.rm = TRUE),
         age = mean(!is.na(pr_age), na.rm = TRUE),
         sex = mean(!is.na(pr_gender), na.rm = TRUE),
         date = mean(!is.na(death_date), na.rm = TRUE)), 
     by = list(amco)]
coverage[, drop := hisco <= 0.1 | age <= 0.2 | date <= 0.4]

deaths[, list(.N, uniqueN(amco))]
deaths = merge(
    x = deaths,
    y = coverage[drop == FALSE],
    by = "amco",
    all.x = FALSE, all.y = FALSE)

# and another round based on Rick's list
deaths[, list(.N, uniqueN(amco))]
deaths = merge(
    x = deaths,
    y = municipalities[Sampled == "Sampled", list(amco = ACODE, corop = Corop, egg = EGG, prov = Provincie)],
    by = "amco",
    all.x = FALSE, all.y = FALSE)

deaths[, list(.N, uniqueN(amco))]

fwrite(deaths, "../dat/deaths_subset.csv")
fwrite(coverage, "../dat/coverage.csv")

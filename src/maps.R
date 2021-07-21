rm(list = ls())

library("data.table")
library("sf")
library("viridisLite")

source("fun.R")

deaths = data.table::fread("../dat/deaths.csv", na.strings = "")
municipalities = data.table::fread("../dat/spatialagg.txt")
coverage = fread("../dat/HDNG_OpenArch.txt")
nl = read_sf("../dat/nl_1918/nl_1918.shp")

deaths = merge(
    x = deaths,
    y = municipalities[, list(amco = ACODE, corop = Corop, egg = EGG)],
    by = "amco",
    all.x = TRUE, all.y = FALSE)

# death certificate coverage
meancov = coverage[value > 0, list(diffperc = mean(diffperc), value = mean(value), openach = mean(n_death_OpenArch)), by = list(acode = ACODE)]
meancov[, include := ifelse(abs(diffperc) >= 0 & abs(diffperc) < 50, TRUE, FALSE)]
municipalities[ACODE %in% meancov[include == FALSE, acode], .N, by = Provincie]

toplot = merge(
  nl,
  coverage[, list(coverage = TRUE), by = list(acode = ACODE)],
  by = "acode",
  all.x = TRUE)

toplot$coverage[is.na(toplot$coverage)] <- FALSE

pdf("../out/certificate_coverage.pdf", width = 6)
plot(toplot[, "coverage"], pal = viridisLite::viridis, main = "Certificate coverage")
dev.off()

# coverage other variables
# hisco_2 map to get only usable occs for pr_age >= 15
deaths[HISCO == "99999" | grepl("-", HISCO), HISCO := NA]

toplot = merge(
    nl,
    deaths[, 
        list(hisco = mean(!is.na(HISCO[pr_age >= 15 & pr_age <= 80]), na.rm = TRUE),
             age = mean(!is.na(pr_age), na.rm = TRUE),
             sex = mean(!is.na(pr_gender), na.rm = TRUE),
             date = mean(!is.na(death_date), na.rm = TRUE),
             N = .N), 
         by = list(acode = amco)],
    by = "acode",
    all.x = TRUE)

# fix scales by setting smallest munic
toplot[toplot$acode == 10980, c("hisco", "age", "sex", "date")] <- 0
toplot$hisco[toplot$hisco >= 0.6 & toplot$N < 10] <- NA
toplot[order(-toplot$hisco), ]

pdf("../out/age_coverage.pdf", width = 6)
plot(toplot[, "age"], pal = viridisLite::viridis, main = "Age coverage")
dev.off()
pdf("../out/sex_coverage.pdf", width = 6)
plot(toplot[, "sex"], pal = viridisLite::viridis, main = "Sex coverage")
dev.off()
pdf("../out/date_coverage.pdf", width = 6)
plot(toplot[, "date"], pal = viridisLite::viridis, main = "Date coverage")
dev.off()
pdf("../out/occupations_coverage.pdf", width = 6)
plot(toplot[, "hisco"], pal = viridisLite::viridis, main = "Occupation coverage")
dev.off()

# occupations range 0-0.4 because age < 20 age > 60 and f age > 40 rarely have one
# hisco should be bigger than say 0.2 for the relevant agegroup (0.1 overall)
# age should be bigger than say 0.2
# sex and date should be present
# and it should be in the coverage list

# emr plot
toplot = excess(deaths[sep_dec == TRUE & data.table::between(pr_age, 12, 79)], 
    aggvrbs = c("year", "amco"))

# note: subsetting on age and sep-dec changes the emr picture somewhat compared to the HLC paper
toplot = merge(
    nl,
    toplot,
    by.x = "acode", by.y = "amco",
    all.x = TRUE)

# trim outliers
toplot$emr2 <- cut(toplot$emr, c(0, 1, 2, 3, 4, 5, 55))
levels(toplot$emr2)[6] <- ">5"

pdf("../out/excessmap.pdf", width = 6)
plot(toplot[, "emr2"],  pal = viridisLite::viridis, main = "Excess mortality rate")
dev.off()

# corop and egg regions
toplot = merge(
    nl,
    municipalities[, list(acode = ACODE, egg = EGG, corop = Corop)],
    by = "acode",
    all.x = TRUE)
corops = aggregate(toplot, by = list(toplot$corop), length)
eggs = aggregate(toplot, by = list(toplot$egg), length)

pdf("../out/regions.pdf", width = 9)
par(mfrow = c(1, 2))
plot(eggs[, "Group.1"], col = NA, main = "EGG", reset = FALSE)
plot(corops[, "Group.1"], col = NA, main = "Corop", reset = FALSE)
dev.off()
# add hdng population

rm(list = ls())

library("data.table")
library("sf")
library("viridisLite")

source("fun.R")

deaths = data.table::fread("../dat/deaths.csv", na.strings = "")
municipalities = data.table::fread("../dat/spatialagg.txt")
coverage = fread("../dat/HDNG_OpenArch.txt")

deaths = municipalities[, list(amco = ACODE, corop = Corop, egg = EGG)][deaths, on = "amco"]

nl = read_sf("../dat/nl_1918/nl_1918.shp")

# death certificate coverage v1
toplot = merge(
    nl,
    coverage[, list(coverage = 1 - mean(diffperc / 100)), by = list(acode = ACODE)],
    by = "acode",
    all.x = TRUE)

pdf("../out/certificate_coverage.pdf", width = 6)
plot(toplot[, "coverage"], pal = viridisLite::viridis)
dev.off()

# death certificate coverage v2 to get rid of yellow Buren and 1/0 scale
toplot = merge(
  nl,
  coverage[, list(coverage = 1 ), by = list(acode = ACODE)],
  by = "acode",
  all.x = TRUE)

toplot[toplot$acode == 10980, c("coverage")] <- 0

pdf("../out/certificate_coverage.pdf", width = 6)
plot(toplot[, "coverage"], pal = viridisLite::viridis)
dev.off()

# hisco_2 map to get only usable occs for pr_age >= 15
deaths[HISCO == "99999" | grepl("-", HISCO), HISCO := NA,]

# coverage other variables
toplot = merge(
    nl,
    deaths[, 
        list(hisco = mean(!is.na(HISCO[pr_age >= 15 & pr_age <= 80]), na.rm = TRUE),
             age = mean(!is.na(pr_age), na.rm = TRUE),
             sex = mean(!is.na(pr_gender), na.rm = TRUE),
             date = mean(!is.na(death_date), na.rm = TRUE)), 
         by = list(acode = amco)],
    by = "acode",
    all.x = TRUE)

# fix scales by setting smallest munic
toplot[toplot$acode == 10980, c("hisco", "age", "sex", "date")] <- 0

pdf("../out/age_coverage.pdf", width = 6)
plot(toplot[, "age"], pal = viridisLite::viridis)
dev.off()
pdf("../out/sex_coverage.pdf", width = 6)
plot(toplot[, "sex"], pal = viridisLite::viridis)
dev.off()
pdf("../out/date_coverage.pdf", width = 6)
plot(toplot[, "date"], pal = viridisLite::viridis)
dev.off()
pdf("../out/occupations_coverage.pdf", width = 6)
plot(toplot[, "hisco"], pal = viridisLite::viridis)
dev.off()
# occupations range 0-0.4 because age < 20 age > 60 and f age > 40 rarely have one

# hisco_2 map to get only usable occs for pr_age >= 15

plot(ecdf(toplot$hisco))
as.data.table(toplot)[, .N, by = round(hisco, 1)][order(round)]

# age should be bigger than say 0.2
# hisco should be bigger than say 0.2 for the relevant agegroup (0.1 overall)
# sex and date should be present
# and it should be in the coverage list

# emr plot
deaths[, agegroup := pr_age]
deaths[data.table::between(pr_age, 10, 70), agegroup := 40]
toplot = excess(deaths[sep_dec == TRUE], aggvrbs = c("year", "amco", "agegroup"))

toplot = merge(
    nl,
    toplot,
    by.x = "acode", by.y = "amco",
    all.x = TRUE)
pdf("../out/excessmap.pdf", width = 6)
plot(toplot[, "emr"], pal = viridisLite::viridis)
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
# plot excess for all nld?

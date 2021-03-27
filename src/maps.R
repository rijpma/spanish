rm(list = ls())

library("data.table")
library("sf")
library("viridisLite")

source("fun.R")

deaths = data.table::fread("../dat/deaths.csv")
municipalities = data.table::fread("../dat/spatialagg.txt")
coverage = fread("../dat/HDNG_OpenArch.txt")

deaths = municipalities[, list(amco = ACODE, corop = Corop, egg = EGG)][deaths, on = "amco"]

nl = read_sf("~/data/nlgis/Gemeentegeschiedenis/nl_1918/nl_1918.shp")

# death certificate coverage
toplot = merge(
    nl,
    coverage[, list(coverage = 100 - mean(diffperc)), by = list(acode = ACODE)],
    by = "acode",
    all.x = TRUE)
pdf("../out/certificate_coverage.pdf", width = 6)
plot(toplot[, "coverage"], pal = viridisLite::viridis)
dev.off()

# occupations coverage
toplot = merge(
    nl,
    deaths[, 
        list(hisco = mean(!is.na(HISCO)),
             age = mean(!is.na(pr_age)),
             sex = mean(!is.na(pr_gender) | pr_gender != ""),
             date = mean(!is.na(death_date))), 
         by = list(acode = amco)],
    by = "acode",
    all.x = TRUE)
pdf("../out/occupations_coverage.pdf", width = 6)
plot(toplot[, "hisco"], pal = viridisLite::viridis)
dev.off()

# emr plot
deaths[, agegroup := pr_age]
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

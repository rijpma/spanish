rm(list = ls())

library("data.table")
library("zoo")


data.table::setDTthreads(2)

source("fun.R")
source("coefmap.R")

# deaths191030 <- fread(cmd = "gunzip -c ../dat/deaths1910-30.csv.gz")
deaths = data.table::fread("../dat/deaths.csv", na.strings = "")
municipalities = data.table::fread("../dat/spatialagg.txt")
coverage = data.table::fread("../dat/coverage.csv")


# deaths = merge(
#     x = deaths,
#     y = municipalities[Sampled == "Sampled", list(amco = ACODE, corop = Corop, egg = EGG, prov = Provincie)],
#     by = "amco",
#     all.x = FALSE, all.y = FALSE)
deaths[, .N, by = list(event_year, is.na(death_date))][order(event_year, is.na)]
deaths[, .N, by = list(is.na(death_date))]

deaths[, agebin := cut(pr_age, c(0, 5, 20, 35, 50, 65, 120))]

toplot = deaths[
    data.table::between(death_date, "1917-01-01", "1919-12-31") & !is.na(agebin) & !is.na(pr_gender), 
    list(.N, death_date = min(death_date)),
    by = list(year(death_date), week(death_date), agebin, pr_gender) ][
        order(death_date, agebin, pr_gender)]
toplot

out = ggplot(toplot, aes(death_date, N, color = pr_gender)) + 
    geom_line() + 
    facet_wrap(~ agebin) + 
    theme_classic()

pdf("../out/weeklydeaths.pdf", height = 5, width = 8)
print(out)
dev.off()

par(mfrow = c(2, 3))
plot(N ~ death_date, data = toplot, type = "n")
lines(N ~ death_date, data = toplot[agebin == "(0,5]" & pr_gender == "m"], col = 4)
lines(N ~ death_date, data = toplot[agebin == "(0,5]" & pr_gender == "f"], col = 2)
# etc.
rm(list = ls())

library("data.table")
library("zoo")
library("ggplot2")
library("gridExtra")

source("fun.R")

deaths = data.table::fread("../dat/deaths.csv", na.strings = "")

deaths[, agegroup := cut(pr_age, c(11, 30, 45, 60, 80))]
deaths[, agegroup := cut(pr_age, c(16, 35, 50, 80))]

toplot = deaths[
    data.table::between(death_date, "1916-01-01", "1919-12-31") & !is.na(agegroup) & !is.na(pr_gender), 
    list(.N, death_date = min(death_date)),
    by = list(year(death_date), week(death_date), agegroup, sex = pr_gender) ][
        order(death_date, agegroup, sex)]

pdf("../out/weeklydeaths_base.pdf", height = 3, width = 9)
mypar(mfrow = c(1, 3))
plot(N ~ death_date, data = toplot, type = "n", 
    main = paste("age", "(16,35]"), xlab = "date", ylab = "N deaths")
lines(N ~ death_date, data = toplot[agegroup == "(16,35]" & sex == "m"], col = 4)
lines(N ~ death_date, data = toplot[agegroup == "(16,35]" & sex == "f"], col = 2)
plot(N ~ death_date, data = toplot, type = "n", 
    main = paste("age", "(35,50]"), xlab = "date", ylab = "N deaths")
lines(N ~ death_date, data = toplot[agegroup == "(35,50]" & sex == "m"], col = 4)
lines(N ~ death_date, data = toplot[agegroup == "(35,50]" & sex == "f"], col = 2)
plot(N ~ death_date, data = toplot, type = "n", 
    main = paste("age", "(50,80]"), xlab = "date", ylab = "N deaths")
lines(N ~ death_date, data = toplot[agegroup == "(50,80]" & sex == "m"], col = 4)
lines(N ~ death_date, data = toplot[agegroup == "(50,80]" & sex == "f"], col = 2)
legend("topleft", fill = c(2, 4), legend = c("f", "m"))
dev.off()
# etc.

# point of these graphs is to show that the 1910-17 <> 1918 comparison is ok, so same subsetting as in regressions
deaths = deaths[sep_dec == TRUE] # only compare pandemic months
deaths = deaths[data.table::between(pr_age, 12, 79)] # no <10 and no >=80 (ending at 79 for 10y binning)
deaths[, agegroup := cut(pr_age, c(11, 30, 45, 60, 80))]
deaths[!is.na(HISCO), farmer := HISCO %in% c(61220)] # also 62210, 62105 ?

toplot = deaths[year <= 1918 & !is.na(skill_level) & !is.na(exposure) & !is.na(pr_gender) & !is.na(agegroup)]

toplot[farmer == TRUE, skill_level := "farmer"]
toplot[exposure == "strangers only", exposure := "contact only"]

pdf("../out/annualdeaths_grouped_base.pdf", height = 6, width = 10)
mypar(mfrow = c(1, 2))
toplot2 = dcast(toplot, year ~ skill_level, fun = length)
toplot2 = toplot2[, order(-toplot2[1, ]), with = FALSE] # reorder for legend
groups = colnames(toplot2)[-1]
matplot(toplot2$year, toplot2[, -"year"], type = "n", log = "y",
    xlab = "date", ylab = "N deaths", main = "Skill level")
matlines(toplot2$year, toplot2[, -"year"], log = "y", type = "b", lty = 1, pch = 19)
legend("topleft", fill = 1:length(groups), legend = groups)

toplot2 = dcast(toplot, year ~ exposure, fun = length)
toplot2 = toplot2[, order(-toplot2[1, ]), with = FALSE]
groups = colnames(toplot2)[-1]
matplot(toplot2$year, toplot2[, -"year"], type = "n", log = "y",
    xlab = "date", ylab = "N deaths", main = "Exposure")
matlines(toplot2$year, toplot2[, -"year"], log = "y", type = "b", lty = 1, pch = 19)
legend("topleft", fill = 1:length(groups), legend = groups)

dev.off()
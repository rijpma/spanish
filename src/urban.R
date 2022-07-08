library("data.table")

deaths_all = data.table::fread("../dat/deaths.csv", na.strings = "")
deaths_sub = data.table::fread("../dat/deaths_subset.csv", na.strings = "")
municipalities = data.table::fread("../dat/spatialagg.txt")
pop = data.table::fread("../dat/inwonertal + bevolkingsdichtheid.txt")

deaths_all[, in_subset := amco %in% deaths_sub$amco]

munics = merge(
    municipalities,
    pop,
    by = "ACODE",
    all = TRUE)

munics[, in_certificates := ACODE %in% deaths_all$amco]
munics[, in_subset := ACODE %in% deaths_sub$amco]

munics[, r:= rank(-PopSize1918)]
munics[order(-PopSize1918)]
munics[in_subset == FALSE][order(-PopSize1918)][1:10, list(gemeente, PopSize1918)]
munics[in_subset == TRUE][order(-PopSize1918)][1:10, list(gemeente, PopSize1918)]

out = dcast(munics, gemeente + r ~ in_subset, value.var = "PopSize1918")[order(r)][1:20][, -"r"]
options(knitr.kable.NA = '')
knitr::kable(out)

plot(r ~ PopSize1918, data = munics, log = "xy", col = munics$in_subset + 1, pch = 19)

# emr urban-rural

x = excess(deaths_all, aggvrbs = c("year", "amco", "in_subset"))
x_sub = excess(deaths_sub, aggvrbs = c("year", "amco"))
x = merge(x, pop, by.x = "amco", by.y = "ACODE", all.x = TRUE)
x_sub = merge(x_sub, pop, by.x = "amco", by.y = "ACODE", all.x = TRUE)

par(mfrow = c(1, 1))
plot(log10(emr) ~ log10(PopSize1918), data = x[emr > 0])
m = lm(log10(emr) ~ log10(PopSize1918), data = x[emr > 0])
abline(m)
abline(h = mean(log10(x[emr > 0, emr])))
# so there is a positive gradient, 0.05, ie 1% larger city, 0.05% higher mort; or twice the city 
2^0.05
10^0.05
# 3% higher emr, or10x 12%, so this is not a huge gradient; what's more, the big 4, the ones we're most worried about, are pretty 

x[, mean(emr), by = in_subset]
x[, median(emr), by = in_subset]

summary(lm(emr ~ in_subset - 1, data = x))
summary(lm(emr ~ in_subset, data = x))
summary(lm(log(emr) ~ in_subset - 1, data = x[emr > 0]))
summary(lm(log(emr) ~ in_subset, data = x[emr > 0]))


plot(log10(emr) ~ log10(PopSize1918), data = x[emr > 0], type = "n")
points(log10(emr) ~ log10(PopSize1918), data = x_sub[emr > 0])
m = lm(log10(emr) ~ log10(PopSize1918), data = x_sub[emr > 0])
abline(m)
abline(a = mean)

smth = loess(emr ~ PopSize1918, data = x[order(PopSize1918)])
lines(smth$x, smth$fitted)

abline(lm(emr ~ PopSize1918, data = x), untf = TRUE)
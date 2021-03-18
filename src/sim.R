rm(list = ls())

library("data.table")
library("ggplot2")

# set up simulation parameters
N = 1000              # number of draws
bins = c(1, 2, 5, 10) # aggregation bins
sigma = 2             # noise
                      # vector of coefficients that we want to retrieve
b = c(`(Intercept)` = 60, age = -0.3, dum1 = -0.2, dum2 = 0.01, `dum2:year` = 0.2, year = 0.01, dum3 = 0.05)

# table to leave the regression estimates
estimates = data.table::CJ(
    binsize = bins, 
    iter = 1:N, 
    variable = names(b), 
    beta = NA_real_, # default NA is logical and gets assignment errors :(
    se = NA_real_, 
    df = NA_integer_)

for (bin in bins){
    for (i in 1:N){
        # data.frame with all combinations (real dataset will not have every combo)
        # totals to 800, our real one has about 10x that.
        sim_count = data.table::CJ(age = 1:100, year = 0:1, dum1 = 0:1, dum2 = 0:1, dum3 = 1:4)

        # simulate N deaths for each cell. Everything is simple and nice and
        # linear. In reality we have more of a cubic profile to age, and we
        # don't know the other estimates. The idea below is that there is one
        # dummy which always has an effect, and one which only has a sizeable
        # effect in one of the two years (the interaction term).
        sim_count[, N := rnorm(
            n = .N, 
            mean = b["(Intercept)"] + b["age"]*age + b["dum1"]*dum1 + b["dum2"]*dum2 + b["dum2:year"]*year*dum2 + b["year"]*year + b["dum3"]*dum3, 
            sd = sigma)]
        
        # round and drop negative death counts
        sim_count[, N := round(N)]
        sim_count = sim_count[N > 0]

        # here the numbers are aggregated using the floor(x / a)*a trick
        dat = sim_count[, 
            list(N = sum(N)), 
            by = list(age = floor(age / bin) * bin, dum1, dum2, year, dum3)]

        # get the estimates. Note: regression is on just N, the raw death
        # count. We're actually interested in N[year == 1] / N[year == 0] (and
        # for us, year == 0 is estimated over a few years), and this is easily
        # coded up. I went for straight N now though because I want to be able
        # to see if we can retrieve the parameter of interest, and once we
        # calculate an excess rate I don't know what that number is anymore
        # (fixable by simulating EMR directly?). My feeling is that the point
        # about the spread of the distribution of betas will stand, but who
        # knows?
        m = summary(lm(N ~ age + dum1 + dum2 + dum2*year + dum3, data = dat))

        # put the estimates in placeholder dataset. NB: this makes the loop a
        # bit slow, but simplifies the code tremendously
        estimates[binsize == bin & iter == i, beta := m$coefficients[variable, 1]]
        estimates[binsize == bin & iter == i, se := m$coefficients[variable, 2]]
        estimates[binsize == bin & iter == i, df := m$df[2]]
        cat(bin, i, " - ")
    }
}


p = ggplot(mapping = aes(beta, col = factor(binsize))) + 
    theme_classic()

pdf("../out/sims.pdf")
p + geom_density(data = estimates[variable == "dum1"]) + geom_vline(xintercept = b["dum1"], col = "gray")
p + geom_density(data = estimates[variable == "dum2"]) + geom_vline(xintercept = b["dum2"], col = "gray")
p + geom_density(data = estimates[variable == "dum2:year"]) + geom_vline(xintercept = b["dum2:year"], col = "gray")
p + geom_density(data = estimates[variable == "year"]) + geom_vline(xintercept = b["year"], col = "gray")
dev.off()
# we can also look at se and beta/se to get a feeling of the precision of our estimates

# priorities to play around with:
# ✅ bigger simulated dataset to begin with. Now we start with 800 and downsize to 80 in the worst case. It shouldn't be that bad for us
# estimate with EMR rather than N as the outcome variable. Would be done like this, I think (lm() call would also need to be changed)
dat = sim_count[, 
    list(EMR = sum(N[year == 1]) / N[year == 0]), 
    by = list(age = floor(age / bin) * bin, dum1, dum2)]
# sigma could be tweaked to make the model more/less noisy
# the coefficients to be retrieved could be tweaked to be more realistic. 
# even more noise could be introduced by randomising the creation of the original dataset as well
    
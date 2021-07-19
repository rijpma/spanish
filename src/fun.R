# year is not actually an aggvrb

excess = function(dat, 
    aggvrbs = c("year", "agegroup", "pr_gender", "amco", "skill_level", "final_under_roof", "final_meet_strangers")){

    # omit 1914 because belgian refugees
    dat = dat[year <= 1918 & year != 1914, list(deaths = .N), by = aggvrbs]

    # cross-join to get a row for groups with zero deaths
    tojoin = dat[, do.call(CJ, c(.SD, unique = TRUE)), .SDcols = aggvrbs]
    # tojoin = na.omit(tojoin)

    dat = merge(dat, tojoin, by = aggvrbs, all = TRUE)
    dat[is.na(deaths), deaths := 0]

    dat[, y1918 := year == 1918]
    dat = dat[,
        list(flu = mean(deaths[y1918 == TRUE]), 
            baseline = mean(deaths[y1918 == FALSE]),
            nbaseline = sum(deaths[!y1918]),
            nflu = sum(deaths[y1918])),
        by = c(aggvrbs[aggvrbs != "year"])]

    # drop if no deaths 1910-1917
    dat = dat[baseline > 0]

    dat[, emr := flu / baseline]

    return(dat)
}


texregse = function(output = texreg, mdl, vcov., ...){
    cfs = lapply(mdl, lmtest::coeftest, vcov. = vcov.)
    mdl = lapply(mdl, texreg::extract, include.adjrs = FALSE)
    output(
        l = mdl,
        override.se = lapply(cfs, function(x) x[, 2]),
        override.pval = lapply(cfs, function(x) x[, 4]),
         ...)
}

mypar = function(...){
    par(..., 
        bty = "l", 
        mar = c(3, 3, 2, 1), 
        mgp = c(1.7, .5, 0), 
        tck=-.01,
        font.main = 1)
}

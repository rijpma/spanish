excess = function(dat, 
    aggvrbs = c("year", "agegroup", "pr_gender", "amco", "skill_level", "final_under_roof", "final_meet_strangers")){

    # omit 1914 because belgian refugees
    dat = dat[year <= 1918 & year != 1914, list(deaths = .N), by = aggvrbs]

    # cross-join to get a row for groups with zero deaths
    tojoin = dat[year <= 1918, do.call(CJ, c(.SD, unique = TRUE)), .SDcols = aggvrbs]
    # tojoin = na.omit(tojoin)

    dat = merge(dat, tojoin, by = aggvrbs, all = TRUE)
    dat[is.na(deaths), deaths := 0]

    dat[, y1918 := year == 1918]
    dat = dat[between(agegroup, 10, 70),
        list(flu = mean(deaths[y1918 == TRUE]), 
            baseline = mean(deaths[y1918 == FALSE])),
        by = c(aggvrbs[aggvrbs != "year"])]

    # drop if no deaths 1910-1917
    dat = dat[baseline > 0]

    dat[, emr := flu / baseline]

    return(dat)
}
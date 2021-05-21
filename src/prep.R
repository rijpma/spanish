rm(list = ls())
library(data.table)
library(stringi)

deaths191030 <- fread(cmd = "gunzip -c ../dat/deaths1910-30.csv.gz")

# municipalities with sufficient number of certificates
munic_cov = fread("../dat/HDNG_OpenArch.txt")
deaths191030 = deaths191030[amco %in% unique(munic_cov$ACODE)]

topo <- fread("../dat/DutchToponyms1812-2012Spatio-Temporal.txt")
# nicer colnames
setnames(topo, tolower(names(topo)))
setnames(topo, "toponym (city,town,village,hamlet)", "toponym")
setnames(topo, "lattitude (3 dec.degrees)", "lat")
setnames(topo, "longitude (3 dec.degrees)" , "lon")
setnames(topo, "amsterdam code" , "amco")
setnames(topo, "part of municipality from jjjjmmdd", "startdate_munc")
setnames(topo, "till jjjjmmdd", "enddate_munc")

deaths191030[, HISCO_THREE := as.character(stri_sub(HISCO, from = 1, to = 3)),]

# add hisclass skill
deaths191030[HISCLASS == 1  | HISCLASS == 2, skill_level := "higher_skilled", ]
deaths191030[HISCLASS == 3  | HISCLASS == 4, skill_level := "medium_skilled", ]
deaths191030[HISCLASS == 6  | HISCLASS == 7  | HISCLASS == 8,  skill_level := "medium_skilled", ]
deaths191030[HISCLASS == 5  | HISCLASS == 9  | HISCLASS == 10, skill_level := "lower_skilled", ]
deaths191030[HISCLASS == 11 | HISCLASS == 12 | HISCLASS == 13, skill_level := "unskilled", ]

# covid coding
covid_coding <- fread("../dat/covid_coding_final.csv")
covid_coding[, HISCO_THREE := as.character(stri_sub(unitGroup, -3, -1)),]

# covid coding only occs where we agreed directly
covid2 <- fread("../dat/covid_coding_rr_ria.csv")

covid2[rr_confined == ria_confined & rr_meet == ria_meet, agreement := 1,]
covid2[is.na(agreement), agreement := 0,]

# add agreement to covid_coding
covid_coding <- merge(covid_coding, covid2[,c("agreement", "unitGroup")], by = "unitGroup")
fwrite(covid_coding, "../dat/exposurecoding_asinmodel.csv")

# merge coding
deaths191030 <- merge(
    deaths191030, 
    covid_coding[,c("HISCO_THREE", "label_clean", "final_under_roof", "final_meet_strangers", "agreement")], 
    by = "HISCO_THREE", 
    all.x = TRUE, all.y = FALSE)

# recode some occs that were not in original file or spelled differently with large N
deaths191030[HISCO_THREE == "042", final_under_roof := 1]
deaths191030[HISCO_THREE == "042", final_meet_strangers := 0]

deaths191030[HISCO_THREE == "033" | HISCO_THREE == "030", final_under_roof := 0]
deaths191030[HISCO_THREE == "033" | HISCO_THREE == "030", final_meet_strangers := 1]

deaths191030[HISCO_THREE == "065", final_under_roof := 0]
deaths191030[HISCO_THREE == "065", final_meet_strangers := 1]

deaths191030[HISCO_THREE == "980", final_under_roof := 0]
deaths191030[HISCO_THREE == "980", final_meet_strangers := 1]

# encoding misery
deaths191030[, occtitle_st := stringi::stri_replace_all_fixed(occtitle_st, "\xe3", "e")]
deaths191030[HISCO_THREE == "999" & grepl("fabriek", occtitle_st), final_under_roof := 1]

deaths191030[occtitle_st == "ziekenverpleegster", HISCO_THREE := "071"]
deaths191030[occtitle_st == "ziekenverpleegster", final_under_roof := 1]
deaths191030[occtitle_st == "ziekenverpleegster", final_meet_strangers := 1]
deaths191030[occtitle_st == "priester" | occtitle_st == "rkpriester", HISCO_THREE := "141"]
deaths191030[occtitle_st == "priester" | occtitle_st == "rkpriester", final_under_roof := 1]
deaths191030[occtitle_st == "priester" | occtitle_st == "rkpriester", final_meet_strangers := 1]
deaths191030[occtitle_st == "apotheker", HISCO_THREE := "067"]
deaths191030[occtitle_st == "apotheker", final_under_roof := 1]
deaths191030[occtitle_st == "apotheker", final_meet_strangers := 1]
deaths191030[occtitle_st == "herbergierster" | occtitle_st == "koffiehuishoudster", HISCO_THREE := "510"]
deaths191030[occtitle_st == "herbergierster" | occtitle_st == "koffiehuishoudster", final_under_roof := 1]
deaths191030[occtitle_st == "herbergierster" | occtitle_st == "koffiehuishoudster", final_meet_strangers := 1]

deaths191030[HISCO == 12310, skill_level := "higher_skilled"] # candidate notaries should have a skill level
deaths191030[HISCO == 92120, skill_level := "medium_skilled"] # letter setters should have a skill level

# drop HISCO 99999, -2, -3
deaths191030[HISCO == 99999 | HISCO == -2 | HISCO == -3, HISCO := NA]

# code exposure
deaths191030[final_under_roof == 0 & final_meet_strangers == 0, exposure := 0]
deaths191030[final_under_roof == 1 & final_meet_strangers == 0, exposure := 1]
deaths191030[final_under_roof == 0 & final_meet_strangers == 1, exposure := 2]
deaths191030[final_under_roof == 1 & final_meet_strangers == 1, exposure := 3]

deaths191030[, .N, list(exposure)][order(exposure)]

deaths191030[, death_date := as.Date(
  stri_join(event_year, "-", event_month, "-", event_day),
  format = "%Y-%m-%d")]

deaths191030[, plus40 := pr_age >= 40]
deaths191030[, year := year(death_date)]
deaths191030[is.na(year), year := event_year] # bit pointless as we'll need a date?
deaths191030[, flu := data.table::between(death_date, as.Date("1918-09-01"), as.Date("1918-12-31"))]
deaths191030[, sep_dec := data.table::between(event_month, 9, 12)]

deaths191030[pr_gender == "", pr_gender := NA]

fwrite(deaths191030, "../dat/deaths.csv")

coverage = deaths191030[, 
    list(hisco = mean(!is.na(HISCO), na.rm = TRUE),
         age = mean(!is.na(pr_age), na.rm = TRUE),
         sex = mean(!is.na(pr_gender), na.rm = TRUE),
         date = mean(!is.na(death_date), na.rm = TRUE)), 
     by = list(amco)]
coverage[, drop := hisco <= 0.1 | age <= 0.2 | date <= 0.4]

fwrite(coverage, "../dat/coverage.csv")

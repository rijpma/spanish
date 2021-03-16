### use exposure score 
library(data.table)
library(stringi)

deaths191030 <- fread(cmd = "gunzip -c ../dat/deaths1910-30.csv.gz")

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

# drop HISCO 99999, -2, -3
deaths191030 <- deaths191030[HISCO != 99999 & HISCO != -2 & HISCO != -3,]

# code exposure
deaths191030[final_under_roof == 0 & final_meet_strangers == 0, exposure := 0]
deaths191030[final_under_roof == 1 & final_meet_strangers == 0, exposure := 1]
deaths191030[final_under_roof == 0 & final_meet_strangers == 1, exposure := 2]
deaths191030[final_under_roof == 1 & final_meet_strangers == 1, exposure := 3]

deaths191030[, .N, list(exposure)][order(exposure)]

deaths191030[, death_date := as.Date(
  stri_join(event_year, "-", event_month, "-", event_day),
  format = "%Y-%m-%d")]

fwrite(deaths191030, "~/dat/civreg/deaths191030.csv")

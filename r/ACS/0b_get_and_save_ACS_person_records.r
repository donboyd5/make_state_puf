

#****************************************************************************************************
#               ONETIME: get ACS person records with selected info and save ####
#****************************************************************************************************
library("DBI")

# define locations
dbdir <- "D:/Data/CensusACS/20175year/RSQLITE/"
hcsvdir <- "D:/Data/CensusACS/20175year/csv_hus/"
hfnbase <- "psam_hus"

dbf <- paste0(dbdir, "acs.sqlite")

#.. get ENTIRE population so that we can match them with housholds!
acsdb <- dbConnect(RSQLite::SQLite(), dbf)
tname <- "acs2017_5year"
dbListTables(acsdb)
dbListFields(acsdb, tname)
getall <- tbl(acsdb, tname) # dplyr does lazy loading, so this does not really grab full table
str(getall)
glimpse(getall)

# intp interest income pap public assistance income retp retirement income
persons <- getall %>%
  select(serialno, sporder, st, pwgtp, adjinc, sex, agep, mar, intp, pap, retp, ssip, ssp, wagp)
# ht(persons) # tail not supported
# DON'T USE glimpse in this situation - it takes a long time
system.time(persons2 <- collect(persons, n=Inf)) # ~ 1 min
glimpse(persons2)
count(persons2, sporder)

# data(package="datasets")
codes <- unique(tigris::fips_codes %>% select(state, state_code) %>% mutate(state_code=as.numeric(state_code)))
persons3 <- persons2 %>%
  mutate(stabbr=factor(st, levels=codes$state_code, labels=codes$state),
         adjinc=adjinc / 1e6) %>%
  select(serialno, sporder, st, stabbr, pwgtp, adjinc, everything()) %>%
  mutate_at(vars(intp, pap, retp, ssip, ssp, wagp), ~ . * adjinc)
count(persons3, st, stabbr) # 1 min, 56 max
glimpse(persons3)

saveRDS(persons3, "d:/temp/persons.rds")
rm(persons, persons2, persons3, codes)

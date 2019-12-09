
base <- readRDS(here::here("data", "hist2_targets2011.rds")) %>%
  filter(stabbr=="US", AGI_STUB==0)

future <- readRDS(here::here("data", paste0("hist2_targets", 2017, ".rds"))) %>%
  filter(stabbr=="US", AGI_STUB==0)

# create a single set of table descriptions for the two files, favoring the future when available, otherwise using the base
table_desc <- base %>%
  select(h2vname, table_desc) %>%
  full_join(future %>% select(h2vname, table_desc), by="h2vname",
            suffix=c(".base", ".future")) %>%
  mutate(table_desc=ifelse(is.na(table_desc.future), table_desc.base, table_desc.future))


oddvars2011 <- c("MARS2", "N1", "N2", "NUMDEP", "PREP", "SCHF")
oddvars2017 <- c("CPREP", "DIR_DEP", "ELDERLY", "ELF", "MARS1", "MARS2", "MARS4", 
                 "MVITA", "NUMDEP", "PREP", "RAC", "SCHF", "TCE", "TOTAL_VITA", "VITA", "VITA_EIC")
oddvars <- c(oddvars2011, oddvars2017) %>% unique %>% sort

stack <- bind_rows(base, future) %>%
  select(year, stabbr, AGI_STUB, h2vname, target) %>%
  left_join(table_desc %>% select(h2vname, table_desc), by="h2vname") %>%
  mutate(vtype=case_when(h2vname %in% oddvars ~ "odd",
                         str_sub(h2vname, 1, 1)=="A" ~ "amount",
                         str_sub(h2vname, 1, 1)=="N" ~ "number",
                         TRUE ~ "ERROR")) %>%
  mutate(pufvname=ifelse(vtype %in% c("amount", "number"), paste0("E", str_sub(h2vname, 2, -1)), paste0(h2vname, "_odd")))

stack2 <- stack %>%
  select(-table_desc, -h2vname) %>%
  pivot_wider(names_from = vtype, values_from = target) %>%
  select(year, stabbr, AGI_STUB, pufvname, odd, number, amount) %>% # so they are in an order I like
  mutate(average=ifelse(!is.na(number), amount / number * 1000, odd))

grow_factors <- stack2 %>%
  select(year, stabbr, AGI_STUB, pufvname, average) %>%
  pivot_wider(names_from=year, values_from = average) %>%
  mutate(grow_factor=`2017` / `2011`)

grow_factors_vec <- grow_factors %>%
  filter(!str_detect(pufvname, "odd"),
         !is.na(grow_factor))

growvals <- grow_factors_vec$grow_factor
names(growvals) <- grow_factors_vec$pufvname
growvals


puf <- get_puf_xagg()
setdiff(names(puf), names(growvals))

grownames <- intersect(names(puf), names(growvals))
growf <- growvals[grownames] # just those ones we can grow

puf2017 <- puf %>%  
  select(RECID, wt, MARS, XTOT, E01500, E02100, grownames)
p2 <- puf2017
p2[, grownames] <- t(t(p2[, grownames]) * growf) # double transpose is fast (https://stackoverflow.com/questions/3643555/multiply-rows-of-matrix-by-vector)

puf2017[1:10, 1:10]
p2[1:10, 1:10]

puf2017 <- p2 %>%
  mutate(wt_2017=wt * grow_factors$grow_factor[grow_factors$pufvname=="N1_odd"])

var <- "E00600"
tmp <- puf %>% select(RECID, value=var) %>%
  left_join(puf2017 %>% select(RECID, value=var), by="RECID") %>%
  mutate(grow=value.y / value.x)
tmp

# note that E01000 minimum needs to be limited to negative 3000

comp <- bind_rows(puf %>% select(intersect(names(puf), names(puf2017))) %>% mutate(ftype="puf"),
                  puf2017 %>% mutate(wt=wt_2017) %>% select(intersect(names(puf), names(puf2017))) %>% mutate(ftype="puf2017"))
ns(comp)

tab <- comp %>%
  select(-MARS, -RECID) %>%
  pivot_longer(-c(ftype, wt)) %>%
  group_by(ftype, name) %>%
  summarise(value=sum(value * wt) / 1e6)
tab %>%
  pivot_wider(names_from=ftype) %>%
  mutate(diff=puf2017 - puf, pdiff=diff / puf * 100, apdiff=abs(pdiff)) %>%
  arrange(-apdiff)


#****************************************************************************************************
#                prepare puf for Tax-Calculator and run the tc CLI ####
#****************************************************************************************************
#.. define the Windows command to call the tc CLI ####
# This is an excerpt from a function I wrote
# Build a Windows system command that will call the Tax-Calculator CLI. See:
#   https://pslmodels.github.io/Tax-Calculator/
# CAUTION: must use full dir names, not relative to working directory
# CAUTION: any directory names that have spaces in them must be shQuoted
# CAUTION: when I updated Anaconda most recently, I had to add 
#   C:\Users\donbo\Anaconda3\Library\bin to the system path for Tax-Calculator to work with the system(cmd) approach
# CAUTION: 2013 is the FIRST possible tax year that Tax-Calculator will do

# Here is the tc CLI usage: 
# tc INPUT TAXYEAR [--help]
# [--baseline BASELINE] [--reform REFORM] [--assump  ASSUMP]
# [--exact] [--tables] [--graphs]
# [--dump] [--dvars DVARS] [--sqldb] [--outdir OUTDIR]
# [--test] [--version]  

globals

puf2017tc <- puf2017 %>%
  setNames(change_case(names(.))) %>% # Tax-Calculator expects mostly lower-case names
  do(impose_variable_rules(.)) %>% # not needed for synpuf5 and later
  do(prime_spouse_splits(.))

system.time(puf2017tc %>% write_csv(paste0(globals$statedir, "puf2017tc.csv")))

# tmp <- puf2017tc %>% select(RECID, e01500, e01700)


dvars <- c("c00100", "c62100", "c09600", "c05800", "taxbc")
dvars_path <- paste0(globals$statedir, "dumpvars.txt") # "D:/tax_data/tc_testfiles/dumpvars.txt"
cat(dvars, file=dvars_path, sep=" ") # write the dvars file

cmd1 <- "C:/ProgramData/Anaconda3/Scripts/tc"
args <- c(shQuote(paste0(globals$statedir, "puf2017tc.csv")), 2017,
          "--dump",
          "--dvars", shQuote(dvars_path),
          "--outdir", shQuote(globals$statedir))

args <- c(shQuote(paste0(globals$statedir, "puf2017tc.csv")), 2017,
          "--dump",
          "--outdir", shQuote(globals$statedir))

cmd1
args

#.. run the command ----
a <- proc.time()
system2(cmd1, args) # CAUTION: this will overwrite any existing output file that was based on the same input filename!
b <- proc.time()
b - a  # it can easily take 5-10 minutes depending on the size of the input file


# system.time(tmp <- read_csv(paste0(globals$statedir, "puf2017tc-17-#-#-#.csv")))
# system.time(tmp %>% write_csv(paste0(globals$statedir, "test.csv")))

# add the calculated variables to puf2017
tcout <- read_csv(paste0(globals$statedir, "puf2017tc-17-#-#-#.csv"))

sum(tcout$c00100) # 86,486,641,590
sum(tcout$c00100) # 86,486,641,590

f <- function(var){
  c(sum(puf[[str_to_upper(var)]]), 
    sum(puf2017[[str_to_upper(var)]]), 
    sum(tcout[[var]]))
}
f("e00200")


puf2017_full <- puf2017 %>%
  left_join(tcout, by="RECID")
glimpse(puf2017_full)


sum(puf$wt) / 1e6
sum(puf$wt * puf$E00100) / 1e9
sum(puf$wt * puf$E00200) / 1e9
sum(puf$wt * puf$E05800) / 1e9

sum(puf2017$wt * puf2017$E00200) / 1e9

sum(puf2017_full$wt) / 1e6
sum(puf2017_full$wt * puf2017_full$c00100) / 1e9
sum(puf2017_full$wt * puf2017_full$E00200) / 1e9
sum(puf2017_full$wt * puf2017_full$taxbc) / 1e9

sum(puf2017_full$wt_2017) / 1e6
sum(puf2017_full$wt_2017 * puf2017_full$c00100) / 1e9
sum(puf2017_full$wt_2017 * puf2017_full$E00200) / 1e9
sum(puf2017_full$wt_2017 * puf2017_full$taxbc) / 1e9

# 2011, 2017 per HT2;     per puf, puf2017 w/puf wts, puf2017 w/2017wts
# returns 146,455,970,   152,455,900;      145.162, 145.162, 151.1089

# AGI 8,378,794,024,    10,991,386,516 31.2%;    8293, 8774.591, 9134.064 10.1% maybe negative numbers are the problem???
# AGI taxcalc with no growthfactors applied, but only our growth variables - is this the problem
# or is it growth of negative numbers??   7428.786 2017 agi concept 2011 income levels 
# vs puf 2011 agi concept 2011 income levels 8293 so I am missing some key variables??

# wages 6,072,880,934,  7,557,396,023 24.4% gf18.7%;     6044.771, 7177.065, 7471.091  23.6%
# taxbc 1,125,358,637,  1,662,439,370 47.7%;     1099.319, 1279.374, 1331.787 21.1%



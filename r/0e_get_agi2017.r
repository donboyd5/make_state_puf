
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
puf2017 <- readRDS(paste0(globals$statedir, "puf2017_unweighted.rds"))

puf2017tc <- puf2017 %>%
  setNames(change_case(names(.))) %>% # Tax-Calculator expects mostly lower-case names
  do(impose_variable_rules(.)) %>% # not needed for synpuf5 and later
  do(prime_spouse_splits(.))

system.time(puf2017tc %>% write_csv(paste0(globals$statedir, "puf2017tc.csv")))

# tmp <- puf2017tc %>% select(RECID, e01500, e01700)


# note: I include e00200 in the output variables because that is an input variable also, and we can compare
# to the input file to make sure tc did not apply any growth factors to it
dvars <- c("e00200", "c00100", "c62100", "c09600", "c05800", "taxbc")
dvars_path <- paste0(globals$statedir, "dumpvars.txt") # "D:/tax_data/tc_testfiles/dumpvars.txt"
cat(dvars, file=dvars_path, sep=" ") # write the dvars file

cmd1 <- "C:/ProgramData/Anaconda3/Scripts/tc"
args <- c(shQuote(paste0(globals$statedir, "puf2017tc.csv")), 2017,
          "--dump",
          "--dvars", shQuote(dvars_path),
          "--outdir", shQuote(globals$statedir))

# args <- c(shQuote(paste0(globals$statedir, "puf2017tc.csv")), 2017,
#           "--dump",
#           "--outdir", shQuote(globals$statedir))

cmd1
args

#.. run the command ----
a <- proc.time()
system2(cmd1, args) # CAUTION: this will overwrite any existing output file that was based on the same input filename!
b <- proc.time()
b - a  # it can easily take 5-10 minutes depending on the size of the input file

# NOTE: 12/15/2019 tc says:
# Your data include the following unused variables that will be ignored:
# xocawh
# xocah
# xopar
# e03260
# FDED
# xoodep

# system.time(tmp <- read_csv(paste0(globals$statedir, "puf2017tc-17-#-#-#.csv")))
# system.time(tmp %>% write_csv(paste0(globals$statedir, "test.csv")))

# add the calculated variables to puf2017
tcout <- read_csv(paste0(globals$statedir, "puf2017tc-17-#-#-#.csv"))

# compare unweighted sums
sum(tcout$c00100) # 131,342,790,041 # $131b unwtd agi
sum(tcout$e00200) # 42,691,443,033 # $42b unwtd wages

# now we are ready to put calculated variables on the ORIGINAL file (before name changes, etc.)
puf2017_full <- puf2017 %>%
  left_join(tcout %>% select(RECID, c00100, c62100, c09600, c05800, taxbc), by="RECID")
glimpse(puf2017_full)

# lastly, calculate simple weights for the file that hit total agi that we expect
agitarget <- readRDS(here::here("data", paste0("hist2_targets", 2017, ".rds"))) %>%
  filter(stabbr=="US", AGI_STUB==0, h2vname=="A00100") %>%
  .$target * 1000  # 10,991,386,516 # convert from thousands to dollars

agicalc <- puf2017_full %>%
  summarise(agicalc=sum(S006 / 100 * c00100)) %>%
  .$agicalc

(wt_ratio <- agitarget / agicalc)

# now we can calculate crude 2017 US weights for the file and save
puf2017_weighted <- puf2017_full %>%
  mutate(wtus_2017=S006 / 100 * wt_ratio)

# verify that we hit the 2017 US agi target
sum(puf2017_weighted$wtus_2017 * puf2017_weighted$c00100) - agitarget

# save the file
saveRDS(puf2017_weighted, paste0(globals$statedir, "puf2017_weighted.rds"))



# just out of curiosity, what do wages look like compared to SOI Historical 2 wages
(wages_file <- sum(puf2017_weighted$wtus_2017 * puf2017_weighted$E00200))
(wages_target <- readRDS(here::here("data", paste0("hist2_targets", 2017, ".rds"))) %>%
    filter(stabbr=="US", AGI_STUB==0, h2vname=="A00200") %>%
    .$target * 1000)
wages_target / wages_file

# 2011, 2017 per HT2;     per puf, puf2017 w/puf wts, puf2017 w/2017wts
# returns 146,455,970,   152,455,900;      145.162, 145.162, 151.1089

# AGI 8,378,794,024,    10,991,386,516 31.2%;    8293, 8774.591, 9134.064 10.1% maybe negative numbers are the problem???
# AGI taxcalc with no growthfactors applied, but only our growth variables - is this the problem
# or is it growth of negative numbers??   7428.786 2017 agi concept 2011 income levels 
# vs puf 2011 agi concept 2011 income levels 8293 so I am missing some key variables??

# wages 6,072,880,934,  7,557,396,023 24.4% gf18.7%;     6044.771, 7177.065, 7471.091  23.6%
# taxbc 1,125,358,637,  1,662,439,370 47.7%;     1099.319, 1279.374, 1331.787 21.1%



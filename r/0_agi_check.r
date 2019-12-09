
# 12/2/2019
# why is calculated agi so far from reported agi in the puf?
# I compared reported 2011 agi and calculated 2013 agi
# I calculated agi for 2013 because that is the first year of tax-calculator
# web sources suggest definitional changes in agi between 2011 and 2013 were minimal, so this should not be a problem

# results:
# correlation is only 0.9172 between reported 2011 agi and calculated 2013 agi
# here are diffs and % diffs by marital status
# ====  ======  ======  =====  =====
#   MARS  E00100  c00100   diff  pdiff
# ====  ======  ======  =====  =====
# 1  2076.0  2128.0   52.0    2.5
# 2  5371.6  5508.2  136.6    2.5
# 3   128.9   139.5   10.6    8.2
# 4   716.6   720.8    4.2    0.6
# ====  ======  ======  =====  =====
# 
# I looked at some recs with large positive or negative differences between reported and calculated agi
# It looks like the file includes reported agi that simply cannot be reproduced from details on the file

# conclusions and solutions:
# I will plan to use calculated agi (for 2017) and treat this as a weighting problem, and 
# get more returns as needed;
# not sure what else to do

# I'm thinking I should only include the variables from our synthesized files in the growfactors.


# get the 2011 puf
puf2011 <- get_puf_xagg() 

# prepare it for tax calculator
# https://pslmodels.github.io/Tax-Calculator/uguide.html
puf2011tc <- puf2011 %>%
  setNames(change_case(names(.))) %>% # Tax-Calculator expects mostly lower-case names
  do(impose_variable_rules(.)) %>% # not needed for synpuf5 and later
  do(prime_spouse_splits(.))
ns(puf2011tc)

# save it as input for tc
puf2011tc %>% write_csv(paste0(globals$statedir, "puf2011tc.csv"))

# run it through tc using 2013 LAW -- the first year

dvars <- c("c00100", "c62100", "c09600", "c05800", "taxbc")
dvars_path <- paste0(globals$statedir, "dumpvars.txt") # "D:/tax_data/tc_testfiles/dumpvars.txt"
cat(dvars, file=dvars_path, sep=" ") # write the dvars file

cmd1 <- "C:/ProgramData/Anaconda3/Scripts/tc"
args <- c(shQuote(paste0(globals$statedir, "puf2011tc.csv")), 2013,
          "--dump",
          "--dvars", shQuote(dvars_path),
          "--outdir", shQuote(globals$statedir))

args <- c(shQuote(paste0(globals$statedir, "puf2011tc.csv")), 2013,
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

# add the calculated variables to puf2011
tcout <- read_csv(paste0(globals$statedir, "puf2011tc-13-#-#-#.csv"))

puf2011_2013tc <- puf2011 %>%
  left_join(tcout %>% select(RECID, setdiff(names(tcout), names(puf2011))), by="RECID")
glimpse(puf2011_2013tc)
ns(puf2011_2013tc)
saveRDS(puf2011_2013tc, paste0(globals$statedir, "puf2011_2013tc.rds"))


agi <- puf2011_2013tc %>%
  select(RECID, MARS, wt, E00100, c00100) %>%
  mutate(diff=c00100 - E00100)
glimpse(agi)

cor(agi[, c("E00100", "c00100")]) # .9172

agi %>% 
  group_by(MARS) %>%
  summarise_at(vars(E00100, c00100, diff), list(~sum(. * wt) / 1e9)) %>%
  mutate(pdiff=diff / E00100 * 100) %>%
  kable(format="rst", digits=1)

bigdiffs <- agi %>%
  arrange(-abs(diff)) %>%
  filter(row_number()<=100)

puf2011_2013tc %>%
  filter(RECID==409618) %>%
  write_csv(paste0(globals$tcdir, "check.csv"))









# df <- readr::read_csv("D:/tcdir/state_puf/puf2017tc-17-#-#-#.csv")
# dim(df)
# 
# s <- proc.time()
# readr::write_csv(df, "D:/tcdir/state_puf/test2.csv")
# f <- proc.time()
# 
# print(f - s)

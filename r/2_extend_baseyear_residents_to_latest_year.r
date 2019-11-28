
#****************************************************************************************************
#                Libraries and globals ####
#****************************************************************************************************
source(file.path(PROJHOME, "r/includes", "libraries.r"))
source(file.path(PROJHOME, "r/includes", "globals.r"))

source(file.path(PROJHOME, "r/includes", "functions_general.r"))
source(file.path(PROJHOME, "r/includes", "functions_target_setup_and_analysis.r"))
source(file.path(PROJHOME, "r/includes", "functions_ipopt.r"))

source(file.path(PROJHOME, "r/includes", "functions_state_puf.r"))

# source("./r/includes/libraries.r")
# source("./r/includes/globals.r")
# 
# source("./r/includes/functions_general.r")
# source("./r/includes/functions_target_setup_and_analysis.r")
# source("./r/includes/functions_ipopt.r")
# 
# source("./r/includes/functions_state_puf.r")


#****************************************************************************************************
#                DEFINE STATE TO ADJUST ####
#****************************************************************************************************

state <- "NY"

#****************************************************************************************************
#                Get data files ####
#****************************************************************************************************
#.. puf data and variable names ----
(stage1fn <- paste0(globals$statedir, state, "/puf_2011_stage1_", state, ".rds"))
puf2011 <- readRDS(stage1fn) # note that this has all records, and has wt variable
glimpse(puf2011)
ht(puf2011 %>% select(RECID, stabbr, ftype, wt_puf, wt_ratio, wt_stage1, wt))
quantile(puf2011$wt, probs=c(0, .01, .05, .10, .25, .5, .75, .9, .95, .99, 1)) %>% round(3)
puf2011 %>%
  summarise(nlt1=sum(wt<1), nlt1pct=nlt1 / n() * 100, 
            wtlt1=sum(wt * (wt<1)), wtlt1pct=wtlt1 / sum(wt) * 100,
            e00100sumb=sum(wt * e00100) / 1e9, e00100lt1=sum(wt * e00100 * (wt >= 1)) / 1e9) %>%
  mutate(e00100pct=e00100lt1 / e00100sumb * 100)

puf.vnames <- get_puf_vnames()

#.. inclink -- income ranges linkage file ----
(inclink <- get_income_ranges(2011))


#****************************************************************************************************
#                Get Historical Table 2 Targets for 2011 base year and for 2016 target year ####
#****************************************************************************************************
#.. targets from Historical Table 2 ----
hist2_targets <- get_hist2_targets(2011, state)
glimpse(hist2_targets)
hist2_targets  %>% ht


hist2_latest_raw <- read_csv(paste0("./data/SOI_Historical_Table_2/", "16in54cmcsv.csv"))
hist2_latest <- hist2_latest_raw %>%
  set_names(., str_to_lower) %>%
  gather(vname, target, -state, -agi_stub)
glimpse(hist2_latest)
ht(hist2_latest)
unique(hist2_latest$vname)


#****************************************************************************************************
#                STAGE 1: Adjust average values on returns, based on per-return growth rates between 2011 and 2016 ####
#****************************************************************************************************





#****************************************************************************************************
#                Calculate per-return growth rates between 2011 and 2016 ####
#****************************************************************************************************


#****************************************************************************************************
#                Use Tax-Calculator to get 2016 AGI, taxable income, and other items ####
#****************************************************************************************************



# STAGE 2 ####




#****************************************************************************************************
#                Determine per-return growth rates for major items ####
#****************************************************************************************************


#****************************************************************************************************
#                Run Tax-Calculator to get calculated variables such as agi ####
#****************************************************************************************************


#****************************************************************************************************
#                Run ipoptr to get new weights ####
#****************************************************************************************************


#****************************************************************************************************
#                Examine results and save ####
#****************************************************************************************************




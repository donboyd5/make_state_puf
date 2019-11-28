# 5/18/2019

# Construct a preliminary set of new weights for the 2011 puf for each of the 50 states.
# These weights will hit the correct number of returns reported in SOI Historical Table 2
# for 2011, for each Table 2 agi range by each Table 2 marital status (married joint and all other).

# We do this by creating a simple ratio adjustment factor for each state, agi range, and marital
# status group.

# The resulting weights get the correct number of returns for each state (and agi range and marital status)
# but many other variables will not be correct, since all this does is make a state look like the
# national average, within agi range and marital status. These weights are a starting point for
# later adjustments to weights.

# Save these 50 sets of preliminary weights in ratio_weights.rds, in an external directory.


#****************************************************************************************************
#                Libraries and globals ####
#****************************************************************************************************
source("./r/includes/libraries.r")
source("./r/includes/globals.r")

source("./r/includes/functions_general.r")
source("./r/includes/functions_state_puf.r")


#****************************************************************************************************
#                get and save data ####
#****************************************************************************************************


#****************************************************************************************************
#                ONETIME: Prepare a file with RECID and ratio-adjusted weights for EACH state ####
#****************************************************************************************************
#.. puf data and variable names ----
# puf <- readRDS(paste0(globals$tc.dir, "puf_lc.rds")) # note that this has all records, and has wt variable
puf <- get_puf_xagg() # ORIGINAL PUF less the 4 aggregate records
glimpse(puf)

pufwts <- puf %>%
  mutate(agi_group=get_agi_group(E00100, 2011),
         wt=S006) %>%
  group_by(agi_group) %>%
  summarise(n=n(), nret=sum(wt), nret_joint=sum(wt * (MARS==2))) %>%
  mutate(nret_other=nret - nret_joint, incgrp=paste0("inc", as.numeric(agi_group)))
pufwts

get_rweights <- function(stabbr){
  # get target weights for the state
  hist2_targets <- get_hist2_targets(2011, stabbr)
  
  targwts <- hist2_targets %>%
    filter(lineno %in% 1:2, incgrp!="inc0") %>%
    mutate(vname=ifelse(lineno==1, "nret", "nret_joint")) %>%
    select(incgrp, vname, target) %>%
    spread(vname, target) %>%
    mutate(nret_other=nret - nret_joint)
  
  # get weight adjustment factors for the state
  wtfactors <- bind_rows(pufwts %>% select(incgrp, nret_joint, nret_other) %>% mutate(ftype="puf"),
                         targwts %>% select(incgrp, nret_joint, nret_other) %>% mutate(ftype="target")) %>%
    gather(rtype, value, -incgrp, -ftype) %>%
    spread(ftype, value) %>%
    mutate(wtfactor=target / puf)
  
  # get a data frame with RECID, stabbr, wt_ratio
  ratio_weight <- puf %>%
    select(RECID, e00100, MARS, wt) %>%
    mutate(agi_group=get_agi_group(e00100, 2011),
           incgrp=get_incgrp(agi_group),
           rtype=ifelse(MARS==2, "nret_joint", "nret_other")) %>%
    left_join(wtfactors %>% select(incgrp, rtype, wtfactor),
              by = c("incgrp", "rtype")) %>%
    mutate(stabbr=stabbr,
           wt_puf=wt,
           wt_ratio=wt_puf * wtfactor) %>%
    select(RECID, stabbr, wt_ratio)
  return(ratio_weight)
}

df <- get_rweights("IL")
df2 <- get_rweights("AL")
glimpse(df)
glimpse(df2)

stabbr <- "IL"

system.time(rweights <- ldply(state.abb, get_rweights, .progress = "text")) # about 30 seconds
ht(rweights)
count(rweights, stabbr)
rweights %>%
  group_by(stabbr) %>%
  slice(1:3) %>%
  spread(stabbr, wt_ratio)

saveRDS(rweights, paste0(globals$tc.dir, "state_puf/", "ratio_weights.rds"))




# 0 = No AGI Stub
# 1 = ‘Under $1’
# 2 = '$1 under $10,000'
# 3 = '$10,000 under $25,000'
# 4 = '$25,000 under $50,000'
# 5 = '$50,000 under $75,000'
# 6 = '$75,000 under $100,000'
# 7 = '$100,000 under $200,000'
# 8 = ‘$200,000 under $500,000’
# 9 = ‘$500,000 under $1,000,000’
# 10 = ‘$1,000,000 or more’

# N1	Number of returns
# MARS1	Number of single returns
# MARS2	Number of joint returns
# MARS4	Number of head of household returns

target_state <- "NY"

weight_targets <- readRDS(here::here("data", paste0("hist2_targets", 2017, ".rds"))) %>%
  filter(stabbr==target_state, AGI_STUB!=0) %>%
  filter(h2vname %in% c("N1", "MARS1", "MARS2", "MARS4")) %>%
  select(year, stabbr, AGI_STUB, h2vname, target) %>%
  pivot_wider(names_from = h2vname, values_from = target) %>%
  mutate(MARS3=N1 - MARS1 - MARS2 - MARS4) %>%
  pivot_longer(-c(year, stabbr, AGI_STUB, N1), names_to = "h2vname", values_to = "target") %>%
  arrange(AGI_STUB, h2vname)
weight_targets

# globals$agibrks$`2017`
# -Inf       1   10000   25000   50000   75000  100000  200000  500000 1000000     Inf
get_AGI_STUB <- function(agi, cuts){
  cut(agi, cuts, right=FALSE, labels=FALSE)
}
# get_AGI_STUB(0, globals$agibrks$`2017`) # 1
# get_AGI_STUB(1, globals$agibrks$`2017`) # 2
# get_AGI_STUB(9999, globals$agibrks$`2017`) # 2
# get_AGI_STUB(10e3, globals$agibrks$`2017`) # 3
# get_AGI_STUB(25e3, globals$agibrks$`2017`) # 4
# get_AGI_STUB(999999, globals$agibrks$`2017`) # 9
# get_AGI_STUB(1e6, globals$agibrks$`2017`) # 10
# get_AGI_STUB(1e99, globals$agibrks$`2017`) # 10


puf2017_weighted <- readRDS(paste0(globals$statedir, "puf2017_weighted.rds"))
ns(puf2017_weighted)
count(puf2017_weighted, MARS)

pufwts <- puf2017_weighted %>%
  select(c00100, MARS, wtus_2017) %>%
  mutate(AGI_STUB=get_AGI_STUB(c00100, globals$agibrks$`2017`),
         h2vname=factor(MARS, levels=1:4, labels=c(paste0("MARS", 1:4))) %>% as.character) %>%
  group_by(AGI_STUB, h2vname) %>%
  summarise(wtsum=sum(wtus_2017))
pufwts

pufratios <- pufwts %>%
  left_join(weight_targets, by=c("AGI_STUB", "h2vname")) %>%
  mutate(wt_ratio=target / wtsum)

puf2017_stateweights1 <- puf2017_weighted %>%
  mutate(stabbr=target_state,
         AGI_STUB=get_AGI_STUB(c00100, globals$agibrks$`2017`),
         h2vname=factor(MARS, levels=1:4, labels=c(paste0("MARS", 1:4))) %>% as.character) %>%
  left_join(pufratios %>% select(stabbr, AGI_STUB, h2vname, wt_ratio)) %>%
  mutate(wtst_2017=wt_ratio * wtus_2017)

# sum of weights on both files should be the same
sum(puf2017_stateweights1$wtst_2017)
sum(weight_targets$target)

saveRDS(puf2017_stateweights1, paste0(globals$statedir, "puf2017_stateweights1.rds"))


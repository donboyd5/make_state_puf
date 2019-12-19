

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

# target_state <- "NY"

# get weight targets for all states by AGI_STUB and MARS
weight_targets <- readRDS(here::here("data", paste0("hist2_targets", 2017, ".rds"))) %>%
  # filter(stabbr==target_state, AGI_STUB!=0) %>%
  filter(AGI_STUB!=0) %>%
  filter(h2vname %in% c("N1", "MARS1", "MARS2", "MARS4")) %>%
  select(year, stabbr, AGI_STUB, h2vname, target) %>%
  pivot_wider(names_from = h2vname, values_from = target) %>%
  mutate(MARS3=N1 - MARS1 - MARS2 - MARS4) %>%
  pivot_longer(c(MARS1, MARS2, MARS3, MARS4), names_to = "MARS", values_to = "weight_target") %>%
  mutate(MARS=str_sub(MARS, 5, 5) %>% as.integer) %>%
  arrange(stabbr, AGI_STUB, MARS) %>%
  select(stabbr, AGI_STUB, MARS, weight_target)
weight_targets

# now compute weight-ratios and weights for all states
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


puf2017_weighted <- readRDS(paste0(globals$statedir, "puf2017_weighted.rds")) # this file has national weights
ns(puf2017_weighted)
count(puf2017_weighted, MARS)

weight_sums <- puf2017_weighted %>%
  select(c00100, MARS, wtus_2017) %>%
  mutate(AGI_STUB=get_AGI_STUB(c00100, globals$agibrks$`2017`)) %>%
  group_by(AGI_STUB, MARS) %>%
  summarise(weight_sum=sum(wtus_2017)) %>%
  ungroup
weight_sums

state_weight_ratios_2017 <- weight_targets %>%
  filter(stabbr != "US") %>%
  left_join(weight_sums, by=c("AGI_STUB", "MARS")) %>%
  mutate(weight_ratio=weight_target / weight_sum)
state_weight_ratios_2017 %>% filter(stabbr=="NY")
saveRDS(state_weight_ratios, paste0(globals$statedir, "state_weight_ratios_2017.rds"))

# now save record-level puf weights and ratios by state
state_ratio_adjusted_weights_2017 <- puf2017_weighted %>%
  select(RECID, c00100, MARS, wtus_2017) %>%
  mutate(AGI_STUB=get_AGI_STUB(c00100, globals$agibrks$`2017`)) %>%
  select(RECID, MARS, AGI_STUB, wtus_2017) %>%
  left_join(weight_ratios, by = c("MARS", "AGI_STUB")) %>%
  mutate(weight_state=wtus_2017 * weight_ratio) %>%
  select(RECID, AGI_STUB, stabbr, wtus_2017, weight_ratio, weight_state)
count(state_ratio_adjusted_weights_2017, stabbr)

# quick check on weights. I weighted the puf to hit the agi target and so the weights are not
# identical to the ratio adjusted weights but the differences are not large
# tmp <- state_ratio_adjusted_weights_2017 %>%
#   group_by(RECID) %>%
#   summarise(wtus_2017=mean(wtus_2017),
#             weight_state=sum(weight_state)) %>%
#   mutate(ratio=weight_state / wtus_2017)
# quantile(tmp$ratio)
# 
# tmp2 <- state_ratio_adjusted_weights_2017 %>%
#   group_by(stabbr) %>%
#   summarise(wtus_2017=mean(wtus_2017),
#             weight_ratio=sum(weight_ratio),
#             weight_state=sum(weight_state))
# sum(tmp2$weight_state)
# sum(puf2017_weighted$wtus_2017)
# weight_targets %>%
#   group_by(stabbr=="US") %>%
#   summarise(weight=sum(weight_target))

saveRDS(state_ratio_adjusted_weights_2017, paste0(globals$statedir, "state_ratio_adjusted_weights_2017.rds"))


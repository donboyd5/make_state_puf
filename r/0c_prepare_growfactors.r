

# define variables for which we need growth factors
(needed_puf_vars <- readRDS(here::here("data", "needed_puf_vars.csv")))

# define the subset of the needed_puf_vars for which we will want growth factors
# we will take the nongrowth_vars as is, or adjust outside of this process
# these variables tend to be form-filing indicators, IDs, and exemption amounts
nongrowth_vars <- c("DSI", "EIC", "F2441", "F6251", "FDED", "MARS", "MIDR", "N24", "RECID", "S006",
                    "XOCAH", "XOCAWH", "XOODEP", "XOPAR", "XTOT")
# DSI Dependent Status Indicator
# EIC Earned Income Credit Code
# F2441 Form 2441, Child Care Credit Qualified Individual
# F6251 Form 6251, Alternative Minimum Tax
# FDED Form of Deduction Code
# MARS Marital (Filing) Status
# MIDR Married Filing Separately Itemized Deductions Requirement Indicator
# N24 Number of Children for Child Tax Credit
# RECID Return(record) ID
# S006 DECIMAL WEIGHT -- we will attend to this separately, later
# XOCAH   Exemptions for Children Living at Home
# XOCAWH  Exemptions for Children Living Away from Home
# XOODEP  Exemptions for Other Dependents 
# XOPAR   Exemptions for Parents Living at Home or Away from Home
# XTOT Total Exemptions
# in the future we may adjust the exemption variables, but not now

(growth_vars <- setdiff(needed_puf_vars, nongrowth_vars))

grow_factors_base <- tibble(pufvname=growth_vars) %>%
  left_join(get_puf_vnames() %>% select(pufvname=vname_puf, puf_desc=vdesc)) %>%
  # our starting assumption is that we will match each puf variable to the base_name vtype given below
  mutate(base_name=str_sub(pufvname, 2, -1),
         vtype="average")

# get the SOI growth ratios and match them to growth vars where appropriate
soi_growth_ratios <- readRDS(here::here("data", "growthratios_2011to2017.rds"))

# repeat these next 3 steps, adding base_name adjustments with case_when until all growth_vars have a grow_factor value
# 1.
grow_factors_adjusted <- grow_factors_base %>%
  mutate(base_name=case_when(pufvname %in% c("E00400") ~ "00300", # tax-exempt interest gets taxable interest growth
                             pufvname %in% c("E00800", "E03500") ~ "00200", # alimony gets wage growth
                             pufvname %in% c("E01500") ~ "01700",  # pensions
                             
                             # gains growth
                             pufvname %in% c("E01100", "E01200", "E24515", "E24518",
                                             "P22250", "P23250") ~ "01000",
                             pufvname %in% c("E02400", "E09800") ~ "02500", # social security
                             pufvname %in% c("E02000") ~ "00900", # business to partnership net income or loss
                             pufvname %in% c("E02100", "E27200") ~ "00900", # farm income - no good mapping for now
                             
                             # tax deductions - can be improved upon
                             pufvname %in% c("E18400") ~ "18425",
                             TRUE ~ base_name))
# 2.
grow_match <- grow_factors_adjusted %>%
  left_join(soi_growth_ratios %>% select(base_name, base_desc, vtype, growth_ratio), by = c("base_name", "vtype"))

# 3. 
grow_match %>%
  filter(is.na(growth_ratio))
# go back up and revise grow_vactors_adjusted

soi_growth_ratios %>% filter(str_detect(base_desc, coll("tax", ignore_case = TRUE)), vtype=="average")

# SHORT TERM FIX: any still unmatched vars get agi growth rate
(mismatch_vars <- grow_match %>% filter(is.na(growth_ratio)) %>% .$pufvname)
(grow_factor_default <- soi_growth_ratios %>% filter(base_name=="00100", vtype=="average"))
grow_factors_final <- grow_match %>%
  mutate(grow_factor=growth_ratio,
         grow_factor=ifelse(is.na(grow_factor), grow_factor_default$growth_ratio, grow_factor))

saveRDS(grow_factors_final, here::here("data", "grow_factors_final_2011to2017.rds"))



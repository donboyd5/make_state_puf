
# This subprogram gets the national-level per-return growth rates of
# SOI Historical Table 2 items from 2011 to 2017

history_raw <- readRDS(here::here("data", "hist2_targets2011.rds")) %>%
  filter(stabbr=="US", AGI_STUB==0)

future_raw <- readRDS(here::here("data", paste0("hist2_targets", 2017, ".rds"))) %>%
  filter(stabbr=="US", AGI_STUB==0)

# ***** TEMPORARY FIX **** ----
# neither file has a variable called N00100, the number of returns with AGI
# however, it should be close to N02650 number of returns with total income
# so for now, create N00100 that equals this variable
# it is not a major issue, but it is a convenience to have N00100 available
history <- bind_rows(history_raw,
                     history_raw %>%  filter(h2vname=="N02650") %>%
                       mutate(h2vname="N00100", 
                              table_desc="Number of returns with AGI (estimated as N02650)",
                              lineno=NA_real_))

future <- bind_rows(future_raw,
                    future_raw %>%  filter(h2vname=="N02650") %>%
                      mutate(h2vname="N00100", 
                             table_desc="Number of returns with AGI (estimated as N02650)"))

# ***** END TEMPORARY FIX **** ----


# create a single set of variable descriptions for the two files, favoring the future when available, otherwise using history
var_table <- history %>%
  select(h2vname, table_desc) %>%
  full_join(future %>% select(h2vname, table_desc), by="h2vname",
            suffix=c(".history", ".future")) %>%
  mutate(table_desc=ifelse(is.na(table_desc.future), table_desc.history, table_desc.future))

# which variables are not in both files?
# we will not extrapolate the missing variables in this stage - that is a job for TaxData in the future
(mismatch_vars <- setdiff_all(history$h2vname, future$h2vname))
var_table %>%
  filter(h2vname %in% mismatch_vars) %>%
  select(-table_desc)

# look at the common vars
common_vars <- intersect(history$h2vname, future$h2vname)
common_vars %>% sort
var_table %>%
  filter(h2vname %in% common_vars) %>%
  select(-table_desc)

# define variables that do not fall into the SOI N-A framework (Number of returns, Amount of money)
oddvars2011 <- c("MARS2", "N1", "N2", "NUMDEP", "PREP", "SCHF")
oddvars2017 <- c("CPREP", "DIR_DEP", "ELDERLY", "ELF", "MARS1", "MARS2", "MARS4", 
                 "MVITA", "NUMDEP", "PREP", "RAC", "SCHF", "TCE", "TOTAL_VITA", "VITA", "VITA_EIC")
(oddvars <- c(oddvars2011, oddvars2017) %>% unique %>% sort)
treat_specially <- intersect(common_vars, oddvars)

var_table %>%
  filter(h2vname %in% treat_specially) %>%
  select(-table_desc)

# create base variable name and description for each item that will be the same, regardless of whether the value is 
# number of returns with an item, the amount of the item or (later), the average per-return amount of the item
var_table2 <- var_table %>%
  filter(h2vname %in% common_vars) %>%
  mutate(vtype=case_when(h2vname %in% treat_specially ~ "number_special",
                         str_sub(h2vname, 1, 1)=="A" ~ "amount",
                         str_sub(h2vname, 1, 1)=="N" ~ "number",
                         TRUE ~ "ERROR"),
         base_name=ifelse(vtype %in% c("amount", "number"), 
                          str_sub(h2vname, 2, -1), h2vname)) %>%
  group_by(base_name) %>%
  mutate(base_desc=ifelse(n()==1, table_desc, table_desc[vtype=="amount"]),
         base_desc=str_remove(base_desc, "amount") %>% str_trim)
unique(var_table2$h2vname) %>% sort
# good, now we have a complete dictionary of names for SOI Historical 2 variables that are in both 2011 and 2017


# stack the common variables for the 2 years and define whether they are an amount or a number
# get averages for those variables that have both amount and a number

# stack the SOI targets for the 2 years
stack <- bind_rows(history, future) %>%
  filter(h2vname %in% common_vars) %>%
  select(year, h2vname, target) %>%
  left_join(var_table2 %>% select(h2vname, vtype, base_name, base_desc), by="h2vname")
summary(stack) # good, no NAs

# get averages for those variables for which we have amount and number
# create a base description for these variables that is the same, whether it is amount, number, or average
averages <- stack %>%
  select(-h2vname) %>%
  filter(vtype %in% c("amount", "number")) %>%
  pivot_wider(names_from = vtype, values_from = target) %>%
  mutate(average=amount / number,
         vtype="average") %>%
  select(year, vtype, base_name, base_desc, target=average)
summary(averages) # 0 NAs because of the agi number fix
# averages %>% filter(is.na(target)) # agi average is NA because we don't have the number of returns with AGI

# now we can compute growth rates of all items
stack_all <- bind_rows(stack, averages)
summary(stack_all) # good, no NAs
ht(stack_all)
count(stack_all, vtype)

stack_all %>% select(base_name, base_desc) %>% unique

growth_ratios <- stack_all %>%
  select(year, base_name, base_desc, vtype, target) %>%
  pivot_wider(names_from = year, values_from = target) %>%
  mutate(growth_ratio=`2017` / `2011`)
summary(growth_ratios)

growth_ratios %>%
  arrange(growth_ratio) %>%
  ht

growth_ratios %>%
  filter(vtype=="average") %>%
  arrange(growth_ratio) %>%
  ht

saveRDS(growth_ratios, here::here("data", "growthratios_2011to2017.rds"))

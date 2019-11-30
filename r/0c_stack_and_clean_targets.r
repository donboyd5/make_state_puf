

#****************************************************************************************************
#                Income ranges ####
#****************************************************************************************************
get_agi_group <- function(agi, year){
  if(year==2011) agibrks <- globals$agibrks_hist2_2011 else
    if(year==2016) agibrks <- globals$agibrks_hist2_2016 
    
    agi_group=cut(agi, agibrks, right=FALSE)
    return(agi_group)
}

get_incgrp <- function(agi_group) {paste0("inc", as.numeric(agi_group))}


get_income_ranges <- function(year) {
  # return a data frame that defines income ranges that SOI uses in Historical Table 2
  # for a year we care about
  if(year==2011) agibrks <- globals$agibrks_hist2_2011 else
    if(year==2016) agibrks <- globals$agibrks_hist2_2016 else
      if(year==2017) agibrks <- globals$agibrks_hist2_2017 
      
      ifactor <- cut(agibrks, agibrks, right=FALSE)
      # ifactor
      # levels(ifactor)
      scale_commak <- function(value) ifelse(value < 1000, scales::comma(value), paste0(scales::comma(value / 1000), "k"))
      
      inclink <- tibble(year=year, 
                        inum=1:(length(agibrks) - 1),
                        imin_ge=agibrks[inum],
                        imax_lt=agibrks[inum + 1], 
                        incgrp=paste0("inc", inum), 
                        agi_group=levels(ifactor)[-length(ifactor)]) %>%
        mutate(agi_label=case_when(
          is.infinite(imin_ge) ~ paste0("< ", scale_commak(imax_lt)),
          (imin_ge >= 0) & (imax_lt < Inf) ~ paste0(">= ", scale_commak(imin_ge), " to ", scale_commak(imax_lt)),
          is.infinite(imax_lt) ~ paste0(">= ", scale_commak(imin_ge)),
          TRUE ~ "ERROR"))
      
      return(inclink)
}



#****************************************************************************************************
#                Fix and improve targets ####
#****************************************************************************************************
get_saltfix <- function(xwalk, hist2_values){
  # the SALT variables in the 2011 SOI data do not mesh with those in the PUF
  
  # PUF has:
  #  - E18400 State and local taxes (Income or sales taxes) [$281b]
  #  - E18500 Real estate tax deductions [$173b]
  #  - E18600 Motor Vehicle Tax (Other taxes) [$15m]
  #  parens indicate that the variable is called 2 different things in the puf documentation
  #  where the total would be E18400 + E18500 + E18600 
  #  - E18600 is 0.0% of this total
  
  # SOI 2011:
  #  - has pit (line 53) and sales (line 55) separately but puf has them combined
  #  - sales is only about 6.5% of PIT
  #  - has real estate (line 57)
  #  - does not have an "other" category 
  #  - has total (line 59)
  #  - pit + sales + real estate is 97.7% of total
  
  # SOI 2017:
  #  - has pit (A18425, N18425) and sales (A18450, N18450) separately but puf has them combined
  #  - sales is only about 5.4% of PIT
  #  - has real estate (A18500, N18500)
  #  - has personal property taxes (I suspect this includes MV tax) (A18800, N18800)
  #  - does not have an "other" category 
  #  - has total  (A18300, N18300)
  #  - pit + sales + real estate + personal property is is 99.2% of total
  
  # For 2011 we construct:
  # -- E18400 = (income + sales)
  # -- other -- E18600 = (total - income - sales - real estate)
  
  # For 2017 we construct:
  # -- E18400 = (income + sales)
  # -- other -- E18600 = (total - income - sales - real estate - personal property)
  
  
  # fix salt
  # saltvars <- c("e18400_pit_n", "e18400_pit", "e18400_sales_n", "e18400_sales", "e18500_n", "e18500", "e18taxes_n", "e18taxes")
  saltvars <- c("E18400_pit_n", "E18400_pit", "E18400_sales_n", "E18400_sales", "E18500_n", "E18500", "E18taxes_n", "E18taxes")
  saltvars <- c("E18400_pit_n", "E18400_pit", "E18400_sales_n", "E18400_sales", "E18500_n", "E18500", "E18taxes_n", "E18taxes")
  saltfix <- xwalk %>%
    filter(h2vname %in% saltvars) %>%
    select(lineno, h2vname) %>%
    left_join(hist2_values %>% select(-table_desc), by="lineno") %>%
    select(-lineno) %>%
    pivot_wider(names_from = h2vname, values_from = value) %>%
    mutate(E18400=E18400_pit + E18400_sales,
           E18400_n=E18400_pit_n + E18400_sales_n,
           E18600=E18taxes - E18400 - E18500) %>% # we cannot compute e1600_n - not enough information
    select(stabbr, incgrp, E18400, E18400_n, E18600) %>%
    pivot_longer(-c(stabbr, incgrp), names_to = "h2vname") %>%
    mutate(table_desc=case_when(h2vname=="E18400" ~ "State and local income or sales taxes",
                                h2vname=="E18400_n" ~ "State and local income or sales taxes: number",
                                h2vname=="E18600" ~ "State and local other taxes (motor vehicle, other)"),
           lineno=1000 + row_number() - 1)
  return(saltfix)
}


#****************************************************************************************************
#                Stack and clean targets ####
#****************************************************************************************************
stack_targets <- function(){
  # stack the targets
  # N variables are in numbers, amount variables are in $1,000s
  
  # stack income linkage:
  inc_stack <- bind_rows(get_income_ranges(2011),
                         get_income_ranges(2017))
  
  
  targets_stack <- bind_rows(
    readRDS(here::here("data", "hist2_targets2011.rds")) %>%
      select(year, stabbr, h2vname, incgrp, target),
    readRDS(here::here("data", "hist2_targets2017.rds")) %>%
      select(year, stabbr, h2vname, incgrp, target))
  targets_stack %>% filter(stabbr=="US", incgrp=="inc0", h2vname %in% c("N1", "A00100", "N00200")) %>% arrange(h2vname, year)
  
  return(stack)
}

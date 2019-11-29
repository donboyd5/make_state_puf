
#****************************************************************************************************
#               PUF manipulation ####
#****************************************************************************************************

get_puf_xagg <- function(){
  # get the 2011 puf, exclude 4 aggregate records, add a weight variable
  puf <- read_csv(paste0(globals$pufdir, "puf2011.csv"), 
                  col_types = cols(.default= col_double()), 
                  n_max=-1)
  puf <- puf %>%
    filter(MARS!=0) %>%
    mutate(wt=S006 / 100)
  return(puf)
}


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
  # for a year we care about - currently 2011 or 2016
  if(year==2011) agibrks <- globals$agibrks_hist2_2011 else
    if(year==2016) agibrks <- globals$agibrks_hist2_2016 
    
    ifactor <- cut(agibrks, agibrks, right=FALSE)
    # ifactor
    # levels(ifactor)
    scale_commak <- function(value) ifelse(value < 1000, scales::comma(value), paste0(scales::comma(value / 1000), "k"))
    
    inclink <- tibble(inum=1:(length(agibrks) - 1),
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
#                Manipulate SOI Historical Table 2 information ####
#****************************************************************************************************

get_SOI_puf_xwalk <- function(year){
  # get a crosswalk between SOI Historical Table 2 line numbers and PUF-like variable names.
  # The spreadsheet we read from, created by djb, has lower case "e" variable names, so I modify them below to upper case
  if(year==2011){
    xwalk <- read_excel(paste0("./data/", globals$xlfile), 
                        sheet = "2011_hist2_puf",
                        range="A4:D107")
    # xwalk$h2vname %>% sort
    str_sub(xwalk$h2vname, 1, 1)[str_sub(xwalk$h2vname, 1, 1)=="e"] <- "E"
    # xwalk$h2vname %>% sort
  } else xwalk <- NULL
  return(xwalk)
}


get_hist2_values <- function(year){
  # read from the previously created file that has all states
  hist2 <- read_csv("./data/hist2_2011.csv")
  return(hist2)
}


get_saltfix <- function(xwalk, hist2_values){
  # fix SALT -- construct:
  # -- E18400 = (income + sales)
  # -- other -- E18600 = (total - income - sales - real estate)
  # fix salt
  # saltvars <- c("e18400_pit_n", "e18400_pit", "e18400_sales_n", "e18400_sales", "e18500_n", "e18500", "e18taxes_n", "e18taxes")
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


get_hist2_targets_allstates <- function(year){
  xwalk <- get_SOI_puf_xwalk(year)
  inclink <- get_income_ranges(year)
  
  hist2_values <- get_hist2_values(year)
  
  saltfix <- get_saltfix(xwalk, hist2_values)
  
  vdrop <- c("nret_prep", "totinc")
  
  # note that we rename hist2 value to target
  hist2_targets <- xwalk %>%
    filter(!h2vname %in% vdrop, !is.na(h2vname)) %>%
    select(lineno, h2vname, table_desc) %>%
    left_join(hist2_values %>% select(-table_desc) %>% rename(target=value), by="lineno") %>%
    bind_rows(saltfix %>% rename(target=value)) %>% # do this here so that we get income info in next step
    left_join(inclink %>% select(incgrp, imin_ge, imax_lt), by="incgrp") %>%
    mutate(vname=ifelse(h2vname=="nret_joint", "E00100", str_remove(h2vname, "_n"))) %>%
    mutate(target=ifelse(str_detect(h2vname, "_n") |
                           str_detect(h2vname, "nret") |
                           str_detect(h2vname, "XTOT"),
                         target, target * 1000)) %>%
    mutate(imin_ge=ifelse(is.na(imin_ge), -Inf, imin_ge),
           imax_lt=ifelse(is.na(imax_lt), Inf, imax_lt),
           puf_multiplier=ifelse(str_detect(h2vname, "_n") | str_detect(h2vname, "nret"), 1, vname)) %>%
    mutate(inc_rule=paste0("E00100 >= ", imin_ge, " & ", "E00100 < ", imax_lt) %>% parens,
           # VERIFY that we should use != rather than > for the _n rule
           other_rule=case_when(h2vname=="nret_joint" ~ "(MARS==2)",
                                str_detect(h2vname, "_n") & vname!="e00100" ~ paste0(vname, " != 0") %>% parens,
                                TRUE ~ ""),
           select_rule=paste0(inc_rule,
                              ifelse(other_rule!="", 
                                     paste0(" & ", other_rule),
                                     "")) %>% parens) %>%
    mutate(calc_rule=paste0("wt * ", puf_multiplier, " * ", select_rule) %>% parens) %>%
    select(-contains("rule"), everything(), contains("rule"))
  return(hist2_targets)
}

# djb NovDec 2019 above here ----



get_hist2_values_state <- function(year, stabbr="NY"){
  # read SOI Historical Table 2 for a single state from its own SOI excel file
  # currently just gets NY for 2011
  vnames <- c("table_desc", paste0("inc", 0:9))
  df <- read_excel(paste0(globals$hist2, "11in33ny.xls"), col_names = vnames)
  # glimpse(df)
  
  firstrow <- which(df$table_desc=="NEW YORK") + 1
  lastrow <- which(str_detect(df$table_desc, coll("** - Not shown to avoid disclosure"))) - 1
  # firstrow; lastrow
  
  hist2 <- df %>%
    filter(row_number() >= firstrow, row_number() <= lastrow) %>%
    mutate(lineno=row_number()) %>%
    gather(incgrp, value, -lineno, -table_desc) %>%
    mutate(value=as.numeric(value),
           stabbr=stabbr) %>%
    select(lineno, table_desc, stabbr, incgrp, value)
  # ht(hist2)
  return(hist2)
}


get_hist2_values <- function(year, stabbr.in="NY"){
  # read from the previously created file that has all states
  hist2 <- read_csv("./data/hist2_2011.csv") %>%
    filter(stabbr==stabbr.in)
  return(hist2)
}





get_hist2_targets <- function(year, stabbr){
  xwalk <- get_SOI_puf_xwalk(year)
  inclink <- get_income_ranges(year)
  
  # hist2_values <- get_hist2_values_state(year, stabbr)
  hist2_values <- get_hist2_values(year, stabbr)
  
  saltfix <- get_saltfix(xwalk, hist2_values)
  
  vdrop <- c("nret_prep", "totinc")
  
  # note that we rename hist2 value to target
  hist2_targets <- xwalk %>%
    filter(!h2vname %in% vdrop, !is.na(h2vname)) %>%
    select(lineno, h2vname, table_desc) %>%
    left_join(hist2_values %>% select(-table_desc) %>% rename(target=value), by="lineno") %>%
    bind_rows(saltfix %>% rename(target=value)) %>% # do this here so that we get income info in next step
    left_join(inclink %>% select(incgrp, imin_ge, imax_lt), by="incgrp") %>%
    mutate(vname=ifelse(h2vname=="nret_joint", "e00100", str_remove(h2vname, "_n"))) %>%
    mutate(target=ifelse(str_detect(h2vname, "_n") |
                           str_detect(h2vname, "nret") |
                           str_detect(h2vname, "XTOT"),
                         target, target * 1000)) %>%
    mutate(imin_ge=ifelse(is.na(imin_ge), -Inf, imin_ge),
           imax_lt=ifelse(is.na(imax_lt), Inf, imax_lt),
           puf_multiplier=ifelse(str_detect(h2vname, "_n") | str_detect(h2vname, "nret"), 1, vname)) %>%
    mutate(inc_rule=paste0("e00100 >= ", imin_ge, " & ", "e00100 < ", imax_lt) %>% parens,
           # VERIFY that we should use != rather than > for the _n rule
           other_rule=case_when(h2vname=="nret_joint" ~ "(MARS==2)",
                                str_detect(h2vname, "_n") & vname!="e00100" ~ paste0(vname, " != 0") %>% parens,
                                TRUE ~ ""),
           select_rule=paste0(inc_rule,
                              ifelse(other_rule!="", 
                                     paste0(" & ", other_rule),
                                     "")) %>% parens) %>%
    mutate(calc_rule=paste0("wt * ", puf_multiplier, " * ", select_rule) %>% parens) %>%
    select(-contains("rule"), everything(), contains("rule"))
  return(hist2_targets)
}


check_targets <- function(targets, df){
  get_value <- function(target_rule, df){
    rule <- parse(text=target_rule) 
    df <- df %>%
      mutate(coeff=eval(rule)) %>%
      summarise(pufval=sum(coeff)) %>%
      mutate(calc_rule=target_rule)
    return(df)
  }
  
  pufvals <- ldply(targets$calc_rule, get_value, df, .progress = "text")
  
  comp_df <- targets %>%
    select(constraint.num, vname, agi_group, target, calc_rule, vdesc) %>%
    left_join(pufvals) %>%
    mutate(diff=pufval - target, pdiff=diff / target * 100) %>%
    select(-calc_rule, -vdesc, everything(), calc_rule, vdesc)
  
  return(comp_df)
}


get_tolerances <- function(){
  # tolerances may need some tinkering with
  # These can be overriden if desired
  # they are specified as percentages
  tolerances <- tribble(
    ~apdiff.lb, ~tol.default,
    -Inf, 0,
    0, .1,
    5, 1,
    10, 3,
    20, 5,
    50, 7.5,
    75, 10,
    100, 25,
    200, 50,
    300, Inf)
  tolerances <- tolerances %>%
    mutate(apdiff.ub=lead(apdiff.lb)) %>% 
    select(starts_with("ap"), tol.default)
  return(tolerances)
}


get_national_puf <- function() {
  # note that this has all records, and has wt variable
  readRDS(paste0(globals$tc.dir, "puf_lc.rds"))
}


get_state_ratio_weighted_puf <- function(stabbr.target){
  puf <- get_national_puf()
  ratio_weights <- readRDS(paste0(globals$tc.dir, "state_puf/", "ratio_weights.rds"))
  
  puf_state <- puf %>%
    left_join(ratio_weights %>% filter(stabbr==stabbr.target),
              by = c("RECID")) %>%
    mutate(wt_puf=wt, wt=wt_ratio)
  return(puf_state)
}



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
#                Manipulate SOI Historical Table 2 information ####
#****************************************************************************************************


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
# hist2_targets <- xwalk %>%
#   filter(!h2vname %in% vdrop, !is.na(h2vname)) %>%
#   select(lineno, h2vname, table_desc) %>%
#   left_join(hist2_values %>% select(-table_desc) %>% rename(target=value), by="lineno") %>%
#   bind_rows(saltfix %>% rename(target=value)) %>% # do this here so that we get income info in next step
#   left_join(inclink %>% select(incgrp, imin_ge, imax_lt), by="incgrp") %>%
#   mutate(vname=ifelse(h2vname=="nret_joint", "e00100", str_remove(h2vname, "_n"))) %>%
#   mutate(target=ifelse(str_detect(h2vname, "_n") |
#                          str_detect(h2vname, "nret") |
#                          str_detect(h2vname, "XTOT"),
#                        target, target * 1000)) %>%
#   mutate(imin_ge=ifelse(is.na(imin_ge), -Inf, imin_ge),
#          imax_lt=ifelse(is.na(imax_lt), Inf, imax_lt),
#          puf_multiplier=ifelse(str_detect(h2vname, "_n") | str_detect(h2vname, "nret"), 1, vname)) %>%
#   mutate(inc_rule=paste0("e00100 >= ", imin_ge, " & ", "e00100 < ", imax_lt) %>% parens,
#          # VERIFY that we should use != rather than > for the _n rule
#          other_rule=case_when(h2vname=="nret_joint" ~ "(MARS==2)",
#                               str_detect(h2vname, "_n") & vname!="e00100" ~ paste0(vname, " != 0") %>% parens,
#                               TRUE ~ ""),
#          select_rule=paste0(inc_rule,
#                             ifelse(other_rule!="", 
#                                    paste0(" & ", other_rule),
#                                    "")) %>% parens) %>%
#   mutate(calc_rule=paste0("wt * ", puf_multiplier, " * ", select_rule) %>% parens) %>%
#   select(-contains("rule"), everything(), contains("rule"))



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


get_state_ratio_weighted_puf <- function(stabbr.target){
  puf <- get_national_puf()
  ratio_weights <- readRDS(paste0(globals$tc.dir, "state_puf/", "ratio_weights.rds"))
  
  puf_state <- puf %>%
    left_join(ratio_weights %>% filter(stabbr==stabbr.target),
              by = c("RECID")) %>%
    mutate(wt_puf=wt, wt=wt_ratio)
  return(puf_state)
}



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

get_puf_vnames <- function(){
  # get data frame with PUF variables names and labels (vdesc)
  # readRDS("./data/puf.vnames.rds")
  read_csv(paste0(globals$tcdir, "pufvars.csv"))
}


#  prep file for Tax-Calculator functions ####
change_case <- function(oldnames, upper_case_vars=c("MARS", "XTOT", "DSI", "EIC", "FDED", "MIDR", "RECID")){
  # convert variable names to lower case, except for those that should
  # remain upper case
  newnames <- oldnames
  lower_case_indexes <- !newnames %in% upper_case_vars
  lower_case_names <- newnames[lower_case_indexes] %>% str_to_lower
  newnames[lower_case_indexes] <-lower_case_names
  return(newnames)
}


impose_variable_rules <- function(df){
  # Per https://pslmodels.github.io/Tax-Calculator/, Tax-Calculator requires:
  #    ordinary dividends (e00600) >= qualified dividends (e00650)
  #    total pension and annuity income (e01500) >= taxable pension and annuity income (e01700)
  
  # A few of the early synthesized files did not enforce this so we enforce it here by
  # forcing the e00600 and e01500 to be at least as large as their counterparts
  # This does NO ERROR CHECKING or reporting to let the user know of a potential problem.
  # Once we no longer use those early files we should stop running this function.
  # This should not be needed for synpuf5 and later.
  
  if("e00650" %in% names(df) & "e00600" %in% names(df)){
    df <- df %>%
      mutate(e00600=pmax(e00650, e00600))
  }
  if("e01500" %in% names(df) & "e01700" %in% names(df)){
    df <- df %>%
      mutate(e01500=pmax(e01500, e01700))
  }
  return(df)
}


prime_spouse_splits <- function(df){
  # Per https://pslmodels.github.io/Tax-Calculator/, Tax-Calculator requires the filing-unit total 
  # for each of several earnings-related variables to be split between the taxpayer and the spouse:
  #   e00200 = e00200p + e00200s  # wages
  #   e00900 = e00900p + e00900s  # business income or loss
  #   e02100 = e02100p + e02100s  # Schedule F net profit/loss
  
  # For now, create arbitrary prime-spouse splits of these variables so that we can
  # run Tax-Calculator
  prime.pct <- ifelse(df$MARS==2, .5, 1)
  spouse.pct <- ifelse(df$MARS==2, .5, 0)
  df <- df %>%
    mutate(e00200p=e00200 * prime.pct,
           e00900p=e00900 * prime.pct,
           e02100p=e02100 * prime.pct,
           
           e00200s=e00200 * spouse.pct,
           e00900s=e00900 * spouse.pct,
           e02100s=e02100 * spouse.pct)
  return(df)
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


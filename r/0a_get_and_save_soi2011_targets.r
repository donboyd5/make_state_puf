# Parse and save SOI Historical Table 2 data, which we will use as targets for the 2011 data file.

# Save the result in hist2_2011.csv in the data directory.


#****************************************************************************************************
#                functions to get and save 2011 data ####
#****************************************************************************************************
# read the data, put line numbers on it, reformat as needed
# lineno h2vname table_desc             incgrp  target imin_ge imax_lt vname puf_multiplier inc.rule        other.rule select.rule        calc.rule            
# <dbl> <chr>   <chr>                  <chr>    <dbl>   <dbl>   <dbl> <chr> <chr>          <chr>           <chr>      <chr>              <chr>                
#   1   1024 e18600  State and local other~ inc4    5.93e7   50000   75000 e186~ e18600         (e00100 >= 500~ ""         ((e00100 >= 50000~ (wt * e18600

get_soi2011_colnames <- function(globals){
  # create a vector with colnames for the 2011 SOI file
  soi_stnames_row <- read_excel(paste0(globals$hist2, "11in54cm.xlsx"), range="A3:TK3", col_names=FALSE)
  
  # put the state names into stnames, a vector -- without "Item" or NAs
  stnames <- soi_stnames_row %>% unlist %>% unname
  stnames <- stnames[!is.na(stnames)]
  stnames <- stnames[-1]
  
  # replace state names with state abbreviations
  imatch <- match(stnames, str_to_upper(state.name))
  stabbrs <- stnames
  stabbrs <- state.abb[imatch]
  stabbrs[1] <- "US"; stabbrs[10] <- "DC"; stabbrs[53] <- "OA"
  # check
  # cbind(stnames, stabbrs) # good, stabbrs is what we want

  # now create vnames - each state has incgrp 0-9 where inc0 is all returns
  inc <- paste0("inc", 0:9)
  
  # CAUTION - put stabbr in original order of our vectors
  colnamesdf <- expand_grid(stabbr=stabbrs, incgrp=inc) %>%
    mutate(stabbr=factor(stabbr, levels=stabbrs), # will keep the desired order
           stinc=paste0(stabbr, "_", incgrp)) %>%
    arrange(stabbr, incgrp)
  
  soi2011_colnames <- c("table_desc", colnamesdf$stinc)
  return(soi2011_colnames)
}

get_soi2011_targets_long <- function(globals){
  # get the 53-"state" hist2 data (states, DC, US, other areas)
  soi_data <- read_excel(paste0(globals$hist2, "11in54cm.xlsx"), 
                         range="A10:TK112", 
                         col_names=get_soi2011_colnames(globals)) %>%
    mutate(lineno=row_number()) %>%
    select(lineno, everything()) %>%
    mutate_at(vars(-lineno, -table_desc), list(~as.numeric(.)))
  
  # check
  # rows <- c(1:5, (nrow(soi_data)-4):nrow(soi_data))
  # cols <- c(1:5, (ncol(soi_data) - 4):ncol(soi_data))
  # soi_data[rows, cols]
  soi_long <- soi_data %>%
    pivot_longer(cols=-c(lineno, table_desc), names_to="vname", values_to = "value") %>%
    separate(vname, c("stabbr", "incgrp"))
  return(soi_long)
}


get_2011SOI_puf_xwalk <- function(globals){
  # get a crosswalk between SOI Historical Table 2 line numbers and PUF-like variable names.
  # The spreadsheet we read from, created by djb, has lower case "e" variable names, so I modify them below to upper case
  xwalk <- read_excel(paste0("./data/", globals$xlfile), 
                      sheet = "2011_hist2_puf",
                      range="A4:D107")
  # xwalk$h2vname %>% sort  # h2vname stands for historical Table 2 variable name
  # str_sub(xwalk$h2vname, 1, 1)[str_sub(xwalk$h2vname, 1, 1)=="e"] <- "E"
  # xwalk$h2vname %>% sort
  return(xwalk)
}


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



create_and_save_soi2011_targets <- function(globals){
  soi_long <- get_soi2011_targets_long(globals)
  xwalk <- get_2011SOI_puf_xwalk(globals)
  inclink <- get_income_ranges(2011)
  
  soi_with_names <- soi_long %>%
    left_join(xwalk, by=c("lineno")) %>%
    rename(table_desc=table_desc.y)
  
  # do some checks
  # soi_with_names %>% filter(is.na(h2vname))
  # tmp <- soi_with_names %>% filter(table_desc != table_desc.x, stabbr=="US")
  
  saltfix <- get_saltfix(xwalk, soi_with_names)
  
  soi_targets <- soi_targets %>%
    select(stabbr, lineno, h2vname, incgrp, table_desc, target) %>%
    arrange(lineno, h2vname, stabbr, incgrp) %>%
    bind_rows(saltfix %>% rename(target=value)) %>% # do this here so that we get income info in next step
  
  
  
  
  saveRDS(soi_targets, here::here("data", "hist2_targets2011.rds"))
  return(NULL)
}


# Parse and save SOI Historical Table 2 data, which we will use as targets for the 2011 data file.
# Put variable names on the file, using the names in the corresponding SOI csv files.

# Save the result in hist2_targets2011.rds in the data directory.


#****************************************************************************************************
#                functions to get and save 2011 data ####
#****************************************************************************************************
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

  # now create vnames - each state has AGI_STUB 0-9 where 0 is all returns

  # CAUTION - put stabbr in original order of our vectors
  colnamesdf <- expand_grid(stabbr=stabbrs, AGI_STUB=0:9) %>%
    mutate(stabbr=factor(stabbr, levels=stabbrs), # will keep the desired order
           stinc=paste0(stabbr, "_", AGI_STUB)) %>%
    arrange(stabbr, AGI_STUB)
  
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
    pivot_longer(cols=-c(lineno, table_desc), names_to="vname", values_to = "target") %>%
    separate(vname, c("stabbr", "AGI_STUB")) %>%
    mutate(AGI_STUB=as.integer(AGI_STUB))
  return(soi_long)
}


get_2011xls_2017csv_xwalk <- function(globals){
  # get a crosswalk between SOI Historical Table 2 line numbers and PUF-like variable names.
  # The spreadsheet we read from, created by djb, has lower case "e" variable names, so I modify them below to upper case
  xwalk <- read_excel(paste0("./data/", globals$xlfile), 
                      sheet = "2011soixls_2017soicsv_xwalk",
                      range="A4:C107")
  return(xwalk)
}


save_soi2011_targets_csvformat <- function(globals){
  soi_long <- get_soi2011_targets_long(globals)
  # xwalk <- get_2011SOI_puf_xwalk(globals)
  xwalk <- get_2011xls_2017csv_xwalk(globals)
  
  soi_with_names <- soi_long %>%
    left_join(xwalk, by=c("lineno")) %>%
    rename(table_desc=table_desc.y)
  
  # do some checks
  # soi_with_names %>% filter(is.na(h2vname))
  # tmp <- soi_with_names %>% filter(table_desc != table_desc.x, stabbr=="US")
  
  soi_targets <- soi_with_names %>%
    mutate(year=2011) %>%
    select(year, stabbr, lineno, h2vname, AGI_STUB, table_desc, target) %>%
    arrange(lineno, h2vname, stabbr, AGI_STUB)
  
  saveRDS(soi_targets, here::here("data", "hist2_targets2011.rds"))
  return(NULL)
}

# left_join(inclink %>% select(incgrp, imin_ge, imax_lt), by="incgrp") %>%
# inclink <- get_income_ranges(2011)


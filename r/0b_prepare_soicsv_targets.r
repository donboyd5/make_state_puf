# Parse and save SOI Historical Table 2 data, which we will use as targets for the 2011 data file.
# Put variable names on the file, using the names in the corresponding SOI csv files.

# Save the result in hist2_targets2011.rds in the data directory.


get_soi2017_xwalk <- function(globals){
  xwalk <- read_excel(paste0("./data/", globals$xlfile), 
                      sheet = "2017soicsv_2011puf_xwalk",
                      range="A3:E157")
  return(xwalk)
}


prepare_soicsv_targets_long <- function(globals, year){
  # get the 53-"state" hist2 data (states, DC, US, other areas)
  year2 <- str_sub(year, 3, 4)
  hist2 <- read_csv(paste0(globals$hist2, paste0(year2, "in54cmcsv.csv")), 
                    col_types = cols(.default= col_character(),
                                     AGI_STUB=col_integer()), 
                    n_max=-1)
  # check
  # rows <- c(1:5, (nrow(hist2)-4):nrow(hist2))
  # cols <- c(1:5, (ncol(hist2) - 4):ncol(hist2))
  # hist2[rows, cols]
  
  xwalk <- get_soi2017_xwalk(globals) %>%
    select(h2vname, table_desc)
  
  hist2_long <- hist2 %>%
    mutate(year=year) %>%
    rename(stabbr=STATE) %>%
    pivot_longer(cols=-c(year, stabbr, AGI_STUB), names_to="h2vname", values_to = "target") %>%
    mutate(target=parse_number(target)) %>%
    left_join(xwalk, by="h2vname") %>%
    select(year, stabbr, h2vname, AGI_STUB, table_desc, target) %>%
    arrange(h2vname, stabbr, AGI_STUB)
  
  saveRDS(hist2_long, here::here("data", paste0("hist2_targets", year, ".rds")))
  return(NULL)
}






# create_and_save_soi2011_targets <- function(globals){
#   soi_long <- get_soi2011_targets_long(globals)
#   xwalk <- get_2011xls_2017csv_xwalk(globals)
#   
#   soi_with_names <- soi_long %>%
#     left_join(xwalk, by=c("lineno")) %>%
#     rename(table_desc=table_desc.y)
#   
#   # do some checks
#   # soi_with_names %>% filter(is.na(h2vname))
#   # tmp <- soi_with_names %>% filter(table_desc != table_desc.x, stabbr=="US")
#   
#   # saltfix <- get_saltfix(xwalk, soi_with_names)
#   
#   inclink <- get_income_ranges(2011)
#   soi_targets <- soi_with_names %>%
#     mutate(year=2011) %>%
#     select(stabbr, h2vname, incgrp, table_desc, target) %>%
#     arrange(h2vname, stabbr, incgrp)
#   
#   saveRDS(soi_targets, here::here("data", "hist2_targets2011.rds"))
#   return(NULL)
# }



# left_join(inclink %>% select(incgrp, imin_ge, imax_lt), by="incgrp") %>%



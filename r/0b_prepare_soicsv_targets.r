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
    mutate(target=parse_number(target),
           # SOI csv dollar data are in thousands, so convert to dollars
           target=ifelse(str_sub(h2vname, 1, 1)=="A", target * 1000, target)) %>%
    left_join(xwalk, by="h2vname") %>%
    select(year, stabbr, h2vname, AGI_STUB, table_desc, target) %>%
    arrange(h2vname, stabbr, AGI_STUB)
  
  saveRDS(hist2_long, here::here("data", paste0("hist2_targets", year, ".rds")))
  return(NULL)
}



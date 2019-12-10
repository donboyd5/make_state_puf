
(needed_puf_vars <- readRDS(here::here("data", "needed_puf_vars.csv")))

puf <- get_puf_xagg() # ORIGINAL PUF less the 4 aggregate records, with a wt variable
glimpse(puf)

puf_slim <- puf %>% select(RECID, needed_puf_vars)
glimpse(puf_slim)


# create a named grow_vector from grow factors
grow_factors_final <- readRDS(here::here("data", "grow_factors_final_2011to2017.rds"))
grow_vector <- grow_factors_final$grow_factor
names(grow_vector) <- grow_factors_final$pufvname
grow_vector

# apply the grow factors to the puf to get a 2017 version
puf2017 <- puf_slim
# double transpose is fast (https://stackoverflow.com/questions/3643555/multiply-rows-of-matrix-by-vector)
puf2017[, names(grow_vector)] <- t(t(puf2017[, names(grow_vector)]) * grow_vector)

puf2017[1:10, 1:10]
puf_slim[1:10, 1:10]
summary(puf2017)
ns(puf2017)
# note that we still need to adjust S006 to reflect 2017 weights in aggregate (and then we need to reweight)

saveRDS(puf2017, paste0(globals$tcdir, globals$statedir, "puf2017_unweighted.rds"))

### ------------------------------------------------------------------------ ###
### Apply chr rule ####
### ------------------------------------------------------------------------ ###

## Before: data/idx.csv
##         data/advice_history.csv
##         data/length_data.rds
## After:  method/advice.rds

library(icesTAF)
taf.libPaths()
library(cat3advice)
library(dplyr)

mkdir("method")

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###

### history of catch and advice
catch <- read.taf("data/advice_history.csv")
catch_A <- catch %>%
  select(year, 
         advice = advice_catch_stock, 
         advice_landings = advice_landings_stock,
         catch = ICES_catch_stock,
         landings = ICES_landings_stock, 
         discards = ICES_discards_stock
         ) %>%
  mutate(advice_discards = advice - advice_landings) %>%
  relocate(advice_discards, .after = advice_landings)

### WGCSE 2025 only - remove advice for 2026 -> will be updated 
catch_A <- catch_A %>% filter(year <= 2025)

### biomass index
idxB <- read.taf("data/idx.csv")

### combine catch and index
catch_idx <- full_join(catch_A, idxB, by = "year") %>%
  select(year, index, catch, landings, discards)

### ------------------------------------------------------------------------ ###
### discard survival ####
### ------------------------------------------------------------------------ ###
### set to 50% by WKBPLAICE 2024
discard_survival <- 50

### ------------------------------------------------------------------------ ###
### chr rule control parameters ####
### ------------------------------------------------------------------------ ###
### control parameters were defined at ICES WKBPLAICE in 2024
### in 2025, several data sources were revised:
### - UK landings & discards for 2021-2023
### - UK FSP index values for 2022-2023 -> also affected earlier years
### - WKBNSCS 2025 revised catch data for ple.27.7e -> affected migration catch

### the absolute values for Itrigger and HRtarget are kept from WKBPLAICE
### but because the historical catches and biomass index values changed,
### the values for x needs to be adapted:
chr_pars <- list(n1 = 2, 
                 v = 2, 
                 w = 3.7, 
                 x = 0.66/1.06682634070264015235807164572179317474365234375)


### ------------------------------------------------------------------------ ###
### reference catch ####
### ------------------------------------------------------------------------ ###
### use last catch advice (advice given in 2022 for 2023 and 2024)
### chr_A() calculates proportion of previous advice with discard_survival

# debugonce(A_calc)
A <- chr_A(catch_A, units = "tonnes", 
           basis = "advice", advice_metric = "catch", 
           discard_survival = discard_survival)
# advice(A)

### ------------------------------------------------------------------------ ###
### I - biomass index ####
### ------------------------------------------------------------------------ ###
### average of last two values
I <- chr_I(idxB, n_yrs = chr_pars$n1, units = "kg/(hr m beam)")

### ------------------------------------------------------------------------ ###
### HR - harvest rate target ####
### ------------------------------------------------------------------------ ###

### 1st: calculate harvest rate over time
hr <- HR(catch_idx, units_catch = "tonnes", units_index = "kg/(hr m beam)",
         split_discards = TRUE,
         discard_survival = discard_survival)

### 2nd: calculate harvest rate target
### include multiplier into target harvest rate (from MSE)
### -> do not include later for chr component m (set m=1)
### use definition from WKBPLAICE 2024:
### - average values 2003-2023 * 0.66/1.06682634070264015235807164572179317474365234375
HR <- F(hr, yr_ref = 2003:2023, MSE = TRUE, multiplier = chr_pars$x)

### ------------------------------------------------------------------------ ###
### b - biomass safeguard ####
### ------------------------------------------------------------------------ ###
### first application of chr rule
### use definition of Itrigger from WKBPLAICE 2024:
### - based on Iloss*w in 2007
b <- chr_b(I, idxB, units = "kg/(hr m beam)", yr_ref = 2007, w = chr_pars$w)

### ------------------------------------------------------------------------ ###
### multiplier ####
### ------------------------------------------------------------------------ ###
### set to 1 because multiplier already included in target harvest rate above

m <- chr_m(1, MSE = TRUE)

### ------------------------------------------------------------------------ ###
### discard rate ####
### ------------------------------------------------------------------------ ###
discard_rate <- catch %>%
  select(year, discards = ICES_discards_stock, landings = ICES_landings_stock,
         catch = ICES_catch_stock) %>%
  mutate(discard_rate = discards/catch * 100) %>%
  filter(year >= 2012) %>%
  summarise(discard_rate = mean(discard_rate, na.rm = TRUE)) %>% 
  as.numeric()

### ------------------------------------------------------------------------ ###
### apply chr rule - combine elements ####
### ------------------------------------------------------------------------ ###

advice <- chr(A = A, I = I, F = HR, b = b, m = m,
              #cap = "conditional", cap_upper = 20, cap_lower = -30,
              frequency = "biennial",
              discard_rate = discard_rate,
              discard_survival = discard_survival,
              units = "tonnes", advice_metric = "catch")
# advice
# advice(advice)

### ------------------------------------------------------------------------ ###
### save output ####
### ------------------------------------------------------------------------ ###
saveRDS(advice, file = "method/advice.rds")



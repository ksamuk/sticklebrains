# new model fitting & plotting approach

library("tidyverse")
library("lme4")

pond_dat_orig <- read.table("data/pond_data_clean.txt", h = T)
gard_dat <- read.table("data/gard_data_clean.txt", h = T)

pond_dat <- pond_dat_orig %>% 
  rowwise %>%
  mutate(total_size = olf_size + tele_size + optic_size + cere_size) %>%
  ungroup %>%
  gather(key = region, value = size, matches("size"))


pond_ls <- split(pond_dat, pond_dat$region)


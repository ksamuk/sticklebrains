###########################################################################
# format pond and common garden data for analysis
# KS nov 2015
###########################################################################

###########################################################################
# libraries
###########################################################################

library("dplyr")
library("ggplot2")
library("tidyr")
library("cowplot")
library("visreg")
library("lme4")

###########################################################################
# raw data
###########################################################################

# load the raw data
pond_dat <- read.delim("data/brain_data_pond.txt")
gard_dat <- read.delim("data/brain_data_common_garden.txt")

# add treatment data
# to common garden data

treat_map <- pond_dat %>% 
  select(pond, treatment) %>%
  distinct

gard_dat <- left_join(gard_dat, treat_map)

# fix sex numeric mappings + capitalization

pond_dat$sex <- ifelse(pond_dat$sex == 0, "F", "M")
gard_dat$sex <- toupper(gard_dat$sex)

###########################################################################
# formatting raw data
###########################################################################

# convert width measurements
# to ellipses 

olf_size <- with(pond_dat, olf_l/scale * olf_w/scale * pi)
tele_size <- with(pond_dat, tele_l/scale * tele_w/scale * pi)
optic_size <- with(pond_dat, optic_l/scale * optic_w/scale * pi)
cere_size <- with(pond_dat, cere_l/scale * cere_w/scale * pi)

pond_dat <- data.frame(pond = pond_dat$pond, cross = pond_dat$cross, id = pond_dat$id, treatment = pond_dat$treatment, sex = pond_dat$sex,
                       sl = pond_dat$sl, bd = pond_dat$bd, olf_size, tele_size, optic_size, cere_size)

olf_size <- with(gard_dat, olf_l/scale * olf_w/scale * pi)
tele_size <- with(gard_dat, tele_l/scale * tele_w/scale * pi)
optic_size <- with(gard_dat, optic_l/scale * optic_w/scale * pi)
cere_size <- with(gard_dat, cere_l/scale * cere_w/scale * pi)

gard_dat <- data.frame(pond = gard_dat$pond, family = gard_dat$family, id = gard_dat$id, treatment = gard_dat$treatment, sex = gard_dat$sex,
                       sl = gard_dat$sl, olf_size, tele_size, optic_size, cere_size)

# wide to long
pond_dat_l <- pond_dat %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size)

gard_dat_l <- gard_dat %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size)

# write pond data 
write.table(pond_dat, "data/pond_data_clean.txt", row.names = FALSE, quote = FALSE)
write.table(pond_dat_l, "data/pond_data_long.txt", row.names = FALSE, quote = FALSE)

# write garden data 
write.table(pond_dat, "data/gard_data_clean.txt", row.names = FALSE, quote = FALSE)
write.table(pond_dat_l, "data/gard_data_long.txt", row.names = FALSE, quote = FALSE)

###########################################################################
# size corrected datasets
###########################################################################

# pond data

# generate size-corrected residual values 
resids <- pond_dat %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  group_by(region) %>%
  do(resid = residuals(lm(.$size ~ .$sl))) %>%
  .$resid %>%
  unlist

# format output
pond_dat_resid <- pond_dat %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  mutate(resid = resids)

# write
write.table(pond_dat_resid, "data/pond_data_corrected.txt", row.names = FALSE, quote = FALSE)

# common garden data

# generate size-corrected residual values 
resids <- gard_dat %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  group_by(region) %>%
  do(resid = residuals(lm(.$size ~ .$sl, na.action = "na.exclude"))) %>%
  .$resid %>%
  unlist

# format output
gard_dat_resid <- gard_dat %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  mutate(resid = resids) 

# write
write.table(gard_dat_resid, "data/garden_data_corrected.txt", row.names = FALSE, quote = FALSE)


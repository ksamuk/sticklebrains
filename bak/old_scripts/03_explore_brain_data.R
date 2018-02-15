###########################################################################
# plots of brain size data
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
# exploring data
###########################################################################

# histogram of brain region sizes
pond_dat_l %>%
  ggplot(aes(x = sl)) +
  geom_histogram() +
  facet_wrap(~region)

# lobe size vs. body size, for each lobe x treatment (pond)
pond_dat_l %>%
  filter(sl  < 60) %>%
  ggplot(aes(x = sl, y = size, colour = factor(treatment))) +
  geom_point(size = 1, alpha = 0.5, pch = 16) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  facet_wrap(~region, scales = "free_y") +
  scale_colour_brewer(palette = "Set1")

# lobe size vs. body size, for each lobe x treatment (garden)
gard_dat_l %>%
  filter(sl  < 60) %>%
  ggplot(aes(x = sl, y = size, colour = factor(treatment))) +
  geom_point(size = 1, alpha = 0.5, pch = 16) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5)+
  facet_wrap(~region, scales = "free_y") +
  scale_colour_brewer(palette = "Set1")

###########################################################################
# linear models
###########################################################################

# function fits three linear models for each brain region
# compares the fit of each to the data using anova (likelihood ratio test)
# returns each set of models as data frame in a list

fit_model_region_pond <- function(brain_dat, region, experiment, group = "cross"){
  
  # fix region name to match data frame
  region <- paste0(region, "_size")
  
  # select columns for linear model
  brain_dat <- brain_dat %>%
    select_("pond", group, "id", "treatment", "sex", "sl", region)
  
  # genericize names
  names(brain_dat)[2] <- "group"
  names(brain_dat)[7] <- "size"
  
  # fit three models, 
  # 1: reduced, only the variables being controlled for) 
  # 2: + treatment effect
  # 3: + treatment x sex interaction
  
  mod1 <- brain_dat %>%
    lmer(size ~ sl + sex +  (1 + pond|group), data = .)
  
  mod2 <- brain_dat %>%
    lmer(size ~ sl + sex + treatment + (1 + pond|group), data = .)
  
  mod3 <- brain_dat %>%
    lmer(size ~ sl + sex * treatment + (1 + pond|group), data = .)
  
  # perform anova
  model_return <- suppressMessages(suppressWarnings(anova(mod1, mod2, mod3) %>% tidy))
  model_return <- data.frame(experiment, region, model_return)
  
  model_return
  
}

# fit models for pond data and bind into single dataframe
pond_models <- lapply(c("olf", "tele", "optic", "cere"), fit_model_region_pond, brain_dat = pond_dat, experiment = "pond")
pond_models <- rbind_all(pond_models)

# fit models for common garden data and bind into single dataframe
gard_models <-lapply(c("olf", "tele", "optic", "cere"), fit_model_region_pond, brain_dat = gard_dat, group = "family", experiment = "garden")
gard_models <- rbind_all(gard_models)

write.table(pond_models, file = "stats/pond_models.txt", quote = FALSE, row.names = FALSE)
write.table(gard_models, file = "stats/gard_models.txt", quote = FALSE, row.names = FALSE)

###########################################################################
# formal plots
###########################################################################

# generate size-corrected residual values 
resids <- pond_dat %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  group_by(region) %>%
  do(resid = residuals(lm(.$size ~ .$sl))) %>%
  .$resid %>%
  unlist


pond_dat_resid <- pond_dat %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  mutate(resid = resids)

write.table(pond_dat_resid, "data/pond_data_corrected.txt", row.names = FALSE, quote = FALSE)

pond_dat_resid %>%  
  ggplot(aes(x = factor(treatment), y = resid, color = factor(sex)))+
  geom_boxplot()+
  facet_wrap(~region, scale = "free_y")+
  scale_colour_brewer(palette = "Set1")

# plots of sex x treamtent, garden

# generate size-corrected residual values 
resids <- gard_dat %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  group_by(region) %>%
  do(resid = residuals(lm(.$size ~ .$sl, na.action = "na.exclude"))) %>%
  .$resid %>%
  unlist

gard_dat_resid <- gard_dat %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  mutate(resid = resids) 

write.table(gard_dat_resid, "data/garden_data_corrected.txt", row.names = FALSE, quote = FALSE)
  
gard_dat_resid %>%
  ggplot(aes(x = factor(treatment), y = resid, color = factor(sex)))+
  geom_boxplot()+
  facet_wrap(~region, scale = "free_y")+
  scale_colour_brewer(palette = "Set1")

# in line reporting of stats (example):
# telencephalon size was significantly smaller in the predation ponds (likelihood ratio test, X^2 1 = 7.444, p = 0.006)

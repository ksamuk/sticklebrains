###########################################################################
# fit and test statistical models 
# KS nov 2015
###########################################################################

###########################################################################
# libraries
###########################################################################

library("dplyr")
library("tidyr")
library("broom")
library("visreg")
library("lme4")
#library("lmerTest")
library("car")
library("optimx")
options(scipen = 999)

###########################################################################
# raw data
###########################################################################

# clean data
pond_dat <- read.table("data/pond_data_clean.txt", h = T)
gard_dat <- read.table("data/gard_data_clean.txt", h = T)

pond_dat <- pond_dat %>%
  mutate(total_size = olf_size + tele_size + optic_size + cere_size)

pond_tab <- pond_dat %>%
  group_by(treatment, cross, pond) %>%
  tally() %>% ungroup %>%
  arrange(cross)

gard_tab<- gard_dat %>%
  select(-family) %>%
  group_by(treatment, pond) %>%
  tally() %>% ungroup %>%
  arrange(pond) %>%
  rename(common_n = n)

# supplemental table of sample sizes
all_tab <- left_join(pond_tab, gard_tab)


pond_dat$total_size %>% length
pond_dat$cere_size %>% length

gard_dat <- gard_dat %>%
  mutate(total_size = olf_size + tele_size + optic_size + cere_size)

##########################################################################
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
  
  # only fit a pond AND family effect in the case of the pond experiment
  # otherwise just fit a pond effect (in the common garden case, we only know the individuals pond, not their actual pedigree)
  
  if(experiment == "pond"){
    
    # fit a model without interaction terms
    mod1 <- lmer(size ~ sl + sex + treatment + (1|group) + (1|pond), REML = FALSE,
                 data = brain_dat)
    
    # fit the full model
    mod2 <- lmer(size ~ sl + sex + treatment + sl:treatment + sex:treatment  + (1|group) + (1|pond),
                 data = brain_dat)
  }
  
  if(experiment == "garden"){
    
    # fit a model without interaction terms
    mod1 <- lmer(size ~ sl + sex + treatment + (1|group), REML = FALSE, 
                 data = brain_dat)
    
    # fit the full model
    mod2 <- lmer(size ~ sl + sex + treatment + sl:treatment + sex:treatment  + (1|group), 
                 data = brain_dat)
    
  }
 

  # fit the "full" model
  
  
  #mod6 <- lmer(size ~ sl + sex + treatment + sl:treatment + sex:treatment + (1 + pond|group), 
  #             data = brain_dat)
  
  #setNames(list(Anova(mod6)), region)
  
  full_df <- tidy(Anova(mod2)) %>%
    mutate(region = region) %>%
    select(region, everything())
  
  no_ixn_df <- tidy(Anova(mod1)) %>%
    mutate(region = region) %>%
    select(region, everything())
  
  names(no_ixn_df) <- c("region", "term", "stat_no_ixn", "df_no_ixn", "p.value_no_ixn")
  
  left_join(full_df, no_ixn_df)

  #setNames(list(lmerTest::step(mod6)), region)
  
  #augment(mod1) %>%
  #  group_by(group, treatment) %>%
  #  summarise(mean_size = mean(.fitted, na.rm = TRUE), sd_size = sd(.fitted, na.rm = TRUE) / sqrt(length(.resid))) %>%
  #  ggplot(aes(x = treatment, y = mean_size, group = group, color = group))+
  #  #geom_errorbar(aes(ymin = mean_size - sd_size, ymax = mean_size + sd_size), width = 0.0)+
  #  geom_line(size = 1)+
  #  geom_point(size = 2.5)
  
  
}

# fit models for pond data and bind into single dataframe
pond_models <- lapply(c("olf", "tele", "optic", "cere", "total"), fit_model_region_pond, brain_dat = pond_dat, group = "cross", experiment = "pond")
pond_models <- bind_rows(pond_models)

# add in cross ids for gard_dat 
cross_dat <- pond_dat %>%
  select(pond, cross) %>%
  distinct

gard_dat <- left_join(gard_dat, cross_dat)

# fit models for common garden data and bind into single dataframe
gard_models <-lapply(c("olf", "tele", "optic", "cere", "total"), fit_model_region_pond, brain_dat = gard_dat, group = "cross", experiment = "garden")
gard_models <- bind_rows(gard_models)

write.table(pond_models, file = "stats/pond_models.txt", quote = FALSE, row.names = FALSE)
write.table(gard_models, file = "stats/gard_models.txt", quote = FALSE, row.names = FALSE)


### BODY SIZE

pond_dat %>% head

mod1 <- pond_dat %>%
  lmer(sl ~ sex + (1 + pond|cross), data = .)

mod2 <- pond_dat %>%
  lmer(sl ~ sex + treatment + (1 + pond|cross), data = .)

mod3 <- pond_dat %>%
  lmer(sl ~ sex * treatment + (1 + pond|cross), data = .)

visreg(mod2, "treatment")

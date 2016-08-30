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

###########################################################################
# raw data
###########################################################################

# clean data
pond_dat <- read.table("data/pond_data_clean.txt", h = T)
gard_dat <- read.table("data/gard_data_clean.txt", h = T)

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
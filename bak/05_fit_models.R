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
options(scipen = 999)

###########################################################################
# raw data
###########################################################################

# clean data
pond_dat <- read.table("data/pond_data_clean.txt", h = T)
gard_dat <- read.table("data/gard_data_clean.txt", h = T)

pond_dat <- pond_dat %>%
  mutate(total_size = olf_size + tele_size + optic_size + cere_size)

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
  
  # fit three models, 
  # 1: reduced, only the variables being controlled for) 
  # 2: + treatment effect
  # 3: + treatment x sex interaction
  
  mod1 <- lmer(size ~ sl + sex +  (1 + pond|group), data = brain_dat)
  
  mod2 <- lmer(size ~ sl + sex + treatment + (1 + pond|group), data = brain_dat)
  
  mod3 <- lmer(size ~ sl + sex + treatment + sex:treatment + (1 + pond|group), data = brain_dat)
  
  mod4 <- lmer(size ~ sl + sex + treatment + sl:treatment + (1 + pond|group), data = brain_dat)
  
  # perform anova
  model_return <- suppressMessages(suppressWarnings(anova(mod1, mod2, mod3, mod4) %>% tidy))
  model_return <- data.frame(experiment, region, model_return)
  
  mod_coeff1 <- data.frame(coefficients(mod1)$group[,-c(1:2)][1,], treatmentP = NA, "sexM:treatmentP" = NA)
  mod_coeff2 <- data.frame(coefficients(mod2)$group[,-c(1:2)][1,], "sexM:treatmentP" = NA)
  mod_coeff3 <- data.frame(coefficients(mod3)$group[,-c(1:2)][1,])
  mod_coeff4 <- data.frame(coefficients(mod4)$group[,-c(1:2)][1,])

  
  coeffs <- bind_rows(list(mod_coeff1, mod_coeff2, mod_coeff3, mod_coeff4))

model_return <- cbind(model_return, coeffs)
  
list(model_return, mod1, mod2, mod3, mod4)

#augment(mod1) %>%
#  group_by(group, treatment) %>%
#  summarise(mean_size = mean(.fitted, na.rm = TRUE), sd_size = sd(.fitted, na.rm = TRUE) / sqrt(length(.resid))) %>%
#  ggplot(aes(x = treatment, y = mean_size, group = group, color = group))+
#  #geom_errorbar(aes(ymin = mean_size - sd_size, ymax = mean_size + sd_size), width = 0.0)+
#  geom_line(size = 1)+
#  geom_point(size = 2.5)
  
  
}

# fit models for pond data and bind into single dataframe
pond_models <- lapply(c("olf", "tele", "optic", "cere", "total"), fit_model_region_pond, brain_dat = pond_dat, experiment = "pond")
pond_mod_df <- bind_rows(lapply(pond_models, `[[`, 1))

# fit models for common garden data and bind into single dataframe
gard_models <-lapply(c("olf", "tele", "optic", "cere", "total"), fit_model_region_pond, brain_dat = gard_dat, group = "family", experiment = "garden")
gard_mod_df <- bind_rows(lapply(gard_models, `[[`, 1))

write.table(pond_mod_df, file = "stats/pond_models.txt", quote = FALSE, row.names = FALSE)
write.table(gard_mod_df, file = "stats/gard_models.txt", quote = FALSE, row.names = FALSE)


### BODY SIZE

pond_dat %>% head

mod1 <- pond_dat %>%
  lmer(sl ~ sex + (1 + pond|cross), data = .)

mod2 <- pond_dat %>%
  lmer(sl ~ sex + treatment + (1 + pond|cross), data = .)

mod3 <- pond_dat %>%
  lmer(sl ~ sex * treatment + (1 + pond|cross), data = .)

visreg(mod2, "treatment")

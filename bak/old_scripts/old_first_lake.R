###########################################################################
# plot pond and common garden data
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
library("broom")

###########################################################################
# raw data
###########################################################################

# clean data
pond_dat <- read.table("data/pond_data_clean.txt", h = T)
gard_dat <- read.table("data/gard_data_clean.txt", h = T)

# long data
pond_dat_l <- read.table("data/pond_data_long.txt", h = T)
gard_dat_l <- read.table("data/gard_data_long.txt", h = T)

###########################################################################
# compare first lake respone to other responses or something
###########################################################################

## classify first lake first and correct for body size (pond)

pond_dat_l <- pond_dat_l %>%
  mutate(first_lake = ifelse(pond == 9 | pond == 13, 1, 0))

pond_dat_cor_l <- pond_dat_l %>%
  group_by(region) %>%
  do(augment(lm(.$size ~ .$sl), data = .)) %>%
  ungroup %>%
  select(pond, cross, first_lake, id, treatment, sex, sl, region, size, .resid) %>%
  rename(size_resid = .resid)

## classify first lake first and correct for body size (garden)

gard_dat_l <- gard_dat_l %>%
  mutate(first_lake = ifelse(pond == 9 | pond == 13, 1, 0))

gard_dat_cor_l <- gard_dat_l %>%
  group_by(region) %>%
  do(augment(lm(.$size ~ .$sl), data = .)) %>%
  ungroup %>%
  select(pond, family, first_lake, id, treatment, sex, sl, region, size, .resid) %>%
  rename(size_resid = .resid)

# bind pond and garden data into a single dataframe

cor_dat_all <- bind_rows(data.frame(experiment = "Garden", gard_dat_cor_l), data.frame(experiment = "Pond", pond_dat_cor_l))
cor_dat_all <- cor_dat_all %>%
  mutate(first_lake = ifelse(pond == 9 | pond == 13, 1, 0))

##################################################################################
# plot the treatment response in all other families and first-lake fish separately
##################################################################################

# Figure S8

# p13 c7 id7 was a malformed individual with an unusually small brain

first_lake <- cor_dat_all  %>%
  filter(!(pond == 13 & cross == 7 & id == 79)) %>%
  # filter(!is.na(sex)) %>%
  mutate(region = factor(.$region, levels = c("olf_size", "tele_size", "optic_size", "cere_size", "total_size"), 
                         labels = c("Olfactory", "Telencephalon", "Optic", "Cerebellum", "Total"))) %>%
  mutate(sex = factor(.$sex, labels = c("Female", "Male"))) %>%
  mutate(experiment = factor(.$experiment, levels = c("Pond", "Garden"))) %>%
  mutate(treatment = factor(.$treatment, labels = c("Control", "Predation"))) %>%
  ggplot(aes(x = treatment, y = size_resid, color = factor(treatment), fill = factor(treatment), shape = factor(first_lake))) +
  #geom_point(position = position_jitter(width = 0.3), size = 0.75) +
  stat_summary(fun.data = mean_cl_normal, size = 0.5, geom = "errorbar", 
               width = 0.3, color = "black", position = position_dodge(width = 0.75)) + 
  stat_summary(fun.y = mean, geom = "point", size = 3, color = "black", position = position_dodge(width = 0.75)) +
  #geom_boxplot()+
  facet_wrap(experiment~ region, scales = "free_y", ncol = 5, nrow = 2, labeller = label_wrap_gen(multi_line=FALSE)) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_shape_manual(values = c(21, 22))+
  theme_bw() +
  xlab("Treatment") +
  ylab(expression(paste("Size-corrected brain region size", sep = ""))) +
  theme(strip.background = element_rect(fill = "white"))

ggsave("plots/first_lake.pdf", plot = first_lake,  width = 8, height = 6)


#####################################################################
# t-test to compare differences between second lake and rest of data
#####################################################################

pond_dat <- pond_dat %>%
  filter(!(pond == 13 & cross == 7 & id == 79)) %>%
  mutate(first_lake = ifelse(pond == 9 | pond == 13, 1, 0))

gard_dat <- gard_dat %>%
  mutate(first_lake = ifelse(pond == 9 | pond == 13, 1, 0))

fit_model_first_lake <- function(brain_dat, region, experiment, group = "cross"){
  
  # fix region name to match data frame
  region <- paste0(region, "_size")
  
  # select columns for linear model
  brain_dat <- brain_dat %>%
    select_("pond", group, "id", "treatment", "sex", "sl", "first_lake", region)
  
  # genericize names
  names(brain_dat)[2] <- "group"
  names(brain_dat)[8] <- "size"
  
  # fit three models, 
  # 1: reduced, only the variables being controlled for) 
  # 2: + treatment effect
  # 3: + treatment x sex interaction
  
  mod1 <- brain_dat %>%
    lmer(size ~ sl + sex + treatment + first_lake + (1 + pond|group), data = .)
  
  mod2 <- brain_dat %>%
    lmer(size ~ sl + sex + treatment + first_lake + first_lake:treatment + (1 + pond|group), data = .)
  
  # perform anova
  model_return <- suppressMessages(suppressWarnings(anova(mod1, mod2) %>% tidy))
  model_return <- data.frame(experiment, region, model_return)
  
  model_return
  
}

# fit models for pond data and bind into single dataframe
pond_models <- lapply(c("olf", "tele", "optic", "cere"), fit_model_first_lake, brain_dat = pond_dat, experiment = "pond")
pond_models <- rbind_all(pond_models) 
pond_models

gard_models <- lapply(c("olf", "tele", "optic", "cere"), fit_model_first_lake, brain_dat = gard_dat, experiment = "gard", group = "family")
gard_models <- rbind_all(gard_models) 
gard_models

cor_dat_all %>%
  lm(data = ., )
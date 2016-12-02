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

## POND

pond_dat_l <- pond_dat_l %>%
  mutate(first_lake = ifelse(pond == 9 | pond == 13, 1, 0))

## experimental fish
# correct for body size

pond_dat_cor_l <- pond_dat_l %>%
  group_by(region) %>%
  do(augment(lm(.$size ~ .$sl), data = .)) %>%
  ungroup %>%
  select(pond, cross, first_lake, id, treatment, sex, sl, region, size, .resid) %>%
  rename(size_resid = .resid)

# plot comparison between first lake and rest of crosses
# p13 c7 id7 was damaged during dissection or otherwise malformed
box_plot_pond <- pond_dat_cor_l %>%
  filter(!is.na(sex)) %>%
  mutate(region = factor(.$region, levels = c("olf_size", "tele_size", "optic_size", "cere_size"), 
                         labels = c("Olfactory", "Telencephalon", "Optic", "Cerebellum"))) %>%
  filter(!(pond == 13 & cross == 7 & id == 79)) %>%
  mutate(sex = factor(.$sex, labels = c("Female", "Male"))) %>%
  mutate(treatment = factor(.$treatment, labels = c("Control", "Predation"))) %>%
  ggplot(aes(x = treatment, y = size_resid, color = factor(treatment), fill = factor(treatment))) +
  geom_point(position = position_jitter(width = 0.3)) +
  stat_summary(fun.data = mean_cl_normal, size = 0.5, geom = "errorbar", 
               width = 0.2, color = "black", position = position_nudge(x = 0.25)) + 
  stat_summary(fun.y = mean, geom = "point", size = 3, color = "black", position = position_nudge(x = 0.25), pch = 21) +
  #geom_boxplot()+
  facet_wrap(first_lake~region, scales = "free_y", ncol = 4) +
  scale_colour_brewer(palette = "Set1", guide_legend(title = "Treatment")) +
  scale_fill_brewer(palette = "Set1", guide_legend(title = "Treatment")) +
  theme_bw() +
  xlab("Treatment") +
  ylab(expression(paste("Residual brain region size", sep = ""))) +
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "none")

## POND

gard_dat_l <- gard_dat_l %>%
  mutate(first_lake = ifelse(pond == 9 | pond == 13, 1, 0))

## experimental fish
# correct for body size

gard_dat_cor_l <- gard_dat_l %>%
  group_by(region) %>%
  do(augment(lm(.$size ~ .$sl), data = .)) %>%
  ungroup %>%
  select(pond, family, first_lake, id, treatment, sex, sl, region, size, .resid) %>%
  rename(size_resid = .resid)

# plot comparison between first lake and rest of crosses

box_plot_pond <- gard_dat_cor_l  %>%
  filter(!is.na(sex)) %>%
  mutate(region = factor(.$region, levels = c("olf_size", "tele_size", "optic_size", "cere_size"), 
                         labels = c("Olfactory", "Telencephalon", "Optic", "Cerebellum"))) %>%
  #filter(!(pond == 13 & cross == 7 & id == 79)) %>%
  mutate(sex = factor(.$sex, labels = c("Female", "Male"))) %>%
  mutate(treatment = factor(.$treatment, labels = c("Control", "Predation"))) %>%
  ggplot(aes(x = treatment, y = size_resid, color = factor(treatment), fill = factor(treatment))) +
  geom_point(position = position_jitter(width = 0.3)) +
  stat_summary(fun.data = mean_cl_normal, size = 0.5, geom = "errorbar", 
               width = 0.2, color = "black", position = position_nudge(x = 0.25)) + 
  stat_summary(fun.y = mean, geom = "point", size = 3, color = "black", position = position_nudge(x = 0.25), pch = 21) +
  #geom_boxplot()+
  facet_wrap(first_lake~region, scales = "free_y", ncol = 4) +
  scale_colour_brewer(palette = "Set1", guide_legend(title = "Treatment")) +
  scale_fill_brewer(palette = "Set1", guide_legend(title = "Treatment")) +
  theme_bw() +
  xlab("Treatment") +
  ylab(expression(paste("Residual brain region size", sep = ""))) +
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "none")


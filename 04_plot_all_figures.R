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

# size-corrected data
pond_dat_resid <- read.table("data/pond_data_corrected.txt", h = T)
gard_dat_resid <- read.table("data/garden_data_corrected.txt", h = T)

###########################################################################
# lm-style plots
###########################################################################

# lobe size vs. body size, for each lobe x treatment (pond)
# filter out strange individual with damaged cerebellum (has size > 60)
pond_fig <- pond_dat_l %>%
  filter(sl  < 60) %>%
  mutate(region = factor(.$region, levels = c("olf_size", "tele_size", "optic_size", "cere_size"), 
                         labels = c("Olfactory", "Telencephalon", "Optic", "Cerebellum"))) %>%
  mutate(sex = factor(.$sex, labels = c("Female", "Male"))) %>%
  mutate(treatment = factor(.$treatment, labels = c("Control", "Predation"))) %>%
  ggplot(aes(x = sl, y = size, colour = factor(treatment))) +
  geom_point(size = 1, alpha = 0.75, pch = 16) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  facet_grid(sex~region, scales = "free_y") +
  scale_colour_brewer(palette = "Set1", guide_legend(title = "Treatment")) +
  theme_bw() +
  xlab("Body size (mm)") +
  ylab(expression(paste("Brain region area (",mm^2,")", sep = ""))) +
  theme(strip.background = element_rect(fill = "white"))


# save as pdf
ggsave("plots/pond_regression.pdf", plot = pond_fig, width = 10, height = 8)

# lobe size vs. body size, for each lobe x treatment (common garden)
gard_fig <- gard_dat_l %>%
  filter(!is.na(sex)) %>%
  mutate(region = factor(.$region, levels = c("olf_size", "tele_size", "optic_size", "cere_size"), 
                         labels = c("Olfactory", "Telencephalon", "Optic", "Cerebellum"))) %>%
  mutate(sex = factor(.$sex, labels = c("Female", "Male"))) %>%
  mutate(treatment = factor(.$treatment, labels = c("Control", "Predation"))) %>%
  ggplot(aes(x = sl, y = size, colour = factor(treatment))) +
  geom_point(size = 1, alpha = 0.75, pch = 16) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  facet_grid(sex~region, scales = "free_y") +
  scale_colour_brewer(palette = "Set1", guide_legend(title = "Treatment")) +
  theme_bw() +
  xlab("Body size (mm)") +
  ylab(expression(paste("Brain region area (",mm^2,")", sep = ""))) +
  theme(strip.background = element_rect(fill = "white"))

ggsave("plots/gard_regression.pdf", plot = gard_fig, width = 10, height = 8)

###########################################################################
# box plots (controlled for body size)
###########################################################################

## experimental fish
# correct for body size

pond_dat_cor_l <- pond_dat_l %>%
  group_by(region) %>%
  do(augment(lm(.$size ~ .$sl), data = .)) %>%
  ungroup %>%
  select(pond, cross, id, treatment, sex, sl, region, size, .resid) %>%
  rename(size_resid = .resid)

# dat plot
box_plot_gard <- pond_dat_cor_l %>%
  filter(!is.na(sex)) %>%
  mutate(region = factor(.$region, levels = c("olf_size", "tele_size", "optic_size", "cere_size"), 
                         labels = c("Olfactory", "Telencephalon", "Optic", "Cerebellum"))) %>%
  mutate(sex = factor(.$sex, labels = c("Female", "Male"))) %>%
  mutate(treatment = factor(.$treatment, labels = c("Control", "Predation"))) %>%
  ggplot(aes(x = treatment, y = size_resid, color = factor(treatment), fill = factor(treatment))) +
  geom_point(position = position_jitter(width = 0.3)) +
  stat_summary(fun.data = mean_cl_normal, size = 0.5, geom = "errorbar", 
               width = 0.2, color = "black", position = position_nudge(x = 0.25)) + 
  stat_summary(fun.y = mean, geom = "point", size = 3, color = "black", position = position_nudge(x = 0.25), pch = 21) +
  #geom_boxplot()+
  facet_wrap(sex~region, scales = "free_y", nrow = 2, ncol = 4) +
  scale_colour_brewer(palette = "Set1", guide_legend(title = "Treatment")) +
  scale_fill_brewer(palette = "Set1", guide_legend(title = "Treatment")) +
  theme_bw() +
  xlab("Treatment") +
  ylab(expression(paste("Residual brain region size", sep = ""))) +
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "none")


## common garden
# correct for body size

gard_dat_cor_l <- gard_dat_l %>%
  group_by(region) %>%
  do(augment(lm(.$size ~ .$sl), data = .)) %>%
  ungroup %>%
  select(pond, family, id, treatment, sex, sl, region, size, .resid) %>%
  rename(size_resid = .resid)

# dat plot
box_plot_gard <- gard_dat_cor_l  %>%
  filter(!is.na(sex)) %>%
  mutate(region = factor(.$region, levels = c("olf_size", "tele_size", "optic_size", "cere_size"), 
                         labels = c("Olfactory", "Telencephalon", "Optic", "Cerebellum"))) %>%
  mutate(sex = factor(.$sex, labels = c("Female", "Male"))) %>%
  mutate(treatment = factor(.$treatment, labels = c("Control", "Predation"))) %>%
  ggplot(aes(x = treatment, y = size_resid, color = factor(treatment), fill = factor(treatment))) +
  geom_point(position = position_jitter(width = 0.3)) +
  stat_summary(fun.data = mean_cl_normal, size = 0.5, geom = "errorbar", 
               width = 0.2, color = "black", position = position_nudge(x = 0.25)) + 
  stat_summary(fun.y = mean, geom = "point", size = 3, color = "black", position = position_nudge(x = 0.25), pch = 21) +
  #geom_boxplot()+
  facet_wrap(sex~region, scales = "free_y", nrow = 2, ncol = 4) +
  scale_colour_brewer(palette = "Set1", guide_legend(title = "Treatment")) +
  scale_fill_brewer(palette = "Set1", guide_legend(title = "Treatment")) +
  theme_bw() +
  xlab("Treatment") +
  ylab(expression(paste("Residual brain region size", sep = ""))) +
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "none")




# in line reporting of stats (example):
# telencephalon size was significantly smaller in the predation ponds (likelihood ratio test, X^2 1 = 7.444, p = 0.006)
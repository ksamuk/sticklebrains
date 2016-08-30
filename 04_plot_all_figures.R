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
# raw data
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




# in line reporting of stats (example):
# telencephalon size was significantly smaller in the predation ponds (likelihood ratio test, X^2 1 = 7.444, p = 0.006)
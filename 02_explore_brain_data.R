library("dplyr")
library("ggplot2")
library("tidyr")
library("cowplot")
library("visreg")
library("lme4")

# load the raw data
brainsdat <- read.delim("data/brain_data_top.txt")

# make a new column: olf_size

olf_size <- with(brainsdat, olf_l/scale * olf_w/scale * pi)
tele_size <- with(brainsdat, tele_l/scale * tele_w/scale * pi)
optic_size <- with(brainsdat, optic_l/scale * optic_w/scale * pi)
cere_size <- with(brainsdat, cere_l/scale * cere_w/scale * pi)

sizedat <- data.frame(pond = brainsdat$pond, id = brainsdat$id, treatment = brainsdat$treatment, 
                      sl = brainsdat$sl, bd = brainsdat$bd, olf_size, tele_size, optic_size, cere_size)

# histogram of olf_size
sizedat %>%
  ggplot(aes(x = olf_size)) +
  geom_histogram()

# hist of tele size
sizedat %>%
  ggplot(aes(x = tele_size)) +
  geom_histogram()

tbl_df(brainsdat)

# 
sizedat %>%
  filter(sl  < 60) %>%
  ggplot(aes(x = sl, y = tele_size, colour = factor(pond))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "tomato")

sizedat %>%
  ggplot(aes(x = treatment, y = olf_size/sl, colour = factor(pond))) +
  geom_boxplot()


olf_plot <- sizedat %>%
  ggplot(aes(x = treatment, y = olf_size/sl)) +
  geom_boxplot()

tele_plot <-sizedat %>%
  ggplot(aes(x = treatment, y = tele_size/sl)) +
  geom_boxplot()

optic_plot <- sizedat %>%
  ggplot(aes(x = treatment, y = optic_size/sl)) +
  geom_boxplot()

cere_plot <- sizedat %>%
  ggplot(aes(x = treatment, y = cere_size/sl)) +
  geom_boxplot()

plot_grid(olf_plot, tele_plot, optic_plot, cere_plot)





# a cross/fam plot


library("tidyverse")
library("lme4")
library("visreg")
library("plotly")

all_dat <- read.table("data/merged_data_all.txt", h = T, stringsAsFactors = FALSE)

# remove misclassified individuals
all_dat <- all_dat %>%
  filter(!(id == 79 & cross == 7)) %>%
  filter(!(id == 56 & cross == 1))

# control for sex

resids <- all_dat %>%
  mutate(sl = ifelse(sl > 20, sl/10, sl)) %>%
  #gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  group_by(type, region) %>%
  do(resid = residuals(lm(.$size ~ .$sl + .$sex, na.action = na.exclude))) %>%
  .$resid %>%
  unlist

# format output
all_dat <- all_dat %>%
  mutate(resid2 = resids)

# sex * region
all_dat %>% 
  filter(type == "pond") %>%
  group_by(cross, treatment, sex, region) %>%
  summarise(mean_size = mean(resid2, na.rm = TRUE), sd_size = sd(resid2, na.rm = TRUE) / sqrt(length(resid2))) %>%
  ggplot(aes(x = treatment, y = mean_size, group = cross, color = cross))+
  geom_errorbar(aes(ymin = mean_size - sd_size, ymax = mean_size + sd_size), width = 0.1, position = position_dodge(width = 0.2))+
  geom_line(size = 1, position = position_dodge(width = 0.2))+
  geom_point(size = 2.5, position = position_dodge(width = 0.2))+
  facet_grid(sex~region)

all_dat %>% 
  filter(type == "pond") %>%
  group_by(cross, treatment, region) %>%
  summarise(mean_size = mean(resid2, na.rm = TRUE), sd_size = sd(resid2, na.rm = TRUE) / sqrt(length(resid2))) %>%
  ggplot(aes(x = treatment, y = mean_size, group = cross, color = cross))+
  #geom_errorbar(aes(ymin = mean_size - sd_size, ymax = mean_size + sd_size), width = 0.0)+
  geom_line(size = 1)+
  geom_point(size = 2.5)+
  facet_wrap(~region, scales = "free")


# just region
all_dat %>% 
  filter(type == "pond") %>%
  group_by(cross, treatment, region) %>%
  summarise(mean_size = mean(resid2, na.rm = TRUE), sd_size = sd(resid2, na.rm = TRUE)) %>%
  ggplot(aes(x = treatment, y = mean_size, group = cross, color = cross))+
  #geom_errorbar(aes(ymin = mean_size - sd_size, ymax = mean_size+ sd_size), width = 0.2, position = position_dodge(width = 0.2))+
  geom_point()+
  geom_line()+
  facet_grid(~region)


p <- all_dat %>% 
  filter(!is.na(sex)) %>%
  filter(type == "garden") %>% 
  filter(!(pond == 7 & cross == 3)) %>%
  group_by(pond, treatment, sex, region) %>%
  summarise(mean_size = mean(resid, na.rm = TRUE), sd_size = sd(resid, na.rm = TRUE)) %>%
  ggplot(aes(x = treatment, y = mean_size, color = pond))+
  geom_point()+
  #geom_line()+
  facet_grid(sex~region)

ggplotly(p)

# just body size

all_dat %>% 
  filter(type != "wild") %>%
  filter(!is.na(sex)) %>%
  filter(!(cross == 3 & type == "garden")) %>%
  group_by(type, cross, treatment, sex, region) %>% 
  summarise(mean_sl = mean(sl, na.rm = TRUE), sd_size = sd(sl, na.rm = TRUE) / sqrt(length(sl))) %>%
  ggplot(aes(x = treatment, y = mean_sl, group = cross, color = cross))+
  geom_errorbar(aes(ymin = mean_sl - sd_size, ymax = mean_sl + sd_size), width = 0)+
  geom_line(size = 1)+
  geom_point(size = 2.5)+
  facet_grid(sex~type)+
  ylab("Standard Length (mm)")+
  xlab("Treatment")+
  scale_color_brewer(type = "qual", palette = "Set1")
  
all_dat %>% 
  filter(type != "wild") %>%
  filter(!is.na(sex)) %>%
  group_by(type, cross, treatment) 

all_dat %>% 
  filter(type == "pond") %>%
  group_by(cross, pond) %>%
  summarise(mean_sl = mean(sl, na.rm = TRUE), sd_size = sd(sl, na.rm = TRUE) / sqrt(length(sl))) %>%
  ungroup %>%
  ggplot(aes(x = treatment, y = mean_sl, group = cross, color = cross, label = pond))+
  geom_errorbar(aes(ymin = mean_sl - sd_size, ymax = mean_sl + sd_size), width = 0)+
  geom_line(size = 1)+
  geom_label(size = 2.5)

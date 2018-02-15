# wild fish: figure out wtf is happening?

library("tidyverse")
library("lme4")
library("visreg")
library("plotly")
#devtools::install_github("remkoduursma/bootpredictlme4")
#library(bootpredictlme4)

all_dat <- read.table("data/merged_data_all.txt", h = T, stringsAsFactors = FALSE)

# remove misclassified individuals
all_dat <- all_dat %>%
  filter(!(id == 79 & cross == 7)) %>%
  filter(!(id == 56 & cross == 1))

resids <- all_dat %>%
  mutate(sl = ifelse(sl > 20, sl/10, sl)) %>%
  #gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  group_by(region) %>%
  do(resid = residuals(lm(.$size ~ .$sl, na.action = na.exclude))) %>%
  .$resid %>%
  unlist

# format output
all_dat <- all_dat %>%
  mutate(resid2 = resids)

#lmer(size ~ sl + sex * treatment + (1 + pond|group), data = all_dat_optic)

all_dat %>%
  filter(size > 1) %>%
  #filter(resid2 > -50) %>%
  mutate(sl = ifelse(sl > 20, sl/10, sl)) %>%
  #filter(type == "wild") %>%
  mutate(treatment = ifelse(cross %in% c("lim", "ben"), cross, treatment)) %>%
  mutate(treatment = ifelse(type %in% c("garden"), paste0(treatment, "_", type), treatment)) %>%
  mutate(lab_id = paste0(cross, "_", id)) %>%
  ggplot(aes(x = sl, y = size, color = treatment, label = lab_id))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~region, scales = "free_y")

ggplotly(lm_plot)


all_dat %>%
  mutate(sl = ifelse(sl > 20, sl/10, sl)) %>%
  filter(size > 1) %>%
  filter(!is.na(sex)) %>%
  mutate(treatment = ifelse(cross %in% c("lim", "ben"), paste0("wild_", cross), treatment)) %>%
  #mutate(treatment2 = ifelse(type %in% c("garden"), paste0(type, "_", treatment), treatment2)) %>%
  mutate(type = ifelse(cross %in% c("lim", "ben"), "wild", type)) %>%
  ggplot(aes(y = sl, x = type, color = treatment)) +
  geom_boxplot()+
  facet_grid(~region, scales = "free")+
  theme(axis.text.x = element_text(angle = 90))


opt_dat <- all_dat %>%
  filter(region == "optic_size") %>%
  filter(type == "pond") %>%
  filter(size > 1) %>%
  filter(!is.na(sex)) %>%
  mutate(sl_scale = scale(sl) %>% as.numeric) %>%
  mutate(size_scale = scale(size) %>% as.numeric)
  
  
opt_mod1 <- lmer(size_scale ~ sl_scale + sex + (1 | cross/pond), data = opt_dat)
opt_mod2 <- lmer(size_scale ~ sl_scale + sex + treatment + (1 | cross/pond), data = opt_dat)
opt_mod3 <- lmer(size_scale ~ sl_scale + sex * treatment + (1 | cross/pond), data = opt_dat)

anova(opt_mod1, opt_mod2, opt_mod3)

visreg(opt_mod2)


v <- visreg(opt_mod2, 'treatment', type = "contrast", re.form=(~1|cross/pond), plot=FALSE)
plot(v, overlay=TRUE)

visreg2d(opt_mod, "sl_scale", "treatment")



###############
# PCA TRY
###############

row.names(all_dat) <- NULL


wide_df <- 1025, %>%
  select(pond, type, cross, id, treatment, sex, region, resid2) %>%
  distinct %>%
  spread(key = region, value = resid2)

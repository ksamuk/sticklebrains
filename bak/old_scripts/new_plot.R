# new model fitting & plotting approach

library("tidyverse")
library("lme4")
library("broom")
library("plotly")
library("lmerTest")

pond_dat_orig <- read.table("data/pond_data_long.txt", h = T)
pond_dat_orig <- read.table("data/pond_data_clean.txt", h = T)
pond_dat <- pond_dat_orig
gard_dat <- read.table("data/gard_data_clean.txt", h = T)

# long data
pond_dat_l <- read.table("data/pond_data_long.txt", h = T)
gard_dat_l <- read.table("data/gard_data_long.txt", h = T)

##########################
# COMPUTE LM RESIDUALS
##########################

pond_dat <- pond_dat_orig %>%
  filter(sl  < 60) %>%
  mutate(type = paste0(treatment, "_", cross)) %>%
  filter(!(type == "P_7" & id == "79")) %>% # these individuals are extreme 'allometric outliers'
  filter(!(type == "NP_1" & id == "56")) %>% # including or excluding them doesn't affect results, but makes plots more readable
  mutate(tmp = paste0(treatment, "_", cross)) %>%
  rowwise %>%
  mutate(total_size = olf_size + tele_size + optic_size + cere_size) %>%
  ungroup %>%
  gather(key = region, value = size, matches("size")) %>%
  select(-tmp)


pond_ls <- split(pond_dat, pond_dat$region)

# function for computing residuals (thanks, augment!)
fit_models <- function(x){
  
  #mod1 <- lm(size ~ sl + sex, na.action = "na.exclude", data = x)
  mod1 <- lmer(size ~ sl + sex + (1|cross), na.action = "na.exclude", data = x)
  data.frame(augment(mod1, x), region = x$region[1])
  
}

# apply augment and bind results
pond_df <- lapply(pond_ls, fit_models)
pond_df <- bind_rows(pond_df) %>%
  distinct

# ordering for cross and region
pond_df <- pond_df %>%
  mutate(cross = factor(cross, levels = c(2, 6, 7, 1, 4), ordered=TRUE)) %>%
  mutate(region = factor(region, levels=c("olf_size", "tele_size", "optic_size", "cere_size", "total_size"), ordered=TRUE))

pond_df$region <- recode_factor(as.factor(unlist(pond_df$region)), 
                                "olf_size" = "Olfactory",
                                "tele_size" = "Telencephalon",
                                "optic_size" = "Optic",
                                "cere_size" = "Cerebellum",
                                "total_size" = "Total")

###########################################################################
# correct for size and reclassify variables
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

###########################################################################
# FIGURE 2: DOT PLOT
###########################################################################

dot_plot <- pond_df %>%
  ggplot(aes(x = treatment, y = .resid, color = factor(treatment), fill = factor(treatment))) +
  geom_point(position = position_jitter(width = 0.3), size = 0.5, alpha = 0.25) +
  stat_summary(fun.data = mean_cl_normal, size = 0.25, geom = "errorbar", 
               width = 0.3, color = "black") + 
  stat_summary(fun.y = mean, geom = "point", size = 1.5, color = "black", pch = 21) +
  #geom_boxplot()+
  facet_wrap(~region, scales = "free_y", nrow = 1) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  xlab("Treatment") +
  ylab(expression(paste("Size-corrected brain region size", sep = ""))) +
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "none", 
        panel.grid = element_blank())

ggsave(dot_plot, file = "new_plots/fig2_dot_plot.pdf", width = 7, height = 3, device = "pdf")

###########################################################################
# FIGURE 2: BAR PLOT
###########################################################################

bar_plot <- pond_df %>% 
  mutate(cross = as.factor(cross)) %>%
  group_by(cross, treatment, region) %>%
  summarise(mean_size = mean(.resid, na.rm = TRUE), sd_size = sd(.resid, na.rm = TRUE) / sqrt(length(.resid))) %>%
  ungroup %>%
  group_by(cross, region) %>%
  mutate(size_diff = (mean_size[2] + 20) - (mean_size[1] + 20)) %>%
  ungroup %>%
  ggplot(aes(x = cross, y = size_diff, group = region))+
  geom_bar(stat = "identity")+
  geom_hline(yintercept = 0)+
  facet_wrap(~region, scales = "free_y", nrow = 1) +
  xlab("Cross") +
  ylab("Difference in Residual Brain Size (Control - Treatment)")  +
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"))

ggsave(bar_plot, file = "new_plots/fig2_per_cross_bar.pdf", width = 7, height = 2, device = "pdf")
###########################################################################
# FIGURE 3: ALLOMETRY + BODY SIZE PLOT
###########################################################################

pond_dat$region <- recode_factor(as.factor(unlist(pond_dat$region)), 
                                 "olf_size" = "Olfactory",
                                 "tele_size" = "Telencephalon",
                                 "optic_size" = "Optic",
                                 "cere_size" = "Cerebellum",
                                 "total_size" = "Total")

size_plot <- pond_dat %>%
  filter(size > 1) %>%
  mutate(type = paste0(treatment, "_", cross)) %>%
  filter(!(type == "P_7" & id == "79")) %>%
  filter(!(type == "NP_1" & id == "56")) %>%
  ggplot(aes(x = log10((sl + 1)^2), y = log10(size + 1), color = treatment, label = id)) +
  geom_point(alpha = 0.25, stroke = 0)+
  #geom_smooth(method = "lm", se = FALSE, alpha= 0.1)+
  stat_smooth(geom = 'line', alpha = 1, se = FALSE, method = "lm", size = 1)+
  #geom_smooth(method = "lm", se = FALSE, aes(group = treatment), size = 1.5)+
  facet_wrap(~region, scale = "free")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none")+
  scale_colour_brewer(palette = "Set1", guide_legend(title = "Treatment"))+
  xlab("log(Body Size + 1)^2") + 
  ylab("log(Brain Region Size + 1)")


ggsave(size_plot, file = "new_plots/fig3_size_plot.pdf", width = 6, height = 5, device = "pdf")

# body size only 

bod_size_plot <- pond_dat %>%
  mutate(type = paste0(treatment, "_", cross)) %>%
  filter(!(type == "P_7" & id == "79")) %>%
  filter(!(type == "NP_1" & id == "56")) %>%
  ggplot(aes(x = log(sl), fill = treatment, label = id)) +
  geom_histogram(bins = 25)+
  #geom_smooth(method = "lm", se = FALSE, alpha= 0.1)+
  #geom_smooth(method = "lm", se = FALSE, aes(group = treatment), size = 1.5)+
  facet_wrap(~region, scale = "free")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none")+
  scale_fill_brewer(palette = "Set1", guide_legend(title = "Treatment"))+
  xlab("log(Body Size)") + 
  ylab("log(Brain Region Size)")

ggsave(bod_size_plot, file = "new_plots/fig3_bod_size_plot.pdf", width = 6, height = 5, device = "pdf")

###########################################################################
# FIGURE 4: COMMON GARDEN + POND PLOT
###########################################################################

cor_dat_all <- bind_rows(data.frame(experiment = "Garden", gard_dat_cor_l), data.frame(experiment = "Pond", pond_dat_cor_l))

cor_dat_all$region <- recode_factor(as.factor(unlist(cor_dat_all$region)), 
                                    "olf_size" = "Olfactory",
                                    "tele_size" = "Telencephalon",
                                    "optic_size" = "Optic",
                                    "cere_size" = "Cerebellum",
                                    "total_size" = "Total")

mean_ci_only <- cor_dat_all  %>%
  # filter(!is.na(sex)) %>%
  mutate(sex = factor(.$sex, labels = c("Female", "Male"))) %>%
  mutate(experiment = factor(.$experiment, levels = c("Pond", "Garden"))) %>%
  mutate(treatment = factor(.$treatment, labels = c("Control", "Predation"))) %>%
  ggplot(aes(x = treatment, y = size_resid, color = factor(treatment), fill = factor(treatment), shape = factor(experiment))) +
  #geom_point(position = position_jitter(width = 0.3), size = 0.75) +
  stat_summary(fun.data = mean_cl_normal, size = 0.25, geom = "errorbar", 
               width = 0.3, color = "black", position = position_dodge(width = 0.75)) + 
  stat_summary(fun.y = mean, geom = "point", size = 2.5, color = "black", position = position_dodge(width = 0.75)) +
  #geom_boxplot()+
  facet_wrap(~ region, scales = "free_y", ncol = 5) +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_shape_manual(values = c(21, 22))+
  theme_bw() +
  xlab("Treatment") +
  ylab(expression(paste("Size-corrected brain region size", sep = ""))) +
  theme(strip.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave("new_plots/fig4_mean_ci_only.pdf", plot = mean_ci_only, width = 7, height = 3)


###########################################################################
# NEW STATS
###########################################################################

# pond 

pond_dat <- pond_dat_orig %>% 
  #filter(sl  < 60) %>%
  mutate(type = paste0(treatment, "_", cross)) %>%
  filter(!(type == "P_7" & id == "79")) %>%
  filter(!(type == "NP_1" & id == "56")) %>%
  mutate(tmp = paste0(treatment, "_", cross)) %>%
  rowwise %>%
  mutate(total_size = olf_size + tele_size + optic_size + cere_size) %>%
  ungroup %>%
  gather(key = region, value = size, matches("size")) %>%
  select(-tmp)

pond_ls <- split(pond_dat, pond_dat$region)

fit_full_models <- function(x){
  
  mod1 <- lmer(size ~ sl + sex + (1|cross/pond), data = x)
  mod2 <- lmer(size ~ sl + sex + treatment + (1|cross/pond), data = x)
  mod3 <- lmer(size ~ sl + sex * treatment + (1|cross/pond), data = x)
  #mod4 <- lm(size ~ sl + sex * treatment + cross + pond, data = x)
  
  list(tidy(stats::anova(mod1, mod2, mod3, test="LRT")))
  
}

pond_test_df <- lapply(pond_ls, fit_full_models)

pond_df <- bind_rows(pond_df)

# garden

pond_sub <- pond_dat %>%
  select(pond, cross) %>%
  distinct

gard_dat_l <- gard_dat_l  %>%
  left_join(pond_sub)


gard_ls <- split(gard_dat_l, gard_dat_l$region)

# note pond is not included in models -- fish were reared in common environment
fit_full_models <- function(x){
  
  mod1 <- lmer(size ~ sl + sex + (1|cross), data = x)
  mod2 <- lmer(size ~ sl + sex + treatment + (1|cross), data = x)
  mod3 <- lmer(size ~ sl + sex * treatment + (1|cross), data = x)
  #mod4 <- lm(size ~ sl + sex * treatment + pond, data = x)
  
  list(tidy(anova(mod1, mod2, mod3)))
  
  #list(tidy(anova(mod4)))
}

gard_test_df <- lapply(gard_ls, fit_full_models)

###########################################################################
# TEST OF ALLOMETRY + BODY SIZE
###########################################################################

#install.packages("smatr")

library(smatr)
compare_slopes <- function(x){
  
  reg_name <- x$region %>% unique
  
  x <- x %>%
    filter(!is.na(size), !is.na(sl)) %>%
    filter(size > 1)
  
  males <- x %>%
    filter(sex == "M")
  
  females <- x %>%
    filter(sex != "M")
  
  sma_obj <- sma(log10(size+ 1) ~ log10((sl + 1)^2) + treatment, slope.test = 1, data = males)
  test <-  sma_obj$slopetest
  df <- (sma_obj$data %>% nrow) -2
  
  
  row_m <- data.frame(sex = "M", reg_name, f_test = test[[1]]$F, p_test = test[[2]]$p, df, 
                      np_slope = test[[1]]$b, np_slope_lower = test[[1]]$ci[1,1], np_slope_upper = test[[1]]$ci[1,2], 
                      p_slope = test[[2]]$b, p_slope_lower = test[[2]]$ci[1,1], p_slope_upper = test[[2]]$ci[1,2]) 
  
  sma_obj <- sma(log10(size+ 1) ~ log10((sl + 1)^2) + treatment, slope.test = 1, data = females)
  test <- sma_obj$slopetest
  df <- (sma_obj$data %>% nrow) -2
  
  row_f <- data.frame(sex = "F", reg_name, f_test = test[[2]]$F, p_test = test[[2]]$p, df, 
                      np_slope = test[[1]]$b, np_slope_lower = test[[1]]$ci[1,1], np_slope_upper = test[[1]]$ci[1,2], 
                      p_slope = test[[2]]$b, p_slope_lower = test[[2]]$ci[1,1], p_slope_upper = test[[2]]$ci[1,2])
  bind_rows(list(row_m, row_f))
}


pond_allom_df <- lapply(pond_ls, compare_slopes)
bind_rows(pond_allom_df)

# body size

body_dat <- pond_dat %>%
  select(pond, cross, treatment, sex, sl) %>%
  distinct

mod1 <- lmer(sl ~ sex + (1|cross/pond), data = body_dat)
mod2 <- lmer(sl ~ sex + treatment + (1|cross/pond), data = body_dat)

anova(mod1, mod2)

###########################################################################
# CORRELATIONS AMONG BRAIN REGIONS
###########################################################################

#install.packages("GGally")
library("GGally")

pond_dat_wide <- pond_dat_orig %>%
  filter(cere_size > 1) %>%
  mutate(type = paste0(treatment, "_", cross)) %>%
  filter(!(type == "P_7" & id == "79")) %>% # these individuals are extreme 'allometric outliers'
  filter(!(type == "NP_1" & id == "56")) %>% # including or excluding them doesn't affect results, but makes plots more readable
  mutate(tmp = paste0(treatment, "_", cross)) %>%
  rowwise %>%
  mutate(total_size = olf_size + tele_size + optic_size + cere_size) %>%
  ungroup

cor_plot <- pond_dat_wide %>%
  ggpairs(columns = c(8:11, 14), aes(color = treatment), 
          diag = list(continuous = "blank"))+
  theme_bw()
 
ggsave("new_plots/cor_plot.pdf", plot = cor_plot, width = 7, height = 7, useDingbats=FALSE)
